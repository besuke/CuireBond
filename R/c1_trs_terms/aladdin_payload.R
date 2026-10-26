# c1：Aladdin へ送るスワップ（TRS）の要求 JSON を、読み取った条件から作る（下書き。送信はしない）。
#
# 形式：Aladdin のスワップ登録の要求本文（counterparty、swapInfo.payLeg／recLeg、*PaymentReset、*Stub、
#       pay/recLegCustomSchedules、recLegIncomeFee）。利用者から受け取った見本に合わせる。
# 送信先（URL・メソッド）と認証は未提示のため、送信処理は作っていない（推測で作らない）。
#
# 値の入れ方
#   - 原文から読み取った値をそのまま使える項目（日付、通貨、ルックバック日数）は入れる
#   - Aladdin のコード（相手方・参照金利・参照資産・カレンダー）、選択肢の値（accrualBase などの列挙値）、
#     単位（スプレッド）、どちらが自社か（payLeg／recLeg の向き）が要る項目は、対応表（aladdin_map）にあるときだけ入れる
#   - 入れられなかった項目は JSON に含めず、「未確定」として理由つきで返す（見本の "string" や *_UNSPECIFIED は入れない）
#
# 対応表（aladdin_map）：config/aladdin_map_example.json の形（JSON）。埋めたものは tmp/aladdin_map.json など Git 管理対象外に置き、
# 環境変数 CURIE_ALADDIN_MAP（tmp/.Renviron）でその場所を指定する

# 対応表を読む（なければ空。すべての Aladdin のコード・列挙値・向きが未確定になる）
read_aladdin_map_c1 <- function(path = Sys.getenv("CURIE_ALADDIN_MAP", "")) {
  if (!nzchar(path) || !file.exists(path)) return(list())
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

# 条件の表 → 値を引く関数（記載あり・確認済みの値だけ）
.condition_getter_c1 <- function(conditions) {
  ok <- dplyr::filter(conditions, .data$value_status == "stated", .data$review_status == "confirmed", nzchar(.data$value))
  function(id) dplyr::first(ok$value[ok$item_id == id], default = NA_character_)
}

build_aladdin_trs_payload_c1 <- function(conditions, aladdin_map = read_aladdin_map_c1()) {
  g <- .condition_getter_c1(conditions)
  payload <- list()
  unresolved <- list()
  set <- function(path, value) payload <<- purrr::assign_in(payload, strsplit(path, ".", fixed = TRUE)[[1]], value)
  miss <- function(path, item, value, reason) {
    unresolved[[length(unresolved) + 1L]] <<- tibble::tibble(項目 = path, 条件 = item, 値 = dplyr::coalesce(value, ""), 理由 = reason)
  }
  # 対応表から引く（なければ NA）
  lookup <- function(table, key) {
    if (is.na(key)) return(NA_character_)
    v <- aladdin_map[[table]][[toupper(stringr::str_squish(key))]]
    if (is.null(v) || !nzchar(as.character(v))) NA_character_ else as.character(v)
  }
  put <- function(path, item, table, key = g(item), reason = NULL) {
    v <- lookup(table, key)
    if (is.na(key)) return(miss(path, item, NA_character_, "原文に記載がない、または未確定"))
    if (is.na(v)) return(miss(path, item, key, reason %||% paste0("Aladdin の値への対応表（", table, "）にない")))
    set(path, v)
  }

  # どちらのレッグを受け取るか（自社＝トータル・リターンの受取者なら、リターン・レッグが recLeg）
  ours <- toupper(unlist(aladdin_map$our_party_names %||% list()))
  receiver <- toupper(g("trade.total_return_receiver") %|% ""); payer <- toupper(g("trade.total_return_payer") %|% "")
  side <- if (!length(ours)) NA_character_ else if (receiver %in% ours) "receive_return" else if (payer %in% ours) "pay_return" else NA_character_
  if (is.na(side)) {
    miss("swapInfo.payLeg／recLeg", "trade.total_return_receiver", g("trade.total_return_receiver"),
         if (!length(ours)) "自社（ファンド）の名前が対応表（our_party_names）にないため、payLeg と recLeg の向きを決められない"
         else "トータル・リターンの受取者・支払者のどちらも自社の名前と一致しない")
    return(list(payload = payload, unresolved = purrr::list_rbind(unresolved), side = side))
  }
  return_leg <- if (side == "receive_return") "recLeg" else "payLeg"
  funding_leg <- if (side == "receive_return") "payLeg" else "recLeg"
  counterparty_name <- if (side == "receive_return") g("trade.total_return_payer") else g("trade.total_return_receiver")

  put("counterparty", "trade.total_return_payer／receiver", "counterparty_codes", counterparty_name)

  # 両レッグ共通：通貨・開始日・終了日（原文の値をそのまま使う）
  purrr::walk(c(return_leg, funding_leg), function(leg) {
    ccy <- g("trade.notional_currency")
    if (is.na(ccy)) miss(paste0("swapInfo.", leg, ".currencyCode"), "trade.notional_currency", NA, "原文に記載がない、または未確定") else set(paste0("swapInfo.", leg, ".currencyCode"), ccy)
    purrr::iwalk(c(startDate = "trade.effective_date", endDate = "trade.termination_date"), function(item, field) {
      v <- g(item); p <- paste0("swapInfo.", leg, ".", field)
      if (is.na(v)) miss(p, item, NA, "原文に記載がない、または未確定") else set(p, v)
    })
    put(paste0("swapInfo.", leg, ".type"), "（レッグの種類）", "swap_type", if (leg == return_leg) "RETURN_LEG" else "FUNDING_LEG",
        "SWAP_TYPE の値が未提示")
  })

  # リターン・レッグ：参照資産
  put(paste0("swapInfo.", return_leg, ".underlyingAssetId"), "return.underlyer_id／name", "asset_ids",
      dplyr::coalesce(g("return.underlyer_id"), g("return.underlyer_name")))

  # 資金調達レッグ：参照金利・スプレッド・日数計算
  idx <- g("funding.floating_rate_index"); tenor <- g("funding.index_tenor")
  put(paste0("swapInfo.", funding_leg, ".resetIndex"), "funding.floating_rate_index＋index_tenor", "index_codes",
      if (is.na(idx)) NA_character_ else paste0(idx, if (!is.na(tenor)) paste0(" ", tenor) else ""))
  spread <- g("funding.spread_bp"); unit <- aladdin_map$spread_unit %||% NA_character_
  if (is.na(spread)) miss(paste0("swapInfo.", funding_leg, ".spread"), "funding.spread_bp", NA, "原文に記載がない、または未確定")
  else if (identical(unit, "bp")) set(paste0("swapInfo.", funding_leg, ".spread"), as.numeric(spread))
  else if (identical(unit, "decimal")) set(paste0("swapInfo.", funding_leg, ".spread"), as.numeric(spread) / 10000)
  else if (identical(unit, "percent")) set(paste0("swapInfo.", funding_leg, ".spread"), as.numeric(spread) / 100)
  else miss(paste0("swapInfo.", funding_leg, ".spread"), "funding.spread_bp", spread, "spread の単位（bp・decimal・percent）が未提示（対応表の spread_unit）")
  put(paste0("swapInfo.", funding_leg, ".accrualBase"), "funding.day_count", "accrual_base")

  # 資金調達レッグの支払・リセット
  pr <- paste0("swapInfo.", funding_leg, "PaymentReset")
  put(paste0(pr, ".paymentFrequencyCalendar.frequencyType"), "funding.payment_frequency", "frequency_type")
  put(paste0(pr, ".paymentFrequencyCalendar.adjustMethod"), "funding.payment_business_day_convention", "adjust_method")
  cal <- g("funding.payment_calendar")
  codes <- if (is.na(cal)) character() else purrr::map_chr(.business_centers_c1(cal), \(c) lookup("calendar_codes", c))
  if (is.na(cal)) miss(paste0(pr, ".paymentFrequencyCalendar.calendarCodes"), "funding.payment_calendar", NA, "原文に記載がない、または未確定")
  else if (!length(codes) || anyNA(codes)) miss(paste0(pr, ".paymentFrequencyCalendar.calendarCodes"), "funding.payment_calendar", cal, "Aladdin のカレンダーコードへの対応表（calendar_codes）にない")
  else set(paste0(pr, ".paymentFrequencyCalendar.calendarCodes"), as.list(codes))
  put(paste0(pr, ".resetFrequencyCalendar.frequencyType"), "funding.reset_frequency", "frequency_type")
  fcal <- g("funding.fixing_calendar")
  fcodes <- if (is.na(fcal)) character() else purrr::map_chr(.business_centers_c1(fcal), \(c) lookup("calendar_codes", c))
  if (is.na(fcal)) miss(paste0(pr, ".resetFrequencyCalendar.calendarCodes"), "funding.fixing_calendar", NA, "原文に記載がない、または未確定")
  else if (!length(fcodes) || anyNA(fcodes)) miss(paste0(pr, ".resetFrequencyCalendar.calendarCodes"), "funding.fixing_calendar", fcal, "Aladdin のカレンダーコードへの対応表（calendar_codes）にない")
  else set(paste0(pr, ".resetFrequencyCalendar.calendarCodes"), as.list(fcodes))
  lb <- g("funding.lookback_days")
  if (is.na(lb)) miss(paste0(pr, ".lookback"), "funding.lookback_days", NA, "原文に記載がない、または未確定") else set(paste0(pr, ".lookback"), as.integer(lb))
  put(paste0(pr, ".compounding"), "funding.compounding_method", "compounding_flag", reason = "複利・平均の方法を true／false にする対応が未提示")
  if (!is.na(g("funding.fixing_offset_days"))) {
    miss(paste0(pr, ".resetDelayType／resetDelayValue"), "funding.fixing_offset_days", g("funding.fixing_offset_days"),
         "DELAY_TYPE の値と、日数の符号（前・後）の決まりが未提示")
  }
  # 仕様の見本にない条件（送る項目がない）
  if (!is.na(g("trade.notional_amount"))) {
    miss("（想定元本）", "trade.notional_amount", g("trade.notional_amount"), "受け取った要求本文の見本に想定元本の項目がない")
  }
  list(payload = payload, unresolved = purrr::list_rbind(unresolved), side = side)
}

# 案件の条件から要求 JSON の下書きを作り、result/ に保存する（送信はしない）
write_aladdin_draft_c1 <- function(case, aladdin_map = read_aladdin_map_c1()) {
  cond <- case$validation$conditions
  if (is.null(cond)) return(NULL)
  built <- build_aladdin_trs_payload_c1(cond, aladdin_map)
  dir <- file.path(case$case_dir, "result")
  path <- file.path(dir, "aladdin_request_DRAFT.json")
  jsonlite::write_json(built$payload, path, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  jsonlite::write_json(list(note = "Aladdin へは送信していない（送信先・認証が未提示）", sha256 = pdf_sha256_c1(path),
                            side = built$side, unresolved = built$unresolved),
                       file.path(dir, "aladdin_unresolved.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  list(path = path, unresolved = built$unresolved, payload = built$payload)
}
