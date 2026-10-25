# c1：抽出・照合したTRS条件からFpMLを作る（xml2）。
#
# 形式：FpML 5.x の確認書ビュー（confirmation）の returnSwap を目指した構造
#   returnLeg   … 参照資産（underlyer）、最終評価、想定元本、リターンの種類
#   interestLeg … 資金調達の参照金利（floatingRateIndex）、スプレッド、リセット日・フィキシング日、日数計算方式
# 公式XSDによる検証は手元で行っていない（要素の順序・必須要素は要検証）。
#
# mode = "final"：必須項目がすべて「確定」で、規則違反がなく、コード変換がすべて成功した文書だけ作る
# mode = "draft"：照合前の抽出値のうち記載のある項目だけで作る。空欄・選択肢・記載なしの項目は要素を出さず、
#                 不足項目をコメントに列挙する。正式なFpMLではない
# 同じ入力からは毎回同じXMLになる（実行日時などの可変情報をXMLに入れない）。

.fpml_ns_c1 <- "http://www.fpml.org/FpML-5/confirmation"

# 原文の表記 → FpMLのコード（対応がなければ NA。推測で補わない）
.fpml_code_maps_c1 <- list(
  day_count = c("ACTUAL/360" = "ACT/360", "ACT/360" = "ACT/360", "ACTUAL/365 (FIXED)" = "ACT/365.FIXED",
                "ACTUAL/365F" = "ACT/365.FIXED", "ACT/365.FIXED" = "ACT/365.FIXED", "ACT/365F" = "ACT/365.FIXED",
                "30/360" = "30/360", "ACTUAL/ACTUAL (ISDA)" = "ACT/ACT.ISDA"),
  business_day_convention = c("MODIFIED FOLLOWING" = "MODFOLLOWING", "MODFOLLOWING" = "MODFOLLOWING",
                              "FOLLOWING" = "FOLLOWING", "PRECEDING" = "PRECEDING", "NONE" = "NONE"),
  business_center = c("NEW YORK" = "USNY", "LONDON" = "GBLO", "TOKYO" = "JPTO", "SINGAPORE" = "SGSI",
                      "JAKARTA" = "IDJA", "TARGET" = "EUTA", "USNY" = "USNY", "GBLO" = "GBLO", "JPTO" = "JPTO",
                      "SGSI" = "SGSI", "IDJA" = "IDJA", "EUTA" = "EUTA"),
  frequency = c("MONTHLY" = "1M", "QUARTERLY" = "3M", "SEMIANNUAL" = "6M", "ANNUAL" = "1Y", "TERM" = "1T"),
  reset_relative_to = c("PERIOD_START" = "CalculationPeriodStartDate", "PERIOD_END" = "CalculationPeriodEndDate"),
  return_type = c("TOTAL" = "Total", "PRICE" = "Price"),
  id_scheme = c("ISIN" = "http://www.fpml.org/coding-scheme/external/instrument-id-ISIN-1-0",
                "CUSIP" = "http://www.fpml.org/coding-scheme/external/instrument-id-CUSIP-1-0",
                "INDEX_CODE" = "urn:curie:index-code"),
  definitions = c("2006 ISDA DEFINITIONS" = "ISDA2006", "2021 ISDA INTEREST RATE DERIVATIVES DEFINITIONS" = "ISDA2021",
                  "2021 ISDA INTEREST RATES DERIVATIVES DEFINITIONS" = "ISDA2021")
)

.map_code_c1 <- function(map, x) {
  if (is.na(x)) return(NA_character_)
  key <- toupper(stringr::str_squish(x))
  if (key %in% names(map)) unname(map[[key]]) else NA_character_
}

# 「3M」「1T」などを期間の倍数と単位に分ける
.period_parts_c1 <- function(x) {
  m <- stringr::str_match(toupper(x %||% NA_character_), "^([0-9]+)([DWMYT])$")
  if (is.na(m[1, 1])) NULL else list(multiplier = m[1, 2], period = m[1, 3])
}

# 「Jakarta, Singapore, London and New York」→ 営業日の地点コード（対応しない地名があれば NA を含む）
.business_centers_c1 <- function(x) {
  if (is.na(x)) return(character())
  parts <- stringr::str_split(x, "\\s*(,|/|\\band\\b|;)\\s*")[[1]]
  parts <- stringr::str_squish(parts[nzchar(stringr::str_squish(parts))])
  purrr::map_chr(parts, \(p) .map_code_c1(.fpml_code_maps_c1$business_center, p))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# FpMLの材料を用意し、変換できなかった項目を返す
prepare_fpml_values_c1 <- function(values) {
  v <- as.list(values)
  g <- function(f) v[[f]] %||% NA_character_
  conv <- list(
    day_count = .map_code_c1(.fpml_code_maps_c1$day_count, g("funding.day_count")),
    payment_bdc = .map_code_c1(.fpml_code_maps_c1$business_day_convention, g("funding.payment_business_day_convention")),
    fixing_bdc = .map_code_c1(.fpml_code_maps_c1$business_day_convention, g("funding.fixing_business_day_convention")),
    payment_centers = .business_centers_c1(g("funding.payment_calendar")),
    fixing_centers = .business_centers_c1(g("funding.fixing_calendar")),
    valuation_centers = .business_centers_c1(g("return.valuation_calendar")),
    payment_freq = .period_parts_c1(.map_code_c1(.fpml_code_maps_c1$frequency, g("funding.payment_frequency"))),
    reset_freq = .period_parts_c1(.map_code_c1(.fpml_code_maps_c1$frequency, g("funding.reset_frequency"))),
    index_tenor = .period_parts_c1(g("funding.index_tenor")),
    reset_relative_to = .map_code_c1(.fpml_code_maps_c1$reset_relative_to, g("funding.reset_relative_to")),
    return_type = .map_code_c1(.fpml_code_maps_c1$return_type, g("return.return_type")),
    id_scheme = .map_code_c1(.fpml_code_maps_c1$id_scheme, g("return.underlyer_id_type")),
    definitions = .map_code_c1(.fpml_code_maps_c1$definitions, g("trade.isda_definitions"))
  )
  # 元の値があるのにFpMLのコードへ変換できなかった項目
  source_of <- c(day_count = "funding.day_count", payment_bdc = "funding.payment_business_day_convention",
                 fixing_bdc = "funding.fixing_business_day_convention", payment_freq = "funding.payment_frequency",
                 reset_freq = "funding.reset_frequency", index_tenor = "funding.index_tenor",
                 reset_relative_to = "funding.reset_relative_to", return_type = "return.return_type",
                 id_scheme = "return.underlyer_id_type", definitions = "trade.isda_definitions")
  failed <- names(source_of)[purrr::map_lgl(names(source_of), \(k) !is.na(g(source_of[[k]])) &&
                                              (is.null(conv[[k]]) || all(is.na(conv[[k]]))))]
  centers <- c(payment_centers = "funding.payment_calendar", fixing_centers = "funding.fixing_calendar",
               valuation_centers = "return.valuation_calendar")
  failed_centers <- names(centers)[purrr::map_lgl(names(centers), \(k) any(is.na(conv[[k]])))]
  list(v = v, conv = conv,
       unmapped = unname(c(source_of[failed], centers[failed_centers])))
}

# xml2で子要素を足す。text が NA なら（ドラフトで値がない）要素を作らない。NA の属性は付けない
.add_c1 <- function(parent, name, text = NULL, ...) {
  if (!is.null(text) && (length(text) == 0L || is.na(text))) return(invisible(NULL))
  attrs <- list(...)
  attrs <- attrs[!purrr::map_lgl(attrs, \(a) is.null(a) || is.na(a))]
  args <- c(list(parent, name), if (!is.null(text)) list(as.character(text)), attrs)
  do.call(xml2::xml_add_child, args)
}
# 当事者などへの参照。参照先が決まらない（NA）なら要素を作らない
.add_ref_c1 <- function(parent, name, href) {
  if (is.na(href)) return(invisible(NULL))
  xml2::xml_add_child(parent, name, href = href)
}
# 日付がない（ドラフトで空欄・選択肢）ときは、日付の要素ごと出さない
.add_adjustable_date_c1 <- function(parent, name, date, bdc, centers, id = NULL) {
  if (is.na(date)) return(invisible(NULL))
  node <- .add_c1(parent, name, id = id)
  ad <- .add_c1(node, "adjustableDate")
  .add_c1(ad, "unadjustedDate", date)
  da <- .add_c1(ad, "dateAdjustments")
  .add_c1(da, "businessDayConvention", if (is.na(bdc)) "NONE" else bdc)
  if (length(centers) && !all(is.na(centers))) {
    bc <- .add_c1(da, "businessCenters")
    purrr::walk(stats::na.omit(centers), \(c) .add_c1(bc, "businessCenter", c))
  }
  invisible(node)
}

# FpML文書（xml_document）を作る
# unconfirmed：照合は未確定だが、抽出値で要素を出した必須項目（ドラフトのみ）
# not_output ：空欄・選択肢・記載なし・不一致のため要素を出していない必須項目（ドラフトのみ）
build_fpml_trs_c1 <- function(values, document_id, mode = c("final", "draft"),
                              unconfirmed = character(), not_output = character()) {
  mode <- match.arg(mode)
  p <- prepare_fpml_values_c1(values)
  v <- p$v; cv <- p$conv
  g <- function(f) v[[f]] %||% NA_character_
  dealer <- g("trade.dealer")
  party_of <- function(name) {
    if (is.na(name)) return(NA_character_)
    if (!is.na(dealer) && grepl(toupper(dealer), toupper(name), fixed = TRUE)) "party_dealer" else "party_counterparty"
  }
  tr_payer <- party_of(g("trade.total_return_payer"))
  tr_receiver <- party_of(g("trade.total_return_receiver"))

  doc <- xml2::xml_new_root("dataDocument", xmlns = .fpml_ns_c1, fpmlVersion = "5-12")
  head <- c(
    paste0("CURIE c1 TRS FpML（", if (mode == "final") "正式版" else "ドラフト：正式なFpMLではない", "） document_id=", document_id),
    "FpML 5.x confirmation の returnSwap を目指した構造。公式XSDでの検証は未実施",
    if (length(unconfirmed)) paste0("照合が未確定のまま抽出値で出力した必須項目: ", paste(unconfirmed, collapse = ", ")),
    if (length(not_output)) paste0("空欄・選択肢・記載なし・不一致のため出力していない必須項目: ", paste(not_output, collapse = ", ")),
    if (length(p$unmapped)) paste0("FpMLのコードへ変換できなかった項目: ", paste(p$unmapped, collapse = ", "))
  )
  purrr::walk(head, \(h) xml2::xml_add_child(doc, xml2::xml_comment(paste0(" ", h, " "))))
  trade <- .add_c1(doc, "trade")
  th <- .add_c1(trade, "tradeHeader")
  pti <- .add_c1(th, "partyTradeIdentifier")
  .add_ref_c1(pti, "partyReference", "party_dealer")
  .add_c1(pti, "tradeId", g("trade.dealer_reference"), tradeIdScheme = "urn:curie:dealer-reference")
  .add_c1(th, "tradeDate", g("trade.trade_date"))

  rs <- .add_c1(trade, "returnSwap")
  # リターン・レッグ：参照資産のトータル・リターン（支払者 → 受取者）
  rl <- .add_c1(rs, "returnLeg", id = "returnLeg")
  .add_ref_c1(rl, "payerPartyReference", tr_payer)
  .add_ref_c1(rl, "receiverPartyReference", tr_receiver)
  .add_adjustable_date_c1(rl, "effectiveDate", g("trade.effective_date"), NA, character(), id = "effectiveDate")
  .add_adjustable_date_c1(rl, "terminationDate", g("trade.termination_date"), cv$payment_bdc, cv$payment_centers,
                          id = "terminationDate")
  und <- .add_c1(.add_c1(rl, "underlyer"), "singleUnderlyer")
  asset <- .add_c1(und, if (identical(toupper(g("return.underlyer_type")), "BOND")) "bond" else "index")
  .add_c1(asset, "instrumentId", g("return.underlyer_id"), instrumentIdScheme = cv$id_scheme)
  .add_c1(asset, "description", g("return.underlyer_name"))
  .add_c1(asset, "currency", g("return.underlyer_currency"))
  if (identical(toupper(g("return.underlyer_type")), "BOND")) .add_c1(asset, "maturity", g("return.underlyer_maturity_date"))
  ror <- .add_c1(rl, "rateOfReturn")
  if (!is.na(g("return.initial_price"))) {
    np <- .add_c1(.add_c1(ror, "initialPrice"), "netPrice")
    .add_c1(np, "amount", g("return.initial_price"))
    .add_c1(np, "priceExpression", "PercentageOfNotional")
  }
  vpf <- .add_c1(ror, "valuationPriceFinal")
  if (!is.na(g("return.valuation_date_rule"))) {
    xml2::xml_add_child(vpf, xml2::xml_comment(paste0(" 評価日の決め方（原文の要約）: ", g("return.valuation_date_rule"), " ")))
  }
  .add_adjustable_date_c1(.add_c1(vpf, "valuationRules"), "valuationDate", g("return.final_valuation_date"),
                          NA, cv$valuation_centers)
  notional <- .add_c1(.add_c1(rl, "notional"), "notionalAmount", id = "returnLegNotional")
  .add_c1(notional, "currency", g("trade.notional_currency"))
  .add_c1(notional, "amount", g("trade.notional_amount"))
  .add_c1(.add_c1(rl, "amount"), "referenceAmount", "Standard ISDA")
  .add_c1(.add_c1(rl, "return"), "returnType", cv$return_type)

  # 資金調達レッグ：参照金利＋スプレッド（トータル・リターンの受取者が支払う）
  il <- .add_c1(rs, "interestLeg", id = "interestLeg")
  .add_ref_c1(il, "payerPartyReference", tr_receiver)
  .add_ref_c1(il, "receiverPartyReference", tr_payer)
  pd <- .add_c1(il, "interestLegCalculationPeriodDates", id = "interestLegPeriodDates")
  .add_adjustable_date_c1(pd, "effectiveDate", g("trade.effective_date"), NA, character())
  .add_adjustable_date_c1(pd, "terminationDate", g("trade.termination_date"), cv$payment_bdc, cv$payment_centers)
  rd <- .add_c1(pd, "interestLegResetDates", id = "interestLegResetDates")
  .add_ref_c1(rd, "calculationPeriodDatesReference", "interestLegPeriodDates")
  .add_c1(rd, "resetRelativeTo", cv$reset_relative_to)
  if (!is.na(g("funding.fixing_offset_days"))) {
    fx <- .add_c1(rd, "fixingDates")
    .add_c1(fx, "periodMultiplier", g("funding.fixing_offset_days"))
    .add_c1(fx, "period", "D")
    .add_c1(fx, "dayType", "Business")
    .add_c1(fx, "businessDayConvention", if (is.na(cv$fixing_bdc)) "NONE" else cv$fixing_bdc)
    if (length(cv$fixing_centers)) {
      bc <- .add_c1(fx, "businessCenters")
      purrr::walk(stats::na.omit(cv$fixing_centers), \(c) .add_c1(bc, "businessCenter", c))
    }
    .add_ref_c1(fx, "dateRelativeTo", "interestLegResetDates")
  }
  if (!is.null(cv$reset_freq)) {
    rf <- .add_c1(rd, "resetFrequency")
    .add_c1(rf, "periodMultiplier", cv$reset_freq$multiplier)
    .add_c1(rf, "period", cv$reset_freq$period)
  }
  pay <- .add_c1(.add_c1(pd, "interestLegPaymentDates"), "periodicDates")
  .add_adjustable_date_c1(pay, "calculationStartDate", g("trade.effective_date"), NA, character())
  if (!is.null(cv$payment_freq)) {
    cf <- .add_c1(pay, "calculationPeriodFrequency")
    .add_c1(cf, "periodMultiplier", cv$payment_freq$multiplier)
    .add_c1(cf, "period", cv$payment_freq$period)
  }
  adj <- .add_c1(pay, "calculationPeriodDatesAdjustments")
  .add_c1(adj, "businessDayConvention", if (is.na(cv$payment_bdc)) "NONE" else cv$payment_bdc)
  if (length(cv$payment_centers)) {
    bc <- .add_c1(adj, "businessCenters")
    purrr::walk(stats::na.omit(cv$payment_centers), \(c) .add_c1(bc, "businessCenter", c))
  }
  .add_ref_c1(.add_c1(il, "notional"), "relativeNotionalAmount", "returnLegNotional")
  ia <- .add_c1(il, "interestAmount")
  .add_c1(ia, "currency", g("trade.notional_currency"))
  .add_c1(ia, "referenceAmount", "Standard ISDA")
  ic <- .add_c1(il, "interestCalculation")
  frc <- .add_c1(ic, "floatingRateCalculation")
  .add_c1(frc, "floatingRateIndex", g("funding.floating_rate_index"))
  if (!is.null(cv$index_tenor)) {
    it <- .add_c1(frc, "indexTenor")
    .add_c1(it, "periodMultiplier", cv$index_tenor$multiplier)
    .add_c1(it, "period", cv$index_tenor$period)
  }
  if (!is.na(g("funding.spread_bp"))) {
    .add_c1(.add_c1(frc, "spreadSchedule"), "initialValue", format(as.numeric(g("funding.spread_bp")) / 10000, scientific = FALSE))
  }
  if (!is.na(g("funding.rate_floor_pct"))) {
    .add_c1(.add_c1(frc, "floorRateSchedule"), "initialValue", format(as.numeric(g("funding.rate_floor_pct")) / 100, scientific = FALSE))
  }
  .add_c1(ic, "dayCountFraction", cv$day_count)

  # この構造ではFpMLの要素に対応させていない項目。値を落とさず、コメントに記録する
  not_as_element <- c("funding.compounding_method", "funding.lookback_days", "funding.observation_shift",
                      "funding.payment_lag_days", "return.valuation_frequency", "return.valuation_time",
                      "return.income_treatment", "trade.calculation_agent", "trade.fx_initial_spot_rate",
                      "trade.fx_rate_quotation", "trade.fx_hedge", "return.underlyer_issuer")
  kept <- purrr::keep(not_as_element, \(f) !is.na(g(f)))
  if (length(kept)) {
    xml2::xml_add_child(trade, xml2::xml_comment(paste0(
      " FpMLの要素に対応させていない項目（値はここに記録）: ",
      paste0(kept, "=", purrr::map_chr(kept, g), collapse = "; "), " ")))
  }

  if (!is.na(cv$definitions)) .add_c1(.add_c1(trade, "documentation"), "contractualDefinitions", cv$definitions)
  pdl <- .add_c1(doc, "party", id = "party_dealer")
  .add_c1(pdl, "partyName", dealer)
  pcp <- .add_c1(doc, "party", id = "party_counterparty")
  cp_name <- purrr::detect(c(g("trade.total_return_payer"), g("trade.total_return_receiver")),
                           \(n) !is.na(n) && party_of(n) == "party_counterparty")
  .add_c1(pcp, "partyName", cp_name %||% NA_character_)
  doc
}
