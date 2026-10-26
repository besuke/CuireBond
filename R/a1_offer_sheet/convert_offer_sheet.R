# オファーExcelをCurieの共通形式へ変換する。
#
# 共通形式の必須列は LagrangeFinance の import_offer_sheet() と同じ6列：
#   offer_date, ISIN, broker_names, offer_price, offer_size, offer_settlementdate
# 単位：offer_price は額面100当たりのクリーン価格、offer_size は額面金額（通貨単位、億円→円 等に換算）
# Curie独自の追加列：offer_currency, offer_security_name, sheet_issue_price,
#   offer_size_unit, offer_size_multiplier, offer_source_file, offer_source_sheet,
#   offer_source_row（Excel上の行番号）, offer_header_row
#
# シート名や列位置には依存せず、各シートの先頭から見出し行を探して対象を判定する。
# 候補が複数ある場合は停止し、sheet・header_row での明示指定を求める。


# ---- 見出しの判定 --------------------------------------------------------

# 見出しの比較用キー：半角化・小文字化し、括弧内（単位）と空白・記号を除く
header_key_Curie <- function(x) {
  to_halfwidth_Curie(x) |>
    stringr::str_to_lower() |>
    stringr::str_remove_all("\\([^)]*\\)") |>
    stringr::str_remove_all("[\\s._:/・\\-]")
}

# 見出しの括弧内の単位（例：「数量(億円)」→「億円」、「Size (MM)」→「MM」）
header_unit_Curie <- function(x) {
  stringr::str_match(to_halfwidth_Curie(x), "\\(([^)]*)\\)")[, 2]
}

# 列名の別名。比較は header_key_Curie() 後の完全一致で行う（部分一致はしない）。
# 「発行価格」がオファー単価に誤って対応しないよう、発行価格は独立した項目にする。
offer_header_aliases_Curie <- function() {
  tibble::tribble(
    ~field,                 ~alias,
    "offer_date",           c("オファー日", "提示日", "日付", "trade date", "offer date", "date", "as of date"),
    "ISIN",                 c("ISIN", "ISINコード", "ISIN code"),
    "broker_names",         c("業者名", "業者", "証券会社", "ブローカー", "dealer", "broker", "counterparty"),
    "offer_price",          c("単価", "オファー単価", "価格", "price", "offer price", "clean price"),
    "offer_size",           c("数量", "額面", "額面金額", "size", "amount", "quantity", "notional", "face amount"),
    "offer_settlementdate", c("受渡日", "決済日", "settle date", "settlement date", "value date"),
    "offer_currency",       c("通貨", "ccy", "currency"),
    "offer_security_name",  c("銘柄名", "銘柄", "security", "description", "security name"),
    "sheet_issue_price",    c("発行価格", "発行単価", "参考:発行価格", "issue price")
  ) |>
    tidyr::unnest_longer("alias") |>
    dplyr::mutate(key = header_key_Curie(.data$alias)) |>
    dplyr::distinct(.data$field, .data$key)
}

.required_offer_fields_Curie <- c(
  "offer_date", "ISIN", "broker_names", "offer_price", "offer_size", "offer_settlementdate"
)

# 見出しの各セルを共通列名へ対応付ける。column_map = c(共通列名 = "元の見出し") が優先。
map_header_cells_Curie <- function(cells, column_map = NULL, aliases = offer_header_aliases_Curie()) {
  keys <- header_key_Curie(cells)
  field <- aliases$field[match(keys, aliases$key)]
  if (length(column_map)) {
    hit <- match(stringr::str_trim(cells), stringr::str_trim(unname(column_map)))
    field[!is.na(hit)] <- names(column_map)[hit[!is.na(hit)]]
    # column_map で別の列に割り当てた項目は、別名による対応から外す
    field[is.na(hit) & field %in% names(column_map)] <- NA_character_
  }
  field[.blank_Curie(cells)] <- NA_character_
  field
}

# Excelの行番号どおりに読む（readxl は range 指定時に先頭の空行を詰めない）
.read_raw_sheet_Curie <- function(path, sheet, n_rows = NA) {
  readxl::read_excel(
    path, sheet = sheet, col_names = FALSE, col_types = "text",
    range = readxl::cell_limits(c(1, 1), c(n_rows, NA)),
    .name_repair = function(x) paste0("V", seq_along(x))
  )
}

# 全シートの先頭 scan_rows 行から、必須6項目がそろう見出し行の候補を探す
detect_offer_header_Curie <- function(path, scan_rows = 30, column_map = NULL) {
  aliases <- offer_header_aliases_Curie()
  purrr::map(readxl::excel_sheets(path), function(sheet) {
    raw <- .read_raw_sheet_Curie(path, sheet, scan_rows)
    if (!nrow(raw) || !ncol(raw)) return(NULL)
    purrr::map(seq_len(nrow(raw)), function(i) {
      fields <- map_header_cells_Curie(unlist(raw[i, ], use.names = FALSE), column_map, aliases)
      found <- intersect(.required_offer_fields_Curie, fields)
      if (length(found) < length(.required_offer_fields_Curie)) return(NULL)
      dup <- unique(fields[!is.na(fields) & duplicated(fields)])
      tibble::tibble(sheet = sheet, header_row = i,
                     n_fields = sum(!is.na(fields)),
                     duplicated_fields = paste(dup, collapse = ", "))
    }) |> purrr::list_rbind()
  }) |> purrr::list_rbind() |>
    (\(x) if (is.null(x) || !nrow(x)) tibble::tibble(sheet = character(), header_row = integer(),
                                                       n_fields = integer(), duplicated_fields = character()) else x)()
}

# 単位の文字から額面金額への倍率を決める
size_multiplier_Curie <- function(unit) {
  u <- stringr::str_to_lower(stringr::str_trim(dplyr::coalesce(unit, "")))
  dplyr::case_when(
    stringr::str_detect(u, "億") ~ 1e8,
    stringr::str_detect(u, "百万|^mm$|^mln$|^mio$|million") ~ 1e6,
    stringr::str_detect(u, "千|thousand|^k$|^000$") ~ 1e3,
    u %in% c("", "円", "yen", "face", "額面") ~ 1,
    .default = NA_real_
  )
}

# ---- 変換本体 ------------------------------------------------------------

# path       : オファーExcel（1ファイル）
# sheet      : 対象シート名。NULLなら見出しから自動判定（候補が複数なら停止）
# header_row : 見出しのExcel行番号。NULLなら自動判定
# column_map : c(共通列名 = "元の見出し")。別名で判定できない列や重複の解消に使う
# currency   : 通貨列がないシートの通貨（例：JPYブック）
# size_multiplier : 数量の倍率を明示する場合（見出しの単位より優先）
convert_offer_sheet_Curie <- function(path, sheet = NULL, header_row = NULL, column_map = NULL,
                                      currency = NULL, size_multiplier = NULL, scan_rows = 30) {
  if (!file.exists(path)) stop("File not found: ", path_display_Curie(path), call. = FALSE)
  candidates <- detect_offer_header_Curie(path, scan_rows, column_map)
  if (!is.null(sheet)) candidates <- dplyr::filter(candidates, .data$sheet == .env$sheet)
  if (!is.null(header_row)) candidates <- dplyr::filter(candidates, .data$header_row == .env$header_row)
  where <- basename(path)
  if (!nrow(candidates)) {
    stop(where, ": no header row has all 6 required fields (", paste(.required_offer_fields_Curie, collapse = ", "),
         "). Specify the column names with column_map.", call. = FALSE)
  }
  if (nrow(candidates) > 1L) {
    stop(where, ": multiple offer table candidates. Specify sheet (and header_row if needed).\n",
         paste0("  - sheet \"", candidates$sheet, "\", row ", candidates$header_row, collapse = "\n"),
         call. = FALSE)
  }
  if (nzchar(candidates$duplicated_fields)) {
    stop(where, ": multiple columns map to the same field (", candidates$duplicated_fields,
         "). Specify the column to use with column_map.", call. = FALSE)
  }
  target_sheet <- candidates$sheet
  head_row <- candidates$header_row

  raw <- .read_raw_sheet_Curie(path, target_sheet)
  header_cells <- unlist(raw[head_row, ], use.names = FALSE)
  fields <- map_header_cells_Curie(header_cells, column_map)
  keep <- !is.na(fields)
  body <- raw[-seq_len(head_row), keep, drop = FALSE] |> rlang::set_names(fields[keep])
  # 見出しの次の行から、最初の全空白行の手前までを表とする（下の注記を読まないため）
  blank_row <- purrr::pmap_lgl(body, function(...) all(.blank_Curie(c(...))))
  n_data <- if (any(blank_row)) which(blank_row)[1] - 1L else nrow(body)
  body <- body[seq_len(n_data), , drop = FALSE]
  rows <- head_row + seq_len(n_data)

  size_unit <- header_unit_Curie(header_cells[fields %in% "offer_size"])
  multiplier <- if (is.null(size_multiplier)) size_multiplier_Curie(size_unit) else size_multiplier
  if (is.na(multiplier)) {
    stop(where, ": cannot determine the size unit \"", size_unit, "\". Specify size_multiplier.",
         call. = FALSE)
  }
  optional <- c("offer_currency", "offer_security_name", "sheet_issue_price")
  body[setdiff(optional, names(body))] <- NA_character_

  out <- body |>
    dplyr::transmute(
      offer_date = parse_date_Curie(.data$offer_date, "offer_date", rows),
      ISIN = stringr::str_to_upper(stringr::str_trim(.data$ISIN)),
      broker_names = stringr::str_squish(.data$broker_names),
      offer_price = parse_number_Curie(.data$offer_price, "offer_price", rows),
      offer_size = parse_number_Curie(.data$offer_size, "offer_size", rows) * multiplier,
      offer_settlementdate = parse_date_Curie(.data$offer_settlementdate, "offer_settlementdate", rows),
      offer_currency = dplyr::coalesce(stringr::str_to_upper(stringr::str_trim(.data$offer_currency)),
                                       if (is.null(currency)) NA_character_ else toupper(currency)),
      offer_security_name = stringr::str_squish(.data$offer_security_name),
      sheet_issue_price = parse_number_Curie(.data$sheet_issue_price, "sheet_issue_price", rows),
      offer_size_unit = dplyr::coalesce(size_unit, "") |> (\(u) if (nzchar(u)) u else "額面（倍率1）")(),
      offer_size_multiplier = multiplier,
      offer_source_file = normalizePath(path, winslash = "/"),
      offer_source_sheet = target_sheet,
      offer_source_row = rows,
      offer_header_row = head_row
    )
  attr(out, "column_mapping") <- tibble::tibble(source_header = header_cells[keep], field = fields[keep])
  out
}

# LagrangeFinance の import_offer_sheet() で読める標準形式のExcelを書き出す。
# 必須6列を先頭に置き、Curieの追加列はその後ろに残す。
write_offer_standard_Curie <- function(offers, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_xlsx_Curie(
    list(offers = dplyr::relocate(offers, dplyr::all_of(.required_offer_fields_Curie))),
    path
  )
  invisible(path)
}

