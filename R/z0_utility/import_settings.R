# 取込設定と、テキスト市場データの共通読込。
# 実ファイルの形式が決まったら、ここで定義する設定（区切り文字・文字コード・列名対応）を
# 呼出側で指定するだけで差し替えられるようにする。

# オファーExcelの取込設定（convert_offer_sheet_Curie() の引数）
# sheet      : 対象シート名。NULLなら見出しから自動判定（候補が複数なら停止）
# header_row : 見出しのExcel行番号。NULLなら自動判定
# column_map : c(共通列名 = "元の見出し")
# currency   : 通貨列がないシートで使う通貨（JPYブックなど）
# size_multiplier : 数量の倍率（見出しの単位より優先）
offer_import_settings_Curie <- function(sheet = NULL, header_row = NULL, column_map = NULL,
                                        currency = NULL, size_multiplier = NULL, scan_rows = 30) {
  list(sheet = sheet, header_row = header_row, column_map = column_map,
       currency = currency, size_multiplier = size_multiplier, scan_rows = scan_rows)
}

# テキスト市場データの取込設定
# delim      : 区切り文字（デモは "|"）
# encoding   : 文字コード（例 "UTF-8"、"CP932"）
# column_map : c(共通列名 = "元の列名")。対応しない列はそのまま残す
text_import_settings_Curie <- function(delim = "|", encoding = "UTF-8", column_map = NULL) {
  list(delim = delim, encoding = encoding, column_map = column_map)
}

# 区切り文字・文字コード・列名対応に従って読み、全列を文字列で返す。
# 元ファイル名（source_file）を付ける。
read_text_table_Curie <- function(path, settings = text_import_settings_Curie()) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  x <- readr::read_delim(
    path, delim = settings$delim, col_types = readr::cols(.default = "c"),
    locale = readr::locale(encoding = settings$encoding), trim_ws = TRUE,
    show_col_types = FALSE, progress = FALSE
  )
  if (nrow(readr::problems(x))) stop("Some rows cannot be read with the delimiter: ", path, call. = FALSE)
  map <- settings$column_map
  if (length(map)) {
    missing <- setdiff(unname(map), names(x))
    if (length(missing)) stop(basename(path), ": source columns in the column map are missing: ", paste(missing, collapse = ", "), call. = FALSE)
    x <- dplyr::rename(x, dplyr::all_of(map))
  }
  dplyr::mutate(x, source_file = basename(path))
}

# OISファイルを、移植した import_market_data() が読めるパイプ区切り・UTF-8・標準列名に
# そろえる。すでにその形式なら元ファイルをそのまま使う（既存関数は変更しない）。
normalize_ois_file_Curie <- function(path, settings, work_dir) {
  if (identical(settings$delim, "|") && identical(toupper(settings$encoding), "UTF-8") &&
      !length(settings$column_map)) {
    return(normalizePath(path, winslash = "/"))
  }
  x <- read_text_table_Curie(path, settings) |> dplyr::select(-"source_file")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(work_dir, paste0(tools::file_path_sans_ext(basename(path)), "_normalized.txt"))
  readr::write_delim(x, out, delim = "|", na = "")
  normalizePath(out, winslash = "/")
}

# 複数のOISファイルを読み、元ファイル名を残して1つにする。
# settings は1つ（全ファイル共通）か、ファイルごとの list。
import_ois_files_Curie <- function(paths, settings, work_dir) {
  if (!length(paths)) stop("Specify the OIS files.", call. = FALSE)
  per_file <- if (!is.null(settings$delim)) rep(list(settings), length(paths)) else settings
  if (length(per_file) != length(paths)) stop("The number of ois_settings does not match the number of files.", call. = FALSE)
  normalized <- purrr::map2_chr(paths, per_file, \(p, s) normalize_ois_file_Curie(p, s, work_dir))
  quotes <- purrr::map2(normalized, paths, function(n, original) {
    import_market_data(n) |> dplyr::mutate(source_file = basename(original))
  }) |> purrr::list_rbind()
  if (anyDuplicated(quotes[c("marketdate", "currency", "instrument", "tenor")])) {
    stop("OIS quotes have duplicated marketdate + currency + instrument + tenor.", call. = FALSE)
  }
  list(quotes = quotes, normalized_paths = normalized)
}

# 市場データの日付を評価日と照合した記録（不一致・不足を黙って補わないため）
market_data_log_Curie <- function(x, data_type, valuation_date, date_col = "marketdate") {
  if (!"quote_time" %in% names(x)) x$quote_time <- NA_character_
  x |>
    dplyr::summarise(
      rows = dplyr::n(),
      rows_on_valuation_date = sum(.data[[date_col]] == valuation_date, na.rm = TRUE),
      marketdates = paste(sort(unique(format(.data[[date_col]]))), collapse = ", "),
      quote_times = paste(sort(unique(stats::na.omit(.data$quote_time))), collapse = ", "),
      .by = "source_file"
    ) |>
    dplyr::mutate(
      data_type = data_type, valuation_date = valuation_date,
      status = dplyr::if_else(.data$rows_on_valuation_date > 0L, "使用",
                              "評価日のデータなし（別の日付では補わない）"),
      .before = 1
    )
}
