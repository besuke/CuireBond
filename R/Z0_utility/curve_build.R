# OISカーブの構築（共通）。移植した instance_swap_curve() を評価日の気配だけで使う。


# 評価日の気配で、必要な通貨のOISカーブを1回だけ作る。失敗は理由付きで返す。
build_ois_curves_CURIE <- function(ois, valuation_date, currencies) {
  currencies |>
    rlang::set_names() |>
    purrr::map(function(ccy) {
      quotes <- dplyr::filter(ois, .data$marketdate == valuation_date, .data$currency == ccy)
      if (!nrow(quotes)) {
        return(list(error = paste0(ccy, " の評価日 ", valuation_date, " のOIS気配がない（別の日付では補わない）")))
      }
      tryCatch({
        curve <- instance_swap_curve(ccy, marketdate = valuation_date, quotes = quotes)
        curve$curve_id <- paste("OIS", paste(unique(quotes$source_file), collapse = ","),
                                valuation_date, unique(curve$quotes$instrument), sep = "|")
        curve$quote_time <- if ("quote_time" %in% names(quotes)) {
          paste(unique(stats::na.omit(quotes$quote_time)), collapse = ", ")
        } else ""
        curve
      }, error = function(e) list(error = conditionMessage(e)))
    })
}

summarise_ois_curves_CURIE <- function(curves) {
  purrr::imap(curves, function(x, ccy) {
    if (!is.null(x$error)) {
      return(tibble::tibble(currency = ccy, curve_status = "failed", curve_message = x$error))
    }
    tibble::tibble(
      currency = ccy, curve_status = "success", curve_message = "",
      ois_curve_id = x$curve_id, index = x$index_name, marketdate = x$marketdate,
      quote_time = x$quote_time, quotes = nrow(x$quotes), last_tenordate = max(x$quotes$tenordate),
      max_abs_quote_error_bp = max(abs(x$quote_repricing$quote_error_bp))
    )
  }) |> purrr::list_rbind()
}

