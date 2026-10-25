# 表示用の整形（共通）。HTMLレポートで表を見せるための書式で、計算はしない。
# flextable はレポート表示のときだけ使う（本体の計算・Excel出力には不要）。
#
# 欠損と未計算を区別する：
#   未計算 … 計算状態が成功以外で、その値を計算していない（未対応・失敗・入力不備・定義上の未計算）
#   欠損   … 計算したはずの値が欠けている（本来は起きない。起きたら要確認）
#   —      … その項目が対象外（例：JPY債の円換算ASW）

.na_label_CURIE <- function(computed) dplyr::if_else(computed, "欠損", "未計算")

# 数値を桁数をそろえた文字列にする。computed は「計算済みであるべき行」かどうか
format_number_CURIE <- function(x, digits = 2, computed = rep(TRUE, length(x)), big_mark = ",") {
  out <- formatC(x, format = "f", digits = digits, big.mark = big_mark)
  dplyr::if_else(is.na(x), .na_label_CURIE(computed), out)
}

format_date_CURIE <- function(x, na = "—") {
  dplyr::if_else(is.na(x), na, format(as.Date(x), "%Y-%m-%d"))
}

# 計算状態・検証状態の日本語表示
pricing_status_label_CURIE <- function(x) {
  dplyr::recode(x, success = "成功", partial = "一部未計算", failed = "失敗",
                unsupported = "未対応", skipped = "入力不備", .default = x, .missing = "—")
}
validation_status_label_CURIE <- function(x) {
  dplyr::recode(x, valid = "合格", invalid = "不合格", .default = x, .missing = "—")
}

# flextable の共通書式。labels = c(列名 = "見出し")、widths = c(列名 = 幅（インチ）)、
# right = 右寄せにする列（数値列）
flextable_CURIE <- function(data, labels = NULL, widths = NULL, right = character(), caption = NULL) {
  ft <- flextable::flextable(data)
  if (length(labels)) {
    labels <- labels[intersect(names(labels), names(data))]
    ft <- flextable::set_header_labels(ft, values = as.list(labels))
  }
  ft <- ft |>
    flextable::theme_vanilla() |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::bg(part = "header", bg = "#E8F1F1") |>
    flextable::bold(part = "header") |>
    flextable::valign(valign = "top", part = "all") |>
    flextable::padding(padding = 3, part = "all")
  right <- intersect(right, names(data))
  if (length(right)) ft <- flextable::align(ft, j = right, align = "right", part = "all")
  if (length(widths)) {
    widths <- widths[intersect(names(widths), names(data))]
    ft <- purrr::reduce2(names(widths), unname(widths), \(acc, j, w) flextable::width(acc, j = j, width = w), .init = ft)
  }
  # 未計算・欠損を目立たせる（欠損は要確認なので赤）
  ft <- purrr::reduce(names(data), function(acc, j) {
    v <- data[[j]]
    if (!is.character(v)) return(acc)
    marks <- list("#8A6D00" = which(v == "未計算"), "#B00020" = which(v == "欠損"))
    purrr::reduce2(names(marks), marks, function(a, colour, rows) {
      if (length(rows)) flextable::color(a, i = rows, j = j, color = colour) else a
    }, .init = acc)
  }, .init = ft)
  if (!is.null(caption)) ft <- flextable::set_caption(ft, caption)
  ft
}
