# 観測点の補間（共通）。償還日を横軸とした自然3次スプライン。外挿しない。


# 償還日ごとの観測値を、起点日から償還日までの残存年数（日数/365）を横軸として
# 自然3次スプラインで補間する（通貨ベーシスとJGB利回りで共通）。
# 横軸を一定倍・平行移動しても自然3次スプラインの補間値は変わらないため、
# 年日数（365）や起点日の取り方は、すべての点で同じであれば結果に影響しない。
# 観測範囲外は外挿せず、value = NA と理由を返す。
spline_by_maturity_Curie <- function(maturity_dates, values, origin_date, target_maturity, label) {
  if (anyDuplicated(maturity_dates)) stop("Points with the same maturity date must be averaged before interpolation.", call. = FALSE)
  if (length(maturity_dates) < 2L) {
    return(list(value = NA_real_, message = paste0(label, "の償還日が2点未満のため補間できない")))
  }
  x <- as.numeric(maturity_dates - origin_date) / 365
  x0 <- as.numeric(target_maturity - origin_date) / 365
  if (x0 < min(x) || x0 > max(x)) {
    return(list(value = NA_real_, message = paste0(
      "償還日 ", target_maturity, " が", label, "の範囲（", min(maturity_dates), "〜",
      max(maturity_dates), "）外。外挿しない")))
  }
  list(value = stats::splinefun(x, values, method = "natural")(x0), message = "")
}

