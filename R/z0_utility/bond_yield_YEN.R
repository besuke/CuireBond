# 円債の複利利回り（共通）。GaussQuant の円債関数を、Part II 第1章と同じ引数の与え方で使う。


# 円債の複利利回り（%）。Part II 第1章と同じ GaussQuant 関数・同じ引数の与え方
# （coupon_rate は%、クリーン単価、利払回数と複利回数を指定）。
# 残存年数は関数内の規約（満年数＋端数ACT/365）で、銘柄属性の day_count は使わない。
compound_yield_YEN_Curie <- function(settlement_date, maturity_date, coupon_rate, clean_price,
                                     redemption, coupon_frequency, compound_frequency) {
  GaussQuant::bond_compound_yield_from_price_YEN_GQL(
    start_date = as.character(settlement_date), end_date = as.character(maturity_date),
    coupon_rate = coupon_rate * 100, price = clean_price, redemption = redemption,
    coupon_frequency = coupon_frequency, compound_frequency = compound_frequency
  )
}

# JGB時価（単価と属性）から複利利回りを計算する
compute_jgb_yields_Curie <- function(jgb, compound_frequency = 2) {
  jgb |>
    dplyr::mutate(
      result = purrr::pmap(
        list(.data$settlement_date, .data$maturity_date, .data$coupon_rate, .data$clean_price,
             .data$redemption, .data$coupon_frequency, .data$price_type),
        function(settle, maturity, coupon, price, redemption, freq, type) {
          if (is.na(coupon)) return(list(y = NA_real_, msg = "JGBの銘柄属性がない"))
          if (!identical(type, "clean")) return(list(y = NA_real_, msg = "単価がクリーン価格ではない"))
          tryCatch(
            list(y = compound_yield_YEN_Curie(settle, maturity, coupon, price, redemption,
                                              freq, compound_frequency), msg = ""),
            error = function(e) list(y = NA_real_, msg = conditionMessage(e))
          )
        }
      ),
      compound_yield_pct = purrr::map_dbl(.data$result, "y"),
      yield_message = purrr::map_chr(.data$result, "msg"),
      compound_frequency = compound_frequency,
      remaining_years = purrr::map2_dbl(.data$settlement_date, .data$maturity_date,
                                        \(s, m) GaussQuant::bond_year_fraction_YEN_GQL(s, m))
    ) |>
    dplyr::select(-"result")
}

