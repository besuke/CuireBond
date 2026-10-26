# 債券の生成（共通）。銘柄属性の1行から GaussQuant::fixed_rate_bond_GQL() で固定利付債を作る。


calendar_from_name_Curie <- function(name) {
  switch(name,
    Japan = QuantLib::Japan(),
    UnitedStates_GovernmentBond = QuantLib::UnitedStates("GovernmentBond"),
    TARGET = QuantLib::TARGET(),
    stop("対応していないカレンダーです: ", name, call. = FALSE)
  )
}

frequency_period_Curie <- function(coupon_frequency) {
  name <- c(`1` = "Annual", `2` = "Semiannual", `4` = "Quarterly", `12` = "Monthly")[as.character(coupon_frequency)]
  if (is.na(name)) stop("対応していない利払回数です: ", coupon_frequency, call. = FALSE)
  QuantLib::Period(unname(name))
}

# 銘柄属性の1行から固定利付債を生成する（額面100、終期は計算終期）
build_fixed_rate_bond_Curie <- function(row) {
  GaussQuant::fixed_rate_bond_GQL(
    issue_date = as.character(row$issue_date),
    maturity_date = as.character(row$calculation_end_date),
    coupon_rate = row$coupon_rate,
    face_amount = 100,
    settlement_days = as.integer(row$settlement_days),
    frequency = frequency_period_Curie(row$coupon_frequency),
    calendar = calendar_from_name_Curie(row$calendar),
    accrual_day_counter = GaussQuant::day_counter_GQL(row$day_count),
    payment_convention = row$payment_convention,
    schedule_convention = row$schedule_convention,
    maturity_convention = row$maturity_convention,
    date_generation = row$date_generation,
    end_of_month = isTRUE(as.logical(row$end_of_month)),
    redemption = row$redemption
  )
}

