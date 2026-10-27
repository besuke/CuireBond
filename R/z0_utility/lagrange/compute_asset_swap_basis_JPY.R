# 移植元: LagrangeFinance/R/compute_asset_swap_basis_JPY.R（ブランチ refactor/gaussquant-consolidation のコミット 371d12d。e6614e1 からの差分は GaussQuant の日付・評価日関数への置き換えのみ）
# 関数名・引数・計算ロジックは変更していない。この行より下は移植元のまま。
# ------------------------------------------------------------------------
# JPY-collateral, constant-notional cross-currency asset swap.
# Source R/compute_asset_swap_ISO.R first. Basis is quoted on the JPY receiving leg:
# pay foreign overnight flat / receive TONAR + basis_bp.
# fx_spot is JPY per foreign unit for exchange on effective_date.
# Version 1 calibrates ONE constant discount spread to the maturity CCS quote.
# It is a single-quote model, not a market term-structure bootstrap or MtM CCS.

.xasw_df <- function(curve, dates) {
  purrr::map_dbl(as.character(dates), function(d) {
    x <- as.numeric(curve$discount(GaussQuant::date_GQL(d)))
    if (length(x) != 1L || !is.finite(x) || x <= 0) stop("Invalid discount factor.")
    x
  }) 
}

# Core also accepts independently constructed zero-spread ASW templates.
# Both templates must share effective date, maturity and foreign nominal.
asset_swap_jpy_from_legs <- function(
  native_asw, jpy_template, foreign_curve, jpy_curve, fx_spot, basis_bp,
  currency, spread_bp = 0
) {
  scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!scalar(fx_spot) || fx_spot <= 0 || !scalar(basis_bp) ||
      !scalar(spread_bp)) stop("Invalid FX or spread input.")
  if (toupper(currency) == "JPY") stop("Use a foreign currency.")
  if (!identical(as.Date(foreign_curve$evaluation_date),
                 as.Date(jpy_curve$evaluation_date))) stop("Curve dates differ.")
  if (!identical(as.Date(native_asw$effective_date),
                 as.Date(jpy_template$effective_date)) ||
      !identical(as.Date(native_asw$maturity_date),
                 as.Date(jpy_template$maturity_date))) stop("ASW dates differ.")
  nv <- native_asw$valuation
  jv <- jpy_template$valuation
  if (nrow(nv) != 1L || nrow(jv) != 1L ||
      nv$spread != 0 || jv$spread != 0 ||
      abs(nv$nominal - jv$nominal) > 1e-10) {
    stop("Use zero-spread templates with the same nominal.")
  }
  if (!scalar(jv$spread_annuity) || jv$spread_annuity <= 0) {
    stop("Invalid JPY annuity.")
  }
  start <- as.Date(native_asw$effective_date)
  eval_date <- as.Date(jpy_curve$evaluation_date)
  if (start < eval_date) stop("Effective date precedes valuation date.")
  time <- function(dates) as.numeric(as.Date(dates) - start) / 365
  foreign_start_df <- .xasw_df(foreign_curve$curve, start)
  jpy_start_df <- .xasw_df(jpy_curve$curve, start)
  fcf <- native_asw$cashflows
  jcf <- jpy_template$cashflows
  fbase <- fcf[fcf$leg %in% c("receive_overnight", "receive_principal"), ]
  jbase <- jcf[jcf$leg %in% c("receive_overnight", "receive_principal"), ]
  bond <- fcf[fcf$leg == "pay_bond", ]
  if (!nrow(fbase) || !nrow(jbase) || !nrow(bond) ||
      any(fbase$payment_date <= start) || any(jbase$payment_date <= start) ||
      any(bond$payment_date <= start)) stop("Missing or invalid future legs.")
  base_jpy_pv <- fx_spot * sum(
    jbase$amount * .xasw_df(jpy_curve$curve, jbase$payment_date)
  )
  jpy_annuity <- fx_spot * jv$spread_annuity
  basis <- basis_bp / 1e4
  # Under the common JPY collateral convention:
  # F(t) = FX(start) * [Pf(t)/Pf(start)]/[Pj(t)/Pj(start)] * exp(-z*t).
  # Calibrate z so that the specified CCS is par, including final principal.
  foreign_base_pv <- function(z) fx_spot * jpy_start_df * sum(
    fbase$amount *
      .xasw_df(foreign_curve$curve, fbase$payment_date) / foreign_start_df *
      exp(-z * time(fbase$payment_date))
  )
  objective <- function(z) {
    (foreign_base_pv(z) - base_jpy_pv - basis * jpy_annuity) /
      (fx_spot * nv$nominal)
  }
  z <- stats::uniroot(objective, c(-1, 1), tol = 1e-12)$root
  forward <- function(dates) {
    dates <- as.Date(dates)
    if (anyNA(dates) || any(dates < start)) stop("Invalid forward dates.")
    fx_spot *
      (.xasw_df(foreign_curve$curve, dates) / foreign_start_df) /
      (.xasw_df(jpy_curve$curve, dates) / jpy_start_df) *
      exp(-z * time(dates))
  }
  bond$foreign_amount <- -bond$amount
  bond$fx_forward <- forward(bond$payment_date)
  bond$jpy_amount <- bond$foreign_amount * bond$fx_forward
  bond$jpy_discount_factor <- .xasw_df(jpy_curve$curve, bond$payment_date)
  bond$jpy_present_value <- bond$jpy_amount * bond$jpy_discount_factor
  bond_pv <- sum(bond$jpy_present_value)
  # Non-par bond price is settled at the initial FX exchange rate.
  upfront_pv <- fx_spot * jv$upfront_pv
  fair <- (bond_pv - base_jpy_pv - upfront_pv) / jpy_annuity
  npv <- base_jpy_pv + upfront_pv + spread_bp / 1e4 * jpy_annuity - bond_pv
  structure(list(
    valuation = tibble::tibble(
      currency = toupper(currency),
      native_asw_bp = nv$fair_spread_bp,
      basis_on_jpy_bp = basis_bp,
      jpy_asw_bp = fair * 1e4,
      simple_sum_bp = nv$fair_spread_bp + basis_bp,
      spread_bp = spread_bp,
      npv_jpy = npv,
      fair_spread_npv_jpy = base_jpy_pv + upfront_pv + fair * jpy_annuity - bond_pv,
      hedged_bond_pv_jpy = bond_pv,
      jpy_base_leg_pv = base_jpy_pv,
      upfront_pv_jpy = upfront_pv,
      spread_annuity_jpy = jpy_annuity,
      calibrated_discount_spread_bp = z * 1e4,
      ccs_repricing_npv_jpy = foreign_base_pv(z) - base_jpy_pv - basis * jpy_annuity
    ),
    hedged_bond_cashflows = bond,
    fx_forward = forward,
    native_asw = native_asw,
    jpy_template = jpy_template,
    effective_date = start,
    maturity_date = native_asw$maturity_date,
    assumptions = c(
      "JPY collateral; constant notionals; principal exchanged at start and end.",
      "Basis is on the JPY leg: pay foreign overnight / receive TONAR + basis.",
      "One maturity basis quote calibrates a constant foreign discount spread.",
      "Bond coupons and redemption are hedged using model FX forwards.",
      "Par-par ASW with initial price premium/discount exchanged at initial FX.",
      "No MtM resets, transaction costs, stochastic quanto or credit adjustments."
    )
  ), class = "jpy_asset_swap_valuation")
}

compute_asset_swap_basis_JPY <- function(
  bond, clean_price, foreign_curve, jpy_curve, fx_spot, basis_bp,
  currency = foreign_curve$currency,
  foreign_payment_frequency = "Semiannual",
  jpy_payment_frequency = "Semiannual",
  foreign_payment_lag = 0L, jpy_payment_lag = 0L, spread_bp = 0
) {
  if (is.null(currency) || length(currency) != 1L || is.na(currency)) {
    stop("Supply the foreign currency.")
  }
  if (!is.null(jpy_curve$currency) && jpy_curve$currency != "JPY") {
    stop("jpy_curve must be JPY.")
  }
  if (!is.null(foreign_curve$currency) &&
      toupper(currency) != foreign_curve$currency) stop("Foreign currency mismatch.")
  native <- compute_asset_swap_ISO(
    bond, clean_price, foreign_curve, spread_bp = 0,
    payment_frequency = foreign_payment_frequency, payment_lag = foreign_payment_lag
  )
  yen <- compute_asset_swap_ISO(
    bond, clean_price, jpy_curve, spread_bp = 0,
    payment_frequency = jpy_payment_frequency, payment_lag = jpy_payment_lag
  )
  asset_swap_jpy_from_legs(
    native, yen, foreign_curve, jpy_curve, fx_spot, basis_bp, currency, spread_bp
  )
}



