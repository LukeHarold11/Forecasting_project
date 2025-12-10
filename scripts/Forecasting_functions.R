library(pacman)
p_load(tidyverse, forecast, tsibble, gridExtra, timetk, tidymodels, ranger, glmnet, zoo, fable, feasts, lubridate)

set.seed(1234)

#recursive forecast

recursive_forecast_multi <- function(ts_data, 
                                     start_train,   # e.g. c(2000,1)
                                     end_train,     # e.g. c(2012,12)
                                     model_funs,    # list of functions
                                     names = NULL,  # optional names for columns
                                     h = 1) {
  f <- frequency(ts_data)
  
  # index of end_train in the series
  end_idx <- which(floor(time(ts_data)) == end_train[1] & 
                     cycle(ts_data) == end_train[2])[1]
  H <- length(ts_data) - end_idx
  
  # default column names
  if (is.null(names)) {
    names <- sapply(model_funs, function(fn) deparse(substitute(fn)))
  }
  
  pred <- matrix(NA, nrow = H, ncol = 1 + length(model_funs))
  colnames(pred) <- c("Actual", names)
  
  for (i in 1:H) {
    ei <- end_idx + (i - 1)
    train_i <- window(ts_data, start = start_train, end = time(ts_data)[ei])
    
    if ((ei + h) <= length(ts_data)) {
      actual <- ts_data[ei + h]
      pred[i, "Actual"] <- actual
      
      for (j in seq_along(model_funs)) {
        fit <- model_funs[[j]](train_i)
        fc  <- forecast(fit, h = h)$mean[h]
        pred[i, names[j]] <- fc
      }
    }
  }
  
  pred <- pred[!is.na(pred[, "Actual"]), , drop = FALSE]
  return(as.data.frame(pred))
}




#adaptive window function


# Greedy dynamic-window NNAR (fixed architecture p,P,size; window length adapts)
# - ts_data:       univariate ts (log or level)
# - start_train, end_train: c(year, month) for initial training end
# - p, P, size:    NNAR architecture (fixed features)
# - repeats:       bagging for stability
# - h:             forecast horizon (default 1)
# - min_years:     minimum window length in YEARS (e.g., 3 -> 36 months)
# - step_years:    candidate window grid step in YEARS (1 = try 3y,4y,5y,...)
# - max_years:     optional cap on window length in YEARS (NULL = no cap)
# - lambda:        set to 0 if series is in LEVELS and you want internal log + bias adj
#                  leave NULL if series is already logged
# - name:          forecast column name
dynamic_nnar_greedy <- function(ts_data,
                                start_train, end_train,
                                p, P, size, repeats = 20,
                                h = 1,
                                min_years = 3,
                                step_years = 1,
                                max_years = NULL,
                                lambda = NULL,
                                name = "NNAR_dyn") {
  f <- stats::frequency(ts_data)
  
  idx_start_all <- which(floor(stats::time(ts_data)) == start_train[1] &
                           stats::cycle(ts_data) == start_train[2])[1]
  if (is.na(idx_start_all)) idx_start_all <- 1L
  
  idx_end0 <- which(floor(stats::time(ts_data)) == end_train[1] &
                      stats::cycle(ts_data) == end_train[2])[1]
  if (is.na(idx_end0)) stop("end_train not found in series.")
  
  H <- length(ts_data) - idx_end0
  if (H <= 0) stop("Nothing to forecast after end_train.")
  
  req_min <- max(p + P * f + h + 1, min_years * f)
  step_obs <- max(1L, round(step_years * f))
  max_obs_cap <- if (is.null(max_years)) Inf else round(max_years * f)
  
  Actual <- numeric(0); Forecast <- numeric(0)
  WinObs <- integer(0); TimeIdx <- numeric(0)
  prev_best_win <- NA_integer_
  
  for (i in 1:H) {
    ei <- idx_end0 + (i - 1)
    if ((ei + h) > length(ts_data)) break
    
    L_i <- ei - idx_start_all + 1
    if (L_i < req_min) next
    
    max_obs_i <- min(L_i, max_obs_cap)
    cand_wins <- seq(from = req_min, to = max_obs_i, by = step_obs)
    if (length(cand_wins) == 0L) next
    
    fc_i <- rep(NA_real_, length(cand_wins))
    for (j in seq_along(cand_wins)) {
      start_i <- ei - cand_wins[j] + 1
      train_i <- stats::ts(ts_data[start_i:ei],
                           start = stats::time(ts_data)[start_i],
                           frequency = f)
      fit <- forecast::nnetar(train_i, p = p, P = P, size = size,
                              repeats = repeats, lambda = lambda)
      fc_i[j] <- as.numeric(forecast::forecast(fit, h = h,
                                               biasadj = !is.null(lambda))$mean[h])
    }
    
    y_t   <- as.numeric(ts_data[ei + h])
    t_idx <- stats::time(ts_data)[ei + h]
    
    use_win <- if (is.na(prev_best_win)) max(cand_wins) else prev_best_win
    if (!(use_win %in% cand_wins)) use_win <- max(cand_wins[cand_wins <= use_win], na.rm = TRUE)
    sel_fc <- fc_i[which(cand_wins == use_win)[1]]
    
    Actual   <- c(Actual, y_t)
    Forecast <- c(Forecast, sel_fc)
    WinObs   <- c(WinObs, use_win)
    TimeIdx  <- c(TimeIdx, t_idx)
    
    prev_best_win <- cand_wins[which.min((y_t - fc_i)^2)]
  }
  
  # ---- fixed return block (no tidy-eval) ----
  out <- data.frame(
    time        = TimeIdx,
    Actual      = Actual,
    Forecast    = Forecast,
    WindowObs   = WinObs,
    WindowYears = WinObs / f,
    row.names = NULL
  )
  names(out)[names(out) == "Forecast"] <- name
  out
}
