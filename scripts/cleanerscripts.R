rm(list = ls())
setwd("/Users/test123/Library/Mobile Documents/com~apple~CloudDocs/Masters classes U of A 2/Econ 593/Mega Homework 2")
getwd()
library(pacman)
p_load(tidyverse, forecast, tsibble, gridExtra, timetk, tidymodels, ranger, glmnet, zoo, fable, feasts, lubridate)

set.seed(1234)

# Load and subset to Maryland
Total <- read_csv("state_unem (1).csv")

state_num <- floor(39 / 2) + 1
state.name[state_num]  # should be "Maryland"

Maryland <- Total %>% filter(state_name == "Maryland")
#save Maryland as a csv
write_csv(Maryland, "Maryland_unemployment_rate.csv")
# Plot raw unemployment rate
Maryland %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  labs(title = "Unemployment Rate in Maryland",
       x = "", y = "Unemployment Rate") +
  theme_minimal()
# Create a time series object
Maryland_ts <- ts(Maryland$value, frequency = 12, start = c(1980, 1))
Maryland_ts_log <- log(Maryland_ts)

maryland_df_log <- tibble(
  date = as.Date(as.yearmon(time(Maryland_ts_log))),  # convert to proper Date
  log_unemploy = as.numeric(Maryland_ts_log)
)






# Plot the time series
plot(Maryland_ts, main = "Unemployment Rate in Maryland", ylab = "Unemployment Rate", xlab = "Year")
plot(Maryland_ts_log, main = "Log of Unemployment Rate in Maryland", ylab = "Log Unemployment Rate", xlab = "Year")

# Create a tsibble object
maryland_tsibble_log <- Maryland %>%
  select(date, value) %>%
  rename(unemploy = value) %>%
  mutate(log_unemploy = log(unemploy)) %>%
  as_tsibble(index = date)

maryland_tsibble<- Maryland %>%
  select(date, value) %>%
  rename(unemploy = value) %>%
  as_tsibble(index = date)

maryland_df_monthly <- maryland_df_log %>%
  mutate(month = yearmonth(date))


#untransformed

# Convert to a proper monthly tsibble
maryland_tsibble_log <- maryland_df_monthly %>%
  as_tsibble(index = month, regular = TRUE) %>%
  fill_gaps()














# Plot the log unemployment rate



maryland_tsibble_log %>%
  ggplot(aes(x = date, y = log_unemploy)) +
  geom_line() +
  labs(title = "Log Unemployment Rate in Maryland",
       x = "Date", y = "Log Unemployment Rate") +
  theme_minimal()

#plot acf and pcf of log unemployment rate and normal
P_1<-ggAcf(maryland_tsibble$unemploy)
P_2<-ggPacf(maryland_tsibble$unemploy)

gridExtra::grid.arrange(P_1, P_2, ncol = 1)
#plot acf and pcf of log unemployment rate and normal
P_3<-ggAcf(maryland_tsibble_log$log_unemploy)
P_4<-ggPacf(maryland_tsibble_log$log_unemploy)
gridExtra::grid.arrange(P_3, P_4, ncol = 1)
#now with differences
# Create a new column for the first difference of the unemployment rate
maryland_tsibble_log <- maryland_tsibble_log %>%
  mutate(diff_log_unemploy = log_unemploy - lag(log_unemploy, 1)) %>%
  filter(!is.na(diff_log_unemploy))

maryland_tsibble<- maryland_tsibble %>%
  mutate(diff_unemploy = unemploy - lag(unemploy, 1)) %>%
  filter(!is.na(diff_unemploy))
# Plot the first difference of the log unemployment rate
maryland_tsibble_log %>%
  ggplot(aes(x = date, y = diff_log_unemploy)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "First Difference of Log Unemployment Rate in Maryland",
       x = "Date", y = "First Difference of Log Unemployment Rate") +
  theme_minimal()

maryland_tsibble %>%
  ggplot(aes(x = date, y = diff_unemploy)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "First Difference of Unemployment Rate in Maryland",
       x = "Date", y = "First Difference of Unemployment Rate") +
  theme_minimal()
# Plot ACF and PACF of the first difference of the log unemployment rate
P_5<-ggAcf(maryland_tsibble_log$diff_log_unemploy)
P_6<-ggPacf(maryland_tsibble_log$diff_log_unemploy)
gridExtra::grid.arrange(P_3,P_4,P_5, P_6, ncol = 1)

#decomp
decomp<- maryland_tsibble_log %>%
  model(stl = STL(log_unemploy))
components(decomp)
components(decomp) %>%
  autoplot() +
  labs(
    y = "Log Unemployment Rate",
    title = "STL Decomposition of Maryland Unemployment (log)"
  )

#decomp difference
decomp_diff<- maryland_tsibble_log %>%
  model(stl = STL(diff_log_unemploy))
components(decomp_diff)
components(decomp_diff) %>%
  autoplot() +
  labs(
    y = "Log Unemployment Rate",
    title = "STL Decomposition of Maryland Unemployment (log)"
  )

#seasonally differenced
maryland_tsibble_log %>% 
  gg_tsdisplay(difference(log_unemploy, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

#double differenced
maryland_tsibble_log %>% 
  gg_tsdisplay(difference(log_unemploy, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title="Double differenced", y="")

fit <- maryland_tsibble_log %>% 
  model(
    arima011011 = ARIMA(log_unemploy ~ pdq(0,1,1) + PDQ(0,1,1)),
    arima110011 = ARIMA(log_unemploy ~ pdq(1,1,0) + PDQ(0,1,1)),
    arima011110 = ARIMA(log_unemploy ~ pdq(0,1,1) + PDQ(1,1,0)),
    arima110110 = ARIMA(log_unemploy ~ pdq(1,1,0) + PDQ(1,1,0)),
    auto = ARIMA(log_unemploy, stepwise = FALSE, approx = FALSE)
  )
fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")
glance(fit) |> arrange(AICc) |> select(.model:BIC)

fit %>%
  select(arima011011) %>%
  gg_tsresiduals() +
  labs(title = "Residuals of ARIMA(011011) Models")
#best is arima011011
#plot best model with actual data
maryland_tsibble_log %>%
  model(arima011011 = ARIMA(log_unemploy ~ pdq(0,1,1) + PDQ(0,1,1))) %>%
  forecast(h = "2 years") %>%
  autoplot(maryland_tsibble_log) +
  labs(title = "Forecast of Log Unemployment Rate in Maryland",
       x = "Date", y = "Log Unemployment Rate") +
  theme_minimal()
#best model
augment(fit) |>
  filter(.model == "arima011011") |>
  features(.innov, ljung_box, lag=24, dof=2)
#auto model
augment(fit) |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag=24, dof=4)

#now testing with the full loop
# data and set up
unemployment <- window(maryland_tsibble_log, start = 1980, end = c(2009,4))
n.end <- 2004.75 # 2004Q4
# set matrix for storage, 20 obs in test set
pred <- matrix(rep(NA,80),20,4)
# loop
for(i in 1:20){
  tmp0 <- 1992
  tmp1 <- n.end+(i-1)*.25
  tmp <- window(beer1, tmp0, tmp1)
  pred[i,1] <- window(beer1, tmp1+.25, tmp1+.25) # actual
  # compute forecasts
  pred[i,2] <- meanf(tmp, h=1)$mean
  pred[i,3] <- rwf(tmp, h=1)$mean
  pred[i,4] <- snaive(tmp, h=1)$mean
}



## ---- training and test sets ----
Maryland_ts_total <- ts(Maryland$value, frequency = 12, start = c(1980, 1))
Maryland_ts_train <- ts(Maryland$value, frequency = 12, start = c(1980, 1), end = c(2012, 12))
Maryland_ts_log_train <- log(Maryland_ts_train)
Maryland_ts_log <- log(Maryland_ts_total)

# 
future_data <- new_data(maryland_train, n = 144)  # 12 years * 12 months

#training set
maryland_df_train <- tibble(
  date = as.Date(as.yearmon(time(Maryland_ts_train))),  # convert to proper Date
  unemploy = as.numeric(Maryland_ts_train)
)
maryland_df_monthly_train <- maryland_df_train %>%
  mutate(month = yearmonth(date))
#test set

# Convert to a proper monthly tsibble
maryland_train <- maryland_df_monthly_train %>%
  as_tsibble(index = month, regular = TRUE) %>%
  fill_gaps()
#create log unemployment rate
maryland_train <- maryland_train %>%
  mutate(log_unemploy = log(unemploy)) %>%
  filter(!is.na(log_unemploy))

# Plot the time series
plot(Maryland_ts_train, main = "Unemployment Rate in Maryland", ylab = "Unemployment Rate", xlab = "Year")
plot(Maryland_ts_log_train, main = "Log of Unemployment Rate in Maryland", ylab = "Log Unemployment Rate", xlab = "Year")

#maryland_train is my test set
#plot
maryland_train %>%
  ggplot(aes(x = date, y = log_unemploy)) +
  geom_line() +
  labs(title = "Log Unemployment Rate in Maryland",
       x = "Date", y = "Log Unemployment Rate") +
  theme_minimal() 
#plot acf and pcf of log unemployment rate and normal
P_1<-ggAcf(maryland_train$log_unemploy)
P_2<-ggPacf(maryland_train$log_unemploy)
gridExtra::grid.arrange(P_1, P_2, ncol = 1)
#now with differences
# Create a new column for the first difference of the unemployment rate
maryland_train <- maryland_train %>%
  mutate(diff_log_unemploy = log_unemploy - lag(log_unemploy, 1)) %>%
  filter(!is.na(diff_log_unemploy)) %>% 
  select(date, month, unemploy, log_unemploy, diff_log_unemploy)
# Plot the first difference of the log unemployment rate
maryland_train %>%
  ggplot(aes(x = date, y = diff_log_unemploy)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "First Difference of Log Unemployment Rate in Maryland",
       x = "Date", y = "First Difference of Log Unemployment Rate") +
  theme_minimal()
# Plot ACF and PACF of the first difference of the log unemployment rate
P_3<-ggAcf(maryland_train$diff_log_unemploy)
P_4<-ggPacf(maryland_train$diff_log_unemploy)
gridExtra::grid.arrange(P_3, P_4, ncol = 1)

#looking at the features
decomp<- maryland_train %>%
  model(stl = STL(log_unemploy))
components(decomp)
components(decomp) %>%
  autoplot() +
  labs(
    y = "Log Unemployment Rate",
    x = "",
    title = "STL Decomposition of Maryland Unemployment (log)"
  )
#seasonality
maryland_train %>% 
  gg_season(log_unemploy)
#seasonality on the difference
maryland_train %>% 
  gg_subseries(log_unemploy) +
  labs(
    y = "Log Unemployment Rate",
    title = "maryland log unemployment rate"
  )

# seasonality of the difference
maryland_train %>% 
  gg_season(diff_log_unemploy)
 
maryland_train %>% 
  gg_subseries(diff_log_unemploy) +
  labs(
    y = "Log Unemployment Rate",
    title = "maryland log unemployment rate"
  )


#decomp difference
decomp_diff<- maryland_train %>%
  model(stl = STL(diff_log_unemploy))
components(decomp_diff)
components(decomp_diff) %>%
  autoplot() +
  labs(
    y = "Log Unemployment Rate",
    x = "",
    title = "STL Decomposition of Maryland Unemployment (log)"
  )

#seasonally differenced
maryland_train %>% 
  gg_tsdisplay(
               plot_type='partial', lag=36) +
  labs(title="Maryland Unemployment rate", y="")

#double differenced
maryland_train %>% 
  gg_tsdisplay(difference(log_unemploy, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title="Double differenced(log_unemploy)", y="")

#transformation
lambda <- maryland_train %>% 
  features(unemploy, features = guerrero) %>% 
  pull(lambda_guerrero)
maryland_train %>% 
  autoplot(box_cox(unemploy, lambda)) 
lambda
#add bc transformation column
maryland_train <- maryland_train %>%
  mutate(bc_unemploy = box_cox(unemploy, lambda))

#decomp transformed
decomp_bc<- maryland_train %>%
  model(stl = STL(bc_unemploy))
components(decomp_bc)
components(decomp_bc) %>%
  autoplot() +
  labs(
    y = "bc Unemployment Rate",
    title = "STL Decomposition of Maryland Unemployment (bc)"
  )
#
maryland_train %>% 
  gg_tsdisplay(difference(bc_unemploy, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced(BC)", y="")

#double differenced
maryland_train %>% 
  gg_tsdisplay(difference(bc_unemploy, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title="Double differenced (BC)", y="")

#add lamda
maryland_train <- maryland_train %>%
  mutate(bc_unemploy = box_cox(unemploy, lambda))


#fit the models
fit <- maryland_train %>% 
  model(
    arima012011 = ARIMA(log_unemploy ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(log_unemploy ~ pdq(2,1,0) + PDQ(0,1,1)),
    arima012110 = ARIMA(log_unemploy ~ pdq(0,1,2) + PDQ(1,1,0)),
    arima210110 = ARIMA(log_unemploy ~ pdq(2,1,0) + PDQ(1,1,0)),
    AR2 = ARIMA(log_unemploy ~ pdq(2,1,0)),
    auto = ARIMA(log_unemploy, stepwise = FALSE, approx = FALSE)
  )
fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")
glance(fit) |> arrange(AICc) |> select(.model:BIC)

fit %>%
  select(auto) %>%
  gg_tsresiduals()
augment(fit) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag = 24, dof = 6) 

fit %>%
  select(auto) %>%
  report()




# adjust dof = # of parameters
#best is auto         <ARIMA(2,0,2)(0,1,2)[12]>

fit %>%
  select(AR2) %>%
  gg_tsresiduals()

fit %>%
  select(arima210011) %>%
  gg_tsresiduals()
## ---- plot fits ----
fitted_df <- augment(fit) 

ggplot(fitted_df, aes(x = month)) +
  geom_line(aes(y = log_unemploy), color = "black", size = 1, alpha = 0.6) +  # Actuals
  geom_line(aes(y = .fitted, color = .model), size = 1) +                    # Fitted
  labs(
    title = "Fitted Values from ARIMA Models on Training Data",
    subtitle = "Compared with Actual Log Unemployment",
    x = "Date", y = "Log Unemployment"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())






## ---- forecasts ----
# data and set up
#create tsibble object


maryland_df_log <- tibble(
  date = as.Date(as.yearmon(time(Maryland_ts_log))),  # convert to proper Date
  log_unemploy = as.numeric(Maryland_ts_log)
)
maryland_df_monthly <- maryland_df_log %>%
  mutate(month = yearmonth(date))

# Convert to a proper monthly tsibble
maryland_total <- maryland_df_monthly %>%
  as_tsibble(index = month, regular = TRUE) %>%
  fill_gaps()




unemploy1 <- ts(Maryland_ts_log, start = c(1980, 1), frequency = 12)

n.end <- 2012.9167  # 2012M12
n_forecast <- 144
pred <- matrix(NA, nrow = n_forecast, ncol = 4)
colnames(pred) <- c("Actual", "Auto", "arima210011", "AR2")

for (i in 1:n_forecast) {
  # Forecast origin in decimal form
  tmp1 <- n.end + (i - 1) / 12
  
  # Convert tmp1 to year-month for window()
  year_end <- floor(tmp1)
  month_end <- round(12 * (tmp1 - year_end)) + 1
  if (month_end > 12) {
    year_end <- year_end + 1
    month_end <- 1
  }
  
  # Training window: 1980 to current forecast origin
  train_ts <- window(unemploy1, start = c(1980, 1), end = c(year_end, month_end))
  
  # Forecast target: 1 step ahead
  forecast_target <- tmp1 + 1/12
  year_forecast <- floor(forecast_target)
  month_forecast <- round(12 * (forecast_target - year_forecast)) + 1
  if (month_forecast > 12) {
    year_forecast <- year_forecast + 1
    month_forecast <- 1
  }
  
  # Store actual
  actual_val <- try(window(unemploy1, start = c(year_forecast, month_forecast),
                           end   = c(year_forecast, month_forecast)), silent = TRUE)
  if (inherits(actual_val, "try-error")) next
  pred[i, 1] <- actual_val
  
  # Forecasts
  pred[i, 2] <- auto(train_ts, h = 1)$mean
  pred[i, 3] <- arima210011(train_ts, h = 1)$mean
  pred[i, 4] <- AR2(train_ts, h = 1)$mean
}
library(yardstick)

residuals_df <- as.data.frame(pred)
colnames(residuals_df) <- c("Actual", "Auto", "ARIMA210011", "AR2")

# Calculate residuals
residuals_df <- residuals_df %>%
  mutate(
    Resid_Auto = Actual - Auto,
    Resid_ARIMA210011 = Actual - ARIMA210011,
    Resid_AR2 = Actual - AR2
  )

#

forecasts <- forecast(fit, new_data = future_data)
forecasts <- as_fable(forecasts)

historical_df <- tibble(
  date = as.Date(as.yearmon(time(Maryland_ts_log))),
  value = as.numeric(Maryland_ts_log)
)

forecast_df <- forecasts %>%
  as_tibble() %>%
  select(.model, month, .mean) %>%
  rename(date = month, value = .mean)

combined_df <- bind_rows(
  historical_df %>% mutate(.model = "Actual"),
  forecast_df
)


#print rmse
actuals_df <- combined_df %>%
  filter(.model == "Actual") %>%
  select(date, actual_value = value)

# Step 2: Join actuals to each forecast
rmse_df <- combined_df %>%
  filter(.model != "Actual") %>%
  left_join(actuals_df, by = "date") %>%
  group_by(.model) %>%
  summarise(RMSE = rmse_vec(truth = actual_value, estimate = value))

print(rmse_df)
#Plot
ggplot(combined_df, aes(x = date, y = value, color = .model)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Unemployment Rate in Maryland with Forecasts",
    subtitle = "Actuals and ARIMA Forecasts",
    x = "Date", y = "Log Unemployment Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )



# Create a data frame for the predictions
pred_df <- data.frame(
  date = seq(as.Date("2013-01-01"), by = "month", length.out = n_forecast),
  Actual = pred[, 1],
  Mean = pred[, 2],
  RandomWalk = pred[, 3],
  SeasonalNaive = pred[, 4]
)
historical_df <- data.frame(
  date = as.Date(as.yearmon(time(Maryland_ts_log))),
  Actual = as.numeric(Maryland_ts_log)
) %>%
  filter(date < as.Date("2013-01-01"))

combined_df <- bind_rows(historical_df, pred_df)


# compute rmse
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}
# Calculate RMSE for each method
rmse_results <- data.frame(
  Method = c("Mean", "Random Walk", "Seasonal Naive"),
  RMSE = c(
    rmse(pred_df$Actual, pred_df$Mean),
    rmse(pred_df$Actual, pred_df$RandomWalk),
    rmse(pred_df$Actual, pred_df$SeasonalNaive)
  )
)
# Print RMSE results
print(rmse_results)
# Plot the forecasts
combined_df <- bind_rows(historical_df, pred_df)

combined_long <- combined_df %>%
  pivot_longer(cols = -date, names_to = "Method", values_to = "Value")

ggplot(combined_long, aes(x = date, y = Value, color = Method)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(
    values = c(
      "Actual" = "black",
      "Mean" = "blue",
      "RandomWalk" = "red",
      "SeasonalNaive" = "green"
    )
  ) +
  labs(
    title = "Unemployment Forecasts with Historical Data",
    x = "Date", y = "Log Unemployment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

#untransformed
combined_long_untransformed <- combined_df %>%
  pivot_longer(cols = -date, names_to = "Method", values_to = "Value") 

ggplot(combined_long_untransformed, aes(x = date, y = Value, color = Method)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(
    values = c(
      "Actual" = "black",
      "Mean" = "blue",
      "RandomWalk" = "red",
      "SeasonalNaive" = "green"
    )
  ) +
  labs(
    title = "Unemployment Forecasts with Historical Data (Untransformed)",
    x = "Date", y = "Unemployment Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )  

#
unemploy1 <- ts(Maryland_ts_log, start = c(1980, 1), frequency = 12)

n.end <- 2012.9167  # 2012M12
n_forecast <- 144
pred <- matrix(NA, nrow = n_forecast, ncol = 4)
colnames(pred) <- c("Actual", "Auto", "arima210011", "AR2")

for (i in 1:n_forecast) {
  tmp1 <- n.end + (i - 1) / 12
  
  # Convert to year/month
  year_end <- floor(tmp1)
  month_end <- round(12 * (tmp1 - year_end)) + 1
  if (month_end > 12) {
    year_end <- year_end + 1
    month_end <- 1
  }
  
  # Training window
  train_ts <- window(unemploy1, start = c(1980, 1), end = c(year_end, month_end))
  
  # Forecast target
  forecast_target <- tmp1 + 1/12
  year_forecast <- floor(forecast_target)
  month_forecast <- round(12 * (forecast_target - year_forecast)) + 1
  if (month_forecast > 12) {
    year_forecast <- year_forecast + 1
    month_forecast <- 1
  }
  
  # Store actual
  actual_val <- try(window(unemploy1, start = c(year_forecast, month_forecast),
                           end = c(year_forecast, month_forecast)), silent = TRUE)
  if (inherits(actual_val, "try-error")) next
  pred[i, 1] <- actual_val
  
  # Forecasts
  pred[i, 2] <- forecast(Arima(train_ts, order = c(2, 0, 2), seasonal = c(0, 1, 2)), h = 1)$mean
  pred[i, 3] <- forecast(Arima(train_ts, order = c(2, 1, 0), seasonal = c(0, 1, 1)), h = 1)$mean
  pred[i, 4] <- forecast(Arima(train_ts, order = c(2, 1, 0)), h = 1)$mean
}
#check residuals
residuals_df <- pred_df %>%
  mutate(
    resid_auto        = Actual - Auto,
    resid_arima210011 = Actual - arima210011,
    resid_ar2         = Actual - AR2
  )

residuals_df %>%
  summarise(
    RMSE_auto        = sqrt(mean(resid_auto^2, na.rm = TRUE)),
    RMSE_arima210011 = sqrt(mean(resid_arima210011^2, na.rm = TRUE)),
    RMSE_ar2         = sqrt(mean(resid_ar2^2, na.rm = TRUE))
  )
#
residuals_df %>%
  mutate(horizon = row_number()) %>%
  pivot_longer(cols = starts_with("resid_"), names_to = "model", values_to = "residual") %>%
  ggplot(aes(x = horizon, y = residual, color = model)) +
  geom_line() +
  labs(title = "Time Plot of Residuals", x = "Forecast Horizon", y = "Residual")

res_1 <- Acf(residuals_df$resid_ar2, main = "ACF: AR2", plot = FALSE)
res_2 <- Acf(residuals_df$resid_auto, main = "ACF: Auto", plot = FALSE)
res_3 <- Acf(residuals_df$resid_arima210011, main = "ACF: ARIMA(2,1,0)(0,1,1)[12]", plot = FALSE)

# 
p1 <- autoplot(res_1) + ggtitle("ACF: AR2")
p2 <- autoplot(res_2) + ggtitle("ACF: Auto")
p3 <- autoplot(res_3) + ggtitle("ACF: ARIMA(2,1,0)(0,1,1)[12]")







# Arrange them
gridExtra::grid.arrange(p1, p2, p3, ncol = 1)

pred
pred_df <- as.data.frame(pred)
pred_df$date <- as.yearmon("2013-01") + 0:(n_forecast - 1)
pred_df <- data.frame(
  date = seq(as.Date("2013-01-01"), by = "month", length.out = n_forecast),
  Actual = pred[, 1],
  Auto202012 = pred[, 2],
  arima210011 = pred[, 3],
  AR2 = pred[, 4]
)
historical_df <- data.frame(
  date = as.Date(as.yearmon(time(Maryland_ts_log))),
  Actual = as.numeric(Maryland_ts_log)
) %>%
  filter(date < as.Date("2013-01-01"))

combined_df <- bind_rows(historical_df, pred_df)
# compute rmse
forecast_only <- pred_df %>%
  pivot_longer(cols = -c(date, Actual), names_to = "Method", values_to = "Forecast") %>%
  drop_na()  # in case any rows are missing

forecast_only_back <- forecast_only %>%
  mutate(
    Forecast = exp(Forecast),
    Actual = exp(Actual)
  )

# Step 2: Calculate RMSE per method
rmse_results <- forecast_only %>%
  group_by(Method) %>%
  summarise(RMSE = rmse_vec(truth = Actual, estimate = Forecast))

print(rmse_results)

rmse_results_ <- forecast_only_back %>%
  group_by(Method) %>%
  summarise(RMSE = rmse_vec(truth = Actual, estimate = Forecast))

print(rmse_results_)
# create table of the RMSE results
library(dplyr)
library(knitr)
library(kableExtra)
rmse_table <- rmse_results_ %>%
  mutate(
    RMSE = round(RMSE, 4),
    Method = recode(Method,
                    "Auto" = "Auto ARIMA",
                    "arima210011" = "ARIMA(2,1,0)(0,1,1)[12]",
                    "AR2" = "ARIMA(2,1,0)")
  )

rmse_table <- rmse_results_ %>%
  mutate(
    RMSE = round(RMSE, 4),
    Method = recode(Method,
                    "Auto" = "Auto ARIMA",
                    "arima210011" = "ARIMA(2,1,0)(0,1,1)[12]",
                    "AR2" = "AR2")
  ) %>%
  select(Method, RMSE)

# Step 2: Create pretty table using kableExtra
table <- rmse_table %>%
  kbl(
    caption = "RMSE for One-Step Ahead Forecasts",
    col.names = c("Model", "RMSE"),
    format = "html",     # Use "latex" if knitting to PDF
    booktabs = TRUE
  ) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, width = "4cm")  # Only columns 1 and 2 exist

# Show the table
table



plot_df <- combined_df %>%
  pivot_longer(cols = -date, names_to = "Method", values_to = "Value") %>%
  mutate(real = exp(Value))  # back-transform




# Plot the forecasts
ggplot() +
  # Actual line in black
  geom_line(
    data = filter(plot_df, Method == "Actual"),
    aes(x = date, y = real),
    color = "black", linewidth = 0.5
  ) +
  
  # Forecasts using default color scale
  geom_line(
    data = filter(plot_df, Method != "Actual"),
    aes(x = date, y = real, color = Method),
    linewidth = 0.3
  ) +
  
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", color = "gray50") +
  
  labs(
    title = "Unemployment in Maryland with Forecasts",
    subtitle = "Rolling Forecasts from 2013 to 2024",
    x = "Year", y = "Unemployment Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

## ---- 4 step ahead ----

unemploy1 <- ts(Maryland_ts_log, start = c(1980, 1), frequency = 12)

start_month <- as.yearmon("2013-04")
end_month   <- as.yearmon("2024-12")
n_forecast <- 12 * (2024 - 2013) + (12 - 4 + 1)  # 140 months

# Step 2: Initialize result matrix
pred_4 <- matrix(NA, nrow = n_forecast, ncol = 4)
colnames(pred_4) <- c("Actual", "Auto", "arima210011", "AR2")

# Step 3: Loop with adjusted training window
n.end <- 2012.9167  # December 2012 (in decimal)
for (i in 1:n_forecast) {
  tmp1 <- n.end + (i - 1) / 12  # forecast origin: Dec 2012 + i months
  
  # Convert to (year, month)
  year_end <- floor(tmp1)
  month_end <- round(12 * (tmp1 - year_end)) + 1
  if (month_end > 12) {
    year_end <- year_end + 1
    month_end <- 1
  }
  
  # Training window: Jan 1980 to tmp1
  train_ts <- window(unemploy1, start = c(1980, 1), end = c(year_end, month_end))
  
  # Forecast target: 4 steps ahead
  forecast_target <- tmp1 + 4 / 12
  year_forecast <- floor(forecast_target)
  month_forecast <- round(12 * (forecast_target - year_forecast)) + 1
  if (month_forecast > 12) {
    year_forecast <- year_forecast + 1
    month_forecast <- 1
  }
  
  # Store actual
  actual_val <- try(window(unemploy1, start = c(year_forecast, month_forecast),
                           end = c(year_forecast, month_forecast)), silent = TRUE)
  if (inherits(actual_val, "try-error")) next
  pred_4[i, 1] <- actual_val
  
  # Forecasts (extract only the 4th horizon)
  pred_4[i, 2] <- forecast(Arima(train_ts, order = c(2, 0, 2), seasonal = c(0, 1, 2)), h = 4)$mean[4]
  pred_4[i, 3] <- forecast(Arima(train_ts, order = c(2,1,0), seasonal = c(0,1,1)), h = 4)$mean[4]
  pred_4[i, 4] <- forecast(Arima(train_ts, order = c(2,1,0)), h = 4)$mean[4]
}
pred_4

pred_4_df <- as.data.frame(pred_4)

# Add yearmon-based time index
pred_4_df$date <- as.yearmon("2013-04") + 0:(n_forecast - 1)

# Replace with actual Date column
pred_4_df <- data.frame(
  date = seq(as.Date("2013-04-01"), by = "month", length.out = n_forecast),
  Actual = pred_4[, 1],
  Auto = pred_4[, 2],
  arima210011 = pred_4[, 3],
  AR2 = pred_4[, 4]
)

# Step 2: Compute RMSEs
forecast_4_long <- pred_4_df %>%
  pivot_longer(cols = -c(date, Actual), names_to = "Method", values_to = "Forecast") %>%
  drop_na()
forecast_4_wide <- forecast_4_long %>%
  pivot_wider(names_from = Method, values_from = Forecast) %>% 
  mutate(
    Actual = exp(Actual),
    arima210011 = exp(arima210011),
    Auto = exp(Auto),
    AR2 = exp(AR2)
  )

# RMSE in log scale
rmse_4_log <- forecast_4_long %>%
  group_by(Method) %>%
  summarise(RMSE = rmse_vec(truth = Actual, estimate = Forecast))

# RMSE in real scale (exp-transformed)
rmse_4_real <- forecast_4_long %>%
  mutate(
    Actual = exp(Actual),
    Forecast = exp(Forecast)
  ) %>%
  group_by(Method) %>%
  summarise(RMSE = rmse_vec(truth = Actual, estimate = Forecast))

rmse_table <- forecast_4_long %>%
  mutate(
    Actual = exp(Actual),
    Forecast = exp(Forecast)
  ) %>%
  group_by(Method) %>%
  summarise(RMSE = rmse_vec(truth = Actual, estimate = Forecast)) %>%
  arrange(RMSE)




print(rmse_4_log)
print(rmse_4_real)

table <- rmse_table %>%
  mutate(RMSE = round(RMSE, 2)) %>%
  kbl(
    caption = "RMSE for 4-Step Ahead Forecasts",
    col.names = c("Model", "RMSE"),
    format = "html",       # Change to "latex" if knitting to PDF
    booktabs = TRUE
  ) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, width = "4cm")

table




rmse_labels <- rmse_4_real %>%
  mutate(label = paste0(Method, ": RMSE = ", round(RMSE, 2)))


# Step 3: Prepare data for plotting
historical_df <- data.frame(
  date = as.Date(as.yearmon(time(Maryland_ts_log))),
  Actual = as.numeric(Maryland_ts_log)
) %>%
  filter(date < as.Date("2013-04-01"))

combined_4_df <- bind_rows(historical_df, pred_4_df)

plot_4_df <- combined_4_df %>%
  pivot_longer(cols = -date, names_to = "Method", values_to = "Value") %>%
  mutate(real = exp(Value))  # back-transform

#
ggplot(forecast_4_wide, aes(x = date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 0.5, alpha = 1) +  # Actuals with label
  geom_line(aes(y = arima210011, color = "arima210011"), size = 0.5) +
  geom_line(aes(y = AR2, color = "AR2"), size = 0.5) +
  geom_line(aes(y = Auto, color = "Auto"), size = 0.5) +
  labs(
    title = "4 steps ahead",
    x = "", y = "Unemployment Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"  # Moves legend below the plot
  )









# Step 4: Plot
ggplot() +
  geom_line(
    data = filter(plot_4_df, Method == "Actual"),
    aes(x = date, y = real),
    color = "black", linewidth = 0.5
  ) +
  geom_line(
    data = filter(plot_4_df, Method != "Actual"),
    aes(x = date, y = real, color = Method),
    linewidth = 0.3
  ) +
  geom_vline(xintercept = as.Date("2013-04-01"), linetype = "dashed", color = "gray50") +
  labs(
    title = "4-Step-Ahead Forecasts of Unemployment in Maryland",
    subtitle = "Rolling Forecasts from 2013 to 2024 (4-Month Horizon)",
    x = "Year", y = "Unemployment Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )
## ---- Machine learning ----

#neural networks
maryland_total <- maryland_total %>% 
  mutate(
    value = exp(log_unemploy),
    diff_log = c(NA, diff(log_unemploy))  # prepend NA to match row count
  ) %>%
  select(date, month, value, log_unemploy, diff_log)

fit <- maryland_total %>% 
  model(NNETAR(value))
forecasts_NN<-fit %>% 
  forecast(h = 60)  

forecasts_NN %>% 
  autoplot(maryland_total) +
  labs(x = "Year", y = "unemployment rate", title = "Maryland unemployment rate")



adaptive_nnetar_forecast1 <- function(ts_data, 
                                     forecast_start = c(2013, 1), 
                                     forecast_end   = c(2024, 12),
                                     min_window     = 36,
                                     max_window     = 120) {
  
  start_decimal <- forecast_start[1] + (forecast_start[2] - 1) / 12
  end_decimal   <- forecast_end[1]   + (forecast_end[2] - 1) / 12
  n_forecast <- round((end_decimal - start_decimal) * 12) + 1
  
  pred <- matrix(NA, nrow = n_forecast, ncol = 3)
  colnames(pred) <- c("Actual", "NNETAR_Forecast", "Window_Size")
  forecast_dates <- rep(NA, n_forecast)
  
  for (i in 1:n_forecast) {
    tmp1 <- start_decimal + (i - 1) / 12
    forecast_target <- tmp1 + 1/12
    if (forecast_target > time(ts_data)[length(ts_data)]) next
    forecast_dates[i] <- forecast_target
    
    # Check window sizes in 12-month increments
    candidate_windows <- seq(min_window, max_window, by = 12)
    candidate_errors <- rep(NA, length(candidate_windows))
    
    for (j in seq_along(candidate_windows)) {
      L <- candidate_windows[j]
      candidate_start_decimal <- tmp1 - L / 12
      
      # Define holdout origin
      holdout_origin <- tmp1 - 1/12
      year_hold <- floor(holdout_origin)
      month_hold <- round(12 * (holdout_origin - year_hold)) + 1
      if (month_hold > 12) {
        year_hold <- year_hold + 1
        month_hold <- 1
      }
      
      # Candidate training window
      candidate_train <- try(window(ts_data, 
                                    start = c(floor(candidate_start_decimal), 
                                              round(12 * (candidate_start_decimal - floor(candidate_start_decimal)) + 1)),
                                    end = c(year_hold, month_hold)),
                             silent = TRUE)
      if (inherits(candidate_train, "try-error") || length(candidate_train) < (min_window - 1)) {
        candidate_errors[j] <- Inf
        next
      }
      
      fc_candidate <- try(forecast(nnetar(candidate_train), h = 1), silent = TRUE)
      if (inherits(fc_candidate, "try-error")) {
        candidate_errors[j] <- Inf
        next
      }
      
      actual_hold <- try(window(ts_data, 
                                start = c(floor(tmp1), round(12 * (tmp1 - floor(tmp1)) + 1)),
                                end   = c(floor(tmp1), round(12 * (tmp1 - floor(tmp1)) + 1))),
                         silent = TRUE)
      if (inherits(actual_hold, "try-error")) {
        candidate_errors[j] <- Inf
        next
      }
      
      # Squared error (RMSE criterion)
      candidate_errors[j] <- (fc_candidate$mean[1] - actual_hold)^2
    }
    
    best_idx <- which.min(candidate_errors)
    best_window_length <- if (is.infinite(candidate_errors[best_idx])) min_window else candidate_windows[best_idx]
    
    # Define best training window
    best_window_start_decimal <- tmp1 - best_window_length / 12
    best_window_start_year <- floor(best_window_start_decimal)
    best_window_start_month <- round(12 * (best_window_start_decimal - best_window_start_year)) + 1
    if (best_window_start_month > 12) {
      best_window_start_year <- best_window_start_year + 1
      best_window_start_month <- 1
    }
    best_window_start <- c(best_window_start_year, best_window_start_month)
    
    # Refit final model
    train_ts <- try(window(ts_data, start = best_window_start,
                           end = c(floor(tmp1), round(12 * (tmp1 - floor(tmp1)) + 1))),
                    silent = TRUE)
    if (inherits(train_ts, "try-error") || length(train_ts) < min_window) next
    
    nn_model <- try(nnetar(train_ts), silent = TRUE)
    if (inherits(nn_model, "try-error")) next
    fc_final <- try(forecast(nn_model, h = 1), silent = TRUE)
    if (inherits(fc_final, "try-error")) next
    
    # Get actual value at forecast time
    year_final <- floor(forecast_target)
    month_final <- round(12 * (forecast_target - year_final)) + 1
    if (month_final > 12) {
      year_final <- year_final + 1
      month_final <- 1
    }
    actual_val <- try(window(ts_data, start = c(year_final, month_final),
                             end = c(year_final, month_final)),
                      silent = TRUE)
    if (inherits(actual_val, "try-error")) next
    
    pred[i, "Actual"] <- actual_val
    pred[i, "NNETAR_Forecast"] <- fc_final$mean[1]
    pred[i, "Window_Size"] <- length(train_ts)
  }
  
  valid <- !is.na(forecast_dates)
  pred <- pred[valid, , drop = FALSE]
  forecast_dates <- forecast_dates[valid]
  final_dates <- as.Date(as.yearmon(forecast_dates))
  
  pred_df <- as.data.frame(pred)
  pred_df$date <- final_dates
  return(pred_df)
}

start_time <- Sys.time()

adaptive_forecasts1 <- adaptive_nnetar_forecast1(unemploy_ts,
                                               forecast_start = c(2013, 1),
                                               forecast_end   = c(2024, 12),
                                               min_window     = 36,
                                               max_window     = 540)
end_time <- Sys.time()

print(end_time - start_time)
print(head(adaptive_forecasts1))
# Calculate RMSE for the adaptive NNETAR model

adaptive_forecasts1<-adaptive_forecasts1 %>%
  mutate(
    real = exp(Actual),
    real_Forecast = exp(NNETAR_Forecast)
  )
library(yardstick)

adaptive_rmse1 <- rmse_vec(truth = adaptive_forecasts1$Actual, estimate = adaptive_forecasts1$NNETAR_Forecast)
print(paste("Adaptive NNETAR RMSE:", adaptive_rmse))
# Plot the adaptive NNETAR forecasts
ggplot(adaptive_forecasts, aes(x = date)) +
  geom_line(aes(y = Actual), color = "black", size = 0.5, alpha = 0.5) +  # Actuals
  geom_line(aes(y = NNETAR_Forecast, color = "Adaptive NNETAR"), size = 0.5) +  # Fitted
  labs(
    title = "Adaptive NNETAR Forecasts of Unemployment in Maryland",
    x = "Date", y = "Log Unemployment Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())


adaptive_forecasts1 <- adaptive_forecasts1 %>%
  mutate(
    real = exp(Actual),
    real_Forecast = exp(NNETAR_Forecast)
  )

# Compute RMSE
adaptive_rmse <- rmse_vec(truth = adaptive_forecasts1$real, estimate = adaptive_forecasts1$real_Forecast)
print(paste("Adaptive NNETAR RMSE:", adaptive_rmse))
table <- adaptive_rmse

# Add Adaptive NNETAR to RMSE table
rmse_table <- bind_rows(
  rmse_table,
  tibble(
    Method = "Adaptive NNETAR",
    RMSE = adaptive_rmse
  )
)

# Generate styled table
table <- rmse_table %>%
  kbl(
    caption = "RMSE for One-Step Ahead Forecasts",
    col.names = c("Model", "RMSE"),
    format = "html",     # use "latex" if knitting to PDF
    booktabs = TRUE
  ) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, width = "4cm")

print(table)

# Clean and recode RMSE table
rmse_table <- rmse_table %>%
  mutate(
    RMSE = round(RMSE, 4),
    Method = recode(Method,
                    "Auto202012" = "Auto ARIMA",
                    "arima210011" = "ARIMA(2,1,0)(0,1,1)[12]",
                    "AR2" = "ARIMA(2,1,0)",
                    "Adaptive NNETAR" = "Adaptive NNETAR")
  ) %>%
  select(Method, RMSE) %>%
  filter(!is.na(Method)) %>%
  distinct(Method, .keep_all = TRUE)

# Regenerate table
table <- rmse_table %>%
  kbl(
    caption = "RMSE for One-Step Ahead Forecasts",
    col.names = c("Model", "RMSE"),
    format = "html",
    booktabs = TRUE
  ) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, width = "4cm")
table
# Combine forecasts for visualization
forecast_only_back_wide <- forecast_only_back %>%
  pivot_wider(names_from = Method, values_from = Forecast)

merged_df <- left_join(forecast_only_back_wide, adaptive_forecasts1, by = "date") %>%
  rename(
    NNETAR_Forecast_log = NNETAR_Forecast,
    adaptive_NNETAR = real_Forecast
  )

# Plot: Adaptive NNETAR only
ggplot(adaptive_forecasts1, aes(x = date)) +
  geom_line(aes(y = real), color = "black", size = 0.5, alpha = 0.5) +
  geom_line(aes(y = real_Forecast, color = "Adaptive NNETAR"), size = 0.5) +
  labs(
    title = "Adaptive NNETAR Forecasts of Unemployment in Maryland",
    x = "", y = "Unemployment Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank(), legend.position = "bottom")

# Plot: All Forecasts
ggplot(merged_df, aes(x = date)) +
  geom_line(aes(y = Actual.x), color = "black", size = 0.5, alpha = 1) +
  geom_line(aes(y = adaptive_NNETAR, color = "Adaptive NNETAR"), size = 0.5) +
  geom_line(aes(y = Auto202012, color = "Auto ARIMA"), size = 0.5) +
  geom_line(aes(y = arima210011, color = "ARIMA(2,1,0)(0,1,1)[12]"), size = 0.5) +
  geom_line(aes(y = AR2, color = "AR2"), size = 0.5) +
  labs(
    title = "Adaptive NNETAR Forecasts of Unemployment in Maryland",
    x = "", y = "Unemployment Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank(), legend.position = "bottom")
write.csv(adaptive_forecasts1, "/Users/luke/Desktop/Masters classes U of A/Econ 593/Mega Homework 2/adaptive_forecast1.csv", row.names = FALSE)
