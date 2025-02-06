
library(RcppRoll)

glimpse(df1)

check <- count(df1, naics, fnaics, date)

uname <- "New York State"

uname <- "New York City"

stsum <- df1 |> 
  filter(uniname==uname) |> 
  summarise(n=n(),
            txblsales=sum(txblsales, na.rm = TRUE),
            .by=c(stdate, uniname)) |> 
  mutate(ind="all") |> 
  arrange(stdate)

inds <- df1 |> 
  filter(uniname==uname, naics %in% c(4541, 9999)) |> 
  summarise(txblsales=sum(txblsales, na.rm = TRUE),
            .by=c(stdate, uniname, naics)) |> 
  mutate(ind=paste0("i", naics)) |> 
  select(-naics) |> 
  arrange(ind, stdate)

combo <- bind_rows(stsum, inds) |> 
  select(-n) |> 
  arrange(stdate) |> 
  mutate(txblsales = txblsales / 1e6) |> 
  pivot_wider(names_from=ind, values_from=txblsales) |>
  mutate(ishare = i4541 / all,
         isharema4 = RcppRoll::roll_mean(ishare, n=4,  align="right", fill=NA),
         i4541 = ifelse(stdate > "2021-12-01" & stdate <= "2022-12-01", 
                        all * isharema4[stdate == "2021-12-01"],
                        i4541),
         i4541=ifelse(stdate > "2022-12-01", lag(i4541, 4) * i9999 / lag(i9999, 4), i4541)) |>
  mutate(i4541=ifelse(stdate > "2022-12-01", lag(i4541, 4) * i9999 / lag(i9999, 4), i4541)) |> 
  mutate(across(c(all, i4541, i9999), list(pch = \(x) x / lag(x, 4) - 1)))

# combo |> 
#   select(stdate, all_pch, i4541_pch) |> 
#   pivot_longer(cols=cols=-stdate) |>
#   ggplot(aes(stdate, value, color=name)) +
#   geom_line()

lsize <- 1.25
combo |> 
  select(stdate, all, i4541) |> 
  pivot_longer(cols=-stdate) |>
  ggplot(aes(stdate, value)) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 1.1) +
  geom_smooth(method="loess", se=FALSE) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous() +
  facet_wrap(~name, scales="free_y", nrow=2) +
  ggtitle(uname) +
  geom_vline(xintercept = as.Date("2019-01-15"), linetype="dashed", colour="blue", linewidth = lsize) +
  geom_vline(xintercept = as.Date("2019-06-01"), linetype="dashed", colour="green", linewidth = lsize) +
  geom_vline(xintercept = as.Date("2020-03-019"), linetype="dashed", colour="red", linewidth = lsize) +
  theme_minimal()




tmp <- df1 |>
  filter(naics==4541) 
  # filter(naics==4541, uniname=="New York City") 

summary(df1)
summary(tmp)
tmp <- count(df1, naics, fnaics)


df1 |>
  filter(naics==4541, uniname=="New York State") |> 
  arrange(stdate) |> 
  select(stdate, uniname, naics, txblsales) |> 
  mutate(txblsales=txblsales / 1e6,
         tsma4=roll_sum(txblsales, 4, align="right", fill=NA),
         pchya=tsma4 / lag(tsma4, 4) - 1)





# arima example -----------------------------------------------------------
library(tidyverse)
library(tsibble)
library(fable)
library(lubridate)

# Create random quarterly data with trend and seasonality
set.seed(123)
wages_tbl <- tibble(
  date = yearquarter(seq(ymd("2010-01-01"), ymd("2024-10-01"), by = "quarter")),
  wages = exp(cumsum(rnorm(length(date), mean = 0.02, sd = 0.1)) + 
                sin(seq(0, 4*pi, length.out = length(date)))) + rnorm(length(date), sd = 0.2)
)

# Convert to tsibble
wages_ts <- wages_tbl %>% 
  as_tsibble(index = date)

# Fit ARIMA with log transformation and automatic order selection
arima_fit1 <- wages_ts %>% 
  model(ARIMA(log(wages) ~ pdq() + PDQ()))
report(arima_fit1)

# Generate 4-year forecast and back-transform
forecast_results1 <- arima_fit1 %>% 
  forecast(h = 16) %>% 
  mutate(wages2 = exp(.mean)) %>%  # Proper back-transformation
  as_tibble()

# Combine with original data
full_data1 <- wages_tbl %>% 
  mutate(type = "actual") %>% 
  bind_rows(
    forecast_results1 %>% 
      select(date, wages = wages2) %>% 
      mutate(type = "forecast")
  )

# View model specification and results
arima_fit1 %>% report()
full_data1

full_data1 |> 
  ggplot(aes(date, wages, color = type)) +
  geom_line() +
  geom_point() +
  labs(title = "ARIMA with log transformation and automatic order selection")


# v2 ----------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(distributional)

# Create reproducible example
set.seed(123)
wages_tbl <- tibble(
  date = yearquarter(seq(ymd("2010-01-01"), ymd("2023-10-01"), by = "quarter")),
  wages = exp(cumsum(rnorm(length(date), mean = 0.02, sd = 0.1)) + 
                sin(seq(0, 4*pi, length.out = length(date)))) + rnorm(length(date), sd = 0.2)
) %>% 
  as_tsibble(index = date)

wages_tbl |> 
  ggplot(aes(date, wages)) +
  geom_line()

# Proper ARIMA specification with automatic back-transformation
arima_fit <- wages_tbl %>% 
  model(ARIMA(box_cox(wages, lambda = 0)))  # lambda=0 for log transform
arima_fit |> report()

model <- Arima(data, include.drift = FALSE)

af2 <- wages_tbl %>% 
  model(forecast::Arima(wages, include.drift = FALSE))
af2 |> report()


# Forecast with automatic back-transform
forecast_results <- arima_fit %>% 
  forecast(h = 16) %>% 
  as_tibble()

# Combine results
full_data <- wages_tbl %>% 
  mutate(type = "actual", .mean = wages, lwages = log(wages)) %>% 
  bind_rows(
    forecast_results %>% 
      mutate(type = "forecast", wages = exp(.mean), lwages=.mean) %>% 
      select(date, wages, lwages, type)
  )
full_data

full_data |> 
  ggplot(aes(date, wages, color = type)) +
  geom_line() +
  geom_point() +
  labs(title = "ARIMA with automatic back-transformation")

full_data |> 
  ggplot(aes(date, lwages, color = type)) +
  geom_line() +
  geom_point() +
  labs(title = "log wages ARIMA with automatic back-transformation")


# v3 ----------------------------------------------------------------------

# Load required libraries
library(dplyr)      # for data wrangling
library(lubridate)  # for date utilities
library(forecast)   # for auto.arima() and forecast()
# library(ggplot2)  # if you want to plot
# library(tidyr)    # often useful for pivoting etc.

#----------------------------------------------------------------------------
# 1) Suppose your data frame (tibble) is called "df" with columns:
#    * date (quarterly dates)
#    * wages (not seasonally adjusted wages)
#----------------------------------------------------------------------------

# Example data creation (you can skip this part in real use)
set.seed(123)
df <- tibble(
  date = seq.Date(from = as.Date("2015-01-01"), 
                  by = "quarter", 
                  length.out = 24),  # 6 years of quarterly data
  wages = round(rnorm(24, mean = 10000, sd = 200), 0)
)

df |> ggplot(aes(date, wages)) + geom_line()

#----------------------------------------------------------------------------
# 2) Convert your wages to a quarterly ts object
#    This helps auto.arima() understand the seasonal frequency
#----------------------------------------------------------------------------
df_ts <- ts(df$wages,
            frequency = 4, 
            start = c(year(min(df$date)), quarter(min(df$date))))

#----------------------------------------------------------------------------
# 3) Fit an ARIMA model to the *logged* wages
#----------------------------------------------------------------------------
fit <- auto.arima(log(df_ts), seasonal = TRUE)

#----------------------------------------------------------------------------
# 4) Forecast 4 years ahead (4 quarters/year * 4 years = 16 steps)
#----------------------------------------------------------------------------
h <- 16
fc <- forecast(fit, h = h)

#----------------------------------------------------------------------------
# 5) Exponentiate forecasts to transform them back from log scale
#----------------------------------------------------------------------------
fc_mean    <- exp(fc$mean)
fc_lower80 <- exp(fc$lower[, 1])  # 80% lower
fc_upper80 <- exp(fc$upper[, 1])  # 80% upper
fc_lower95 <- exp(fc$lower[, 2])  # 95% lower
fc_upper95 <- exp(fc$upper[, 2])  # 95% upper

#----------------------------------------------------------------------------
# 6) Build a tibble of forecasted values with future quarterly dates
#----------------------------------------------------------------------------
last_date <- max(df$date)
# For the next 16 quarters, increment by "quarter"
forecast_dates <- seq.Date(from = last_date %m+% months(3),
                           by   = "quarter", 
                           length.out = h)

df_forecast <- tibble(
  date          = forecast_dates,
  forecast_wage = as.numeric(fc_mean),
  lower80       = as.numeric(fc_lower80),
  upper80       = as.numeric(fc_upper80),
  lower95       = as.numeric(fc_lower95),
  upper95       = as.numeric(fc_upper95)
)

#----------------------------------------------------------------------------
# 7) Join forecast data with the original data:
#    - One common approach is to bind_rows(), marking actual vs forecast
#----------------------------------------------------------------------------
df_final <- bind_rows(
  df %>%
    mutate(
      forecast_wage = wages,  # use the real wage for actual rows
      lower80       = NA,
      upper80       = NA,
      lower95       = NA,
      upper95       = NA,
      type          = "actual"
    ),
  df_forecast %>%
    mutate(
      wages = NA,  # no actual wage data for future periods
      type  = "forecast"
    )
)

df_final
# A tibble with your original data plus the forecast columns

df_final |> 
  ggplot(aes(date, forecast_wage, colour=type)) +
  geom_line()

ggplot(df_final, aes(x = date)) +
  geom_line(aes(y = wages), color = "blue") +
  geom_line(aes(y = forecast_wage), color = "red") +
  geom_ribbon(aes(ymin = lower80, ymax = upper80), alpha = 0.2, fill = "red") +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.1, fill = "red") +
  labs(y = "Wages",
       title = "Quarterly Wages with ARIMA Forecast (Logged-then-Exponentiated)") +
  theme_minimal()


# v4 ----------------------------------------------------------------------


# Install packages if needed:
# install.packages(c("fable", "fabletools", "tsibble", "lubridate", "tidyverse"))

library(tidyverse)
library(tsibble)
library(lubridate)
library(fable)
library(fabletools)

#-----------------------------------------------------------------------------
# 1) Example data: a tibble with quarterly dates and wages (unlogged, raw data)
#    In practice, you already have something like this.
#-----------------------------------------------------------------------------
set.seed(123)
df <- tibble(
  date = seq.Date(from = as.Date("2015-01-01"), 
                  by = "quarter", 
                  length.out = 24),  # 6 years of quarterly data
  wages = round(rnorm(24, mean = 10000, sd = 200), 0)
)

#-----------------------------------------------------------------------------
# 2) Convert to a TSIBBLE (the fable-friendly time series object)
#    - We’ll store the quarterly date index as a `yearquarter`.
#-----------------------------------------------------------------------------
df_ts <- df %>%
  mutate(date = yearquarter(date)) %>%  # Convert to quarterly index
  as_tsibble(index = date)

#-----------------------------------------------------------------------------
# 3) Fit an ARIMA model on log(wages).
#    - By default, ARIMA() will do an automatic search for p,d,q (and seasonal).
#    - We’re simply passing `log(wages)` directly in the formula.
#-----------------------------------------------------------------------------
df_fit <- df_ts %>%
  model(
    arima = ARIMA(log(wages))
  )

#-----------------------------------------------------------------------------
# 4) Forecast 4 years (16 quarters) ahead on the logged scale.
#-----------------------------------------------------------------------------
df_fc <- df_fit %>%
  forecast(h = "4 years")


df_ts |> 
  mutate(type="actual") |> 
  bind_rows(as_tsibble(df_fc) |> 
              rename(wagedist=wages, wages=.mean) |> 
              mutate(type="forecast")) |>
  ggplot(aes(date, wages, color=type)) +
  geom_line() +
  geom_point()



#-----------------------------------------------------------------------------
# 5) Exponentiate forecasts back to the original scale.
#    - By default, fable creates columns like .mean, .lower, .upper, etc.
#    - We'll create new columns for wage_mean, wage_lower_80, etc.
#-----------------------------------------------------------------------------
df_fc_exp <- df_fc %>%
  mutate(
    wage_mean      = exp(.mean),
    wage_lower_80  = exp(.lower_80),
    wage_lower_95  = exp(.lower_95),
    wage_upper_80  = exp(.upper_80),
    wage_upper_95  = exp(.upper_95)
  )

#-----------------------------------------------------------------------------
# 6) Combine the forecasts with the original data in one tibble.
#    - We’ll label original data as "actual" and forecasts as the model name.
#    - Note that .model = "arima" by default for forecasts.
#-----------------------------------------------------------------------------
# a) Original data labeled as "actual"
df_actual <- df_ts %>%
  as_tibble() %>%
  mutate(
    .model        = "actual", 
    wage_mean     = wages, 
    wage_lower_80 = NA_real_,
    wage_lower_95 = NA_real_,
    wage_upper_80 = NA_real_,
    wage_upper_95 = NA_real_
  )

# b) Exponentiated forecast data
df_forecast <- df_fc_exp %>%
  as_tibble() %>%
  # Keep only the essential columns
  select(date, .model, wage_mean, wage_lower_80, wage_lower_95, 
         wage_upper_80, wage_upper_95)

# c) Bind rows to get a single tibble
df_final <- bind_rows(df_actual, df_forecast) %>%
  arrange(date)

#-----------------------------------------------------------------------------
# Check the final tibble:
#-----------------------------------------------------------------------------
df_final



# v5 drift ----------------------------------------------------------------------

################################################################################
# Example: Quarterly Wages with Upward Drift, ARIMA on Logged Data (fable)
# 
# - Create example data with upward drift
# - Convert to a tsibble
# - Fit ARIMA(log(wages)) using fable
# - Forecast 4 years ahead
# - Exponentiate forecasts back to original scale
# - Combine actual & forecast data
# - Plot results
#
# References/Links:
#  - fable docs: https://fable.tidyverts.org/
#  - fabletools docs: https://fabletools.tidyverts.org/
#  - tsibble docs: https://tsibble.tidyverts.org/
#  - Forecasting: Principles and Practice (3rd ed.): https://otexts.com/fpp3/
################################################################################

# Install packages if you don't have them:
# install.packages(c("tidyverse", "tsibble", "fable", "fabletools", "lubridate"))

# Load libraries
library(tidyverse)
library(tsibble)
library(lubridate)
library(fable)
library(fabletools)

# 1) CREATE EXAMPLE DATA WITH UPWARD DRIFT
set.seed(123)
n_quarters <- 24
df <- tibble(
  # 24 quarters from 2015Q1
  date = seq.Date(from = as.Date("2015-01-01"), by = "quarter", length.out = n_quarters),
  # Upward drift: baseline 10,000 + 50 per quarter + noise
  wages = round(10000 + 50*(1:n_quarters) + rnorm(n_quarters, mean = 0, sd = 200), 0)
)

df |> ggplot(aes(date, wages)) + geom_line()

# 2) CONVERT TO TSIBBLE
df_ts <- df %>%
  mutate(date = yearquarter(date)) %>%
  as_tsibble(index = date)

# 3) FIT ARIMA ON LOG(WAGES)
df_fit <- df_ts %>%
  model(
    arima = ARIMA(log(wages) ~ drift())  # auto-ARIMA with seasonal=TRUE by default for quarterly data
  )

# 4) FORECAST 4 YEARS (16 QUARTERS)
df_fc <- df_fit %>%
  forecast(h = "4 years")  # "4 years" => 16 quarters ahead

df_ts |> 
  mutate(type="actual") |> 
  bind_rows(as_tsibble(df_fc) |> 
              rename(wagedist=wages, wages=.mean) |> 
              mutate(type="forecast")) |>
  ggplot(aes(date, wages, color=type)) +
  geom_line() +
  geom_point()



# v6 claude ---------------------------------------------------------------

# Load required packages
library(tidyverse)
library(fable)
library(tsibble)

# Create sample data
set.seed(123)
dates <- seq(as.Date("2015-01-01"), as.Date("2023-09-30"), by = "quarter")
wages <- 1000 * exp(0.02 * 1:length(dates) + 0.1 * sin(2 * pi * 1:length(dates)/4) + rnorm(length(dates), 0, 0.02))

# Create initial tibble
wages_data <- tibble(
  date = dates,
  wages = wages
) %>%
  mutate(date=yearquarter(date)) |> 
  as_tsibble(index = date)

# Fit ARIMA model
wages_fit <- wages_data %>%
  model(
    arima = ARIMA(log(wages) ~ pdq(d=1) + PDQ(D=1, period=4))
  )
wages_fit |> report()

# Generate forecasts
wages_forecast <- wages_fit %>%
  forecast(h = "4 years") 

addon <- wages_forecast |> 
  as_tibble() |> 
  select(date, wages=.mean) |> 
  mutate(type="forecast")

result <- bind_rows(
  wages_data |> mutate(type="actual"),
  addon)

result |> 
  ggplot(aes(date, wages, color=type)) +
  geom_line() +
  geom_point()

wages_forecast |> 
  mutate(intervals = hilo(wages, 95)) |> 
  unpack_hilo(intervals)

# Plot results
p <- wages_combined %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = wages)) +
  geom_line(aes(y = forecast), color = "blue") +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "blue", alpha = 0.2) +
  theme_minimal() +
  labs(y = "Quarterly Wages", title = "Wage Forecasts with 95% Prediction Intervals")

print(p)



