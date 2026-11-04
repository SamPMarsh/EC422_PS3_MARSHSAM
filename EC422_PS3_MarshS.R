

library(tidyverse)
library(fpp3)

# 2a Get FRED CSV, had to download weird because FRED has been acting odd.

retail_raw = read.csv("/Users/sammarsh/Downloads/fredgraph.csv")

head(retail_raw)

# 2b fix column issues and missing values

retail_raw <- read_csv(
  "/Users/sammarsh/Downloads/fredgraph.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS   = col_double(),
    RSAFSNA = col_double()
  )
)

head(retail_raw)

# 2c wide to long

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(
    cols = c(RSAFS, RSAFSNA),
    names_to = "series_id",
    values_to = "sales"
  )

head(retail_raw_long)

# 2d tsibble work

retail_raw_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_raw_ts)
has_gaps(retail_raw_ts)

# 2e Canadian gas

head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |> select(Month, Volume)
has_gaps(can_gas)
head(can_gas)

# 2f dates in a tsibble

# 2f part a
can_gas_recent <- can_gas |> filter(Month >= yearmonth("1990 Jan"))
head(can_gas_recent)

# 2f part b
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))


# 2f part c

can_gas_recent |>
  mutate(next_month = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

#2f part d

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))
head(can_gas_q)
can_gas_q |> autoplot(Volume)


# 2g autoplot

# 2g part a

retail_raw_ts |> autoplot(sales)

# 2g part b

retail_raw_ts |>
  autoplot(sales) +
  labs(title = "U.S. Retail and Food Services Sales",
       subtitle = "Seasonally adjusted vs. not seasonally adjusted",
       x = NULL, y = "Millions of dollars", color = NULL) +
  theme_minimal()

# 2g part c

can_gas |>
  autoplot(Volume) +
  labs(title = "Canadian Natural Gas Production",
       x = NULL, y = "Billions of cubic meters") +
  theme_minimal()

# 2g part d

can_gas |>
  autoplot(Volume) +
  geom_smooth(method = "loess", se = FALSE, color = "firebrick") +
  labs(title = "Canadian Natural Gas Production with LOESS Trend",
       x = NULL, y = "Billions of cubic meters") +
  theme_minimal()

# 3.1 logs

# 3.1 b

can_gas <- can_gas |> mutate(log_volume = log(Volume))
can_gas |>
  autoplot(log_volume) +
  labs(title = "Log of Canadian Natural Gas Production",
       x = NULL, y = "log(Volume)") +
  theme_minimal()

# 3.1 c

retail_nsa <- retail_raw_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(title = "NSA Retail Sales (level)", x = NULL, y = "Millions of dollars") +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(title = "NSA Retail Sales (log)", x = NULL, y = "log(sales)") +
  theme_minimal()

# 3.2 growth rates

can_gas <- can_gas |>
  mutate(
    growth_direct  = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(title = "Canadian Gas Production: Month-over-Month Growth",
       x = NULL, y = "Growth rate (%)") +
  theme_minimal()

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(title = "Gap Between Direct and Log-Difference Growth",
       x = NULL, y = "Absolute gap (percentage points)") +
  theme_minimal()

# 3.3 seasonal differences

retail_raw_ts <- retail_raw_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_raw_ts |>
  autoplot(growth_mom) +
  labs(title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
       x = NULL, y = "Growth rate (%)", color = NULL) +
  theme_minimal()

# 4.1 season plots

#4.1 a

retail_raw_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(title = "Seasonal Pattern in NSA Retail Sales",
       y = "Millions of dollars") +
  theme_minimal()

# 4.1 b

retail_raw_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(title = "Seasonal Pattern in SA Retail Sales",
       y = "Millions of dollars") +
  theme_minimal()

# 4.1 c

can_gas |>
  gg_season(log_volume) +
  labs(title = "Seasonal Pattern in Canadian Gas Production (log)",
       y = "log(Volume)") +
  theme_minimal()

# 4.2 subseries plot

can_gas |>
  gg_subseries(log_volume) +
  labs(title = "Subseries Plot: Canadian Gas Production",
       y = "log(Volume)")

# 4.3 seasonal differences

retail_nsa_x13 <- retail_raw_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# 4.3 a

retail_nsa_x13 |>
  as_tibble() |>
  mutate(cal_month = month(Month, label = TRUE)) |>
  group_by(cal_month) |>
  summarize(avg_seasonal_factor = mean(seasonal)) |>
  arrange(desc(avg_seasonal_factor))

# 4.3 b

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_raw_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
       x = NULL, y = "Difference (%)") +
  theme_minimal()

sa_compare |> slice_max(abs(pct_diff), n = 5)


# 4.4 Linear Trend Seasonal adjustments

# 4.4 a 

can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))
report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# avg monthly growth %
can_gas_fit |> tidy() |> filter(term == "trend()") |> mutate(pct = 100 * estimate)

# month with largest |t| among seasonal dummies
can_gas_fit |>
  tidy() |>
  filter(str_detect(term, "season")) |>
  slice_max(abs(statistic), n = 1)

# 4.4 b
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# 4.4 c
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)




