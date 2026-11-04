

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

# 3s1 logs

can_gas <- can_gas |> mutate(log_volume = log(Volume))
can_gas |>
  autoplot(log_volume) +
  labs(title = "Log of Canadian Natural Gas Production",
       x = NULL, y = "log(Volume)") +
  theme_minimal()
