# realized variance -------------------------------------------------------

library("tidyquant")
library("dplyr")

sp500_daily <-  tq_get(x = "^GSPC", 
                       get = "stock.prices",
                       periodicity = "daily",
                       from = "1927-12-01",
                       to = "2019-01-01")

rv_stocks <- sp500_daily %>% 
  select(date, close) %>%
  mutate(log_ret = log(close / lag(close, 1)),
         month = strftime(date, "%m"),
         year = lubridate::year(date)) %>% 
  na.omit() %>% 
  group_by(month, year) %>% 
  mutate(rv = sqrt(sum(log_ret ^ 2))) %>% 
  filter(row_number(date) == 1) %>% 
  ungroup() %>%
  mutate(date = as.numeric(paste0(year, month)),
         month = as.numeric(month)) %>%
  rename(yyyymm = date) %>%  
  select(yyyymm, year, month, rv)

# goyal welch predictors --------------------------------------------------

gw_predictors <- openxlsx::read.xlsx("data-raw/data_raw.xlsx", sheet = 1)

gw_tbl <- gw_predictors %>% 
  as_tibble() %>% 
  mutate_all(list(~ as.numeric(.))) %>% 
  janitor::clean_names() %>% 
  filter(yyyymm >= 192512) %>% 
  select(yyyymm, index, d12, e12, b_m, ntis, tbl, lty, ltr, baa, 
         aaa, corpr, infl, rfree) %>%
  mutate(dp = log(d12/index), ep = log(e12/index), 
         de = log(d12/e12), dy = log(d12/lag(index)),
         ts = lty - tbl, def = baa - aaa, dfr = corpr - ltr, 
         rtb = tbl - TTR::SMA(tbl, 12), 
         rbr = lty - TTR::SMA(lty, 12), 
         year = as.numeric(stringr::str_sub(yyyymm, 1, 4)),
         month = as.numeric(stringr::str_sub(yyyymm, -2))) %>%
  rename(tb = tbl, bm = b_m, infm = infl) %>% 
  select(yyyymm, year, month, dp, dy, ep, de, bm, ntis, tb, lty, 
         ltr, ts, def, dfr, rtb, rbr, infm, rfree) %>% 
  filter(yyyymm >= 198301 & yyyymm <= 201812)

# kennet-french -----------------------------------------------------------

kf_factors <- openxlsx::read.xlsx("data-raw/data_raw.xlsx", sheet = 2)

kf_tbl <- kf_factors %>% 
  as_tibble() %>% 
  mutate_all(list(~ as.numeric(.))) %>% 
  janitor::clean_names() %>% 
  filter(yyyymm >= 192612) %>% 
  rename(mkt = mkt_rf) %>% 
  mutate_at(vars(mkt:hml), list(~ . / 100)) %>% 
  mutate(year = as.numeric(stringr::str_sub(yyyymm, 1, 4)),
         month = as.numeric(stringr::str_sub(yyyymm, -2))) %>%
  filter(yyyymm >= 198301 & yyyymm <= 201812) %>% 
  select(yyyymm, year, month, mkt, smb, hml)

# short term reversal -----------------------------------------------------

st_reversal <- openxlsx::read.xlsx("data-raw/data_raw.xlsx", sheet = 3)

st_tbl <- st_reversal %>% 
  janitor::clean_names() %>%
  mutate(str = st_rev / 100,
         year = as.numeric(stringr::str_sub(yyyymm, 1, 4)),
         month = as.numeric(stringr::str_sub(yyyymm, -2))) %>%
  filter(yyyymm >= 198301 & yyyymm <= 201812) %>% 
  select(yyyymm, year, month, str)

# pastor factor -----------------------------------------------------------

pastor_factor <- openxlsx::read.xlsx("data-raw/data_raw.xlsx", sheet = 4)

pastor_factor_tbl <- pastor_factor %>% 
  as_tibble() %>% 
  mutate_all(list(~ as.numeric(.))) %>% 
  janitor::clean_names() %>%
  select(yyyymm, agg_liq) %>%
  rename(ps = agg_liq) %>% 
  mutate(year = as.numeric(stringr::str_sub(yyyymm, 1, 4)),
         month = as.numeric(stringr::str_sub(yyyymm, -2))) %>%
  filter(yyyymm >= 198301 & yyyymm <= 201812) %>% 
  select(yyyymm, year, month, ps)

# fred data ---------------------------------------------------------------

tickers_monthly <- c("INDPRO", "M1SL", "TCU", "PAYEMS", "UMCSENT", 
                     "HOUST")

fred_tbl <- tq_get(tickers_monthly,
                   get = "economic.data",
                   periodicity = "monthly",
                   from = "1982-01-01",
                   to = "2018-12-01") %>% 
  tidyr::spread(symbol, price) %>% 
  janitor::clean_names() %>% 
  mutate(ipm = log(indpro / lag(indpro, 1)),
         ipa = log(indpro / lag(indpro, 12)),
         m1m = log(m1sl / lag(m1sl, 1)),
         m1a = log(m1sl / lag(m1sl, 12)),
         cap = log(tcu / lag(tcu, 1)),
         empl = (payems - lag(payems, 1)) / lag(payems, 1),
         sent = (umcsent - lag(umcsent, 1)) / lag(umcsent, 1),
         hs = (houst - lag(houst, 1)) / lag(houst, 1)) %>% 
  na.omit() %>% 
  mutate(month = strftime(date, "%m"),
         year = lubridate::year(date),
         yyyymm = as.numeric(paste0(year, month)),
         month = as.numeric(month)) %>% 
  select_at(vars(yyyymm, year, month, ipm:hs))

# philadelphia fed --------------------------------------------------------

diff_tbl <- openxlsx::read.xlsx("data-raw/data_raw.xlsx", sheet = 5) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(year = as.numeric(stringr::str_sub(yyyymm, 1, 4)),
         month = as.numeric(stringr::str_sub(yyyymm, -2))) %>%
  na.omit() %>% 
  select(yyyymm, year, month, diff) %>% 
  filter(yyyymm >= 198301 & yyyymm <= 201812)

# datastream --------------------------------------------------------------

datastream_tbl <- openxlsx::read.xlsx("data-raw/data_raw.xlsx", sheet = 6) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(date = seq.Date(as.Date("1978-01-01"), as.Date("2020-02-01"), by = "month"),
         ordm = log(order / lag(order, 1)),
         orda = log(order/ lag(order, 12)),
         infa = log(cpi / lag(cpi, 12)),
         msci = log(msci / lag(msci, 1)),
         crb = log(crb / lag(crb, 1)),
         pmi = ism,
         conf = (conf - lag(conf, 1))  / lag(conf, 1),
         ted = (libor / 1200 - tbill / 1200),
         month = strftime(date, "%m"),
         year = lubridate::year(date),
         yyyymm = as.numeric(paste0(year, month)),
         month = as.numeric(month)) %>% 
  na.omit() %>% 
  select(yyyymm, year, month, ordm, orda, infa, msci, crb, pmi, pmbb, conf, ted) %>% 
  filter(yyyymm >= 198301 & yyyymm <= 201812)

# short sample ------------------------------------------------------------

# merge tibbles
short_sample_tbl <- left_join(gw_tbl, kf_tbl) %>% 
  left_join(st_tbl) %>% 
  left_join(pastor_factor_tbl) %>% 
  left_join(fred_tbl) %>% 
  left_join(diff_tbl) %>% 
  left_join(datastream_tbl) %>% 
  select(yyyymm, year, month, 
         dp, ep, mkt, smb, hml, str, msci, 
         tb, rtb, ltr, rbr, ts,
         def, ps, ted,
         infm, infa, ipm, ipa, hs, m1m, m1a, ordm, orda, crb, cap, empl, 
         sent, conf, diff, pmbb, pmi)

# save dataset
library("openxlsx")

wb <- createWorkbook()

addWorksheet(wb, "predictors")
writeData(wb, "predictors", short_sample_tbl)

addWorksheet(wb, "volatility")
writeData(wb, "volatility", rv_stocks)

saveWorkbook(wb, "data/short_sample_updated.xlsx", overwrite = TRUE)

# tickers_daily <- c("TEDRATE")
# 
# ted_tbl <- tq_get(tickers_daily,
#                   get = "economic.data",
#                   periodicity = "daily",
#                   from = "1980-01-01") %>%
#   mutate(year = lubridate::year(date),
#          month = lubridate::month(date)) %>%
#   na.omit() %>%
#   group_by(month, year) %>%
#   mutate(ted_month = mean(price) / 12) %>%
#   filter(row_number(date) == 1) %>%
#   ungroup() %>%
#   select(date, ted_month) %>%
#   mutate(date = as.Date(zoo::as.yearmon(date))) %>%
#   summarise(mean(ted_month))
