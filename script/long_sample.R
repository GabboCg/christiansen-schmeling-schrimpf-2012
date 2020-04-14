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
         year = stringr::str_sub(yyyymm, 1, 4),
         month = as.numeric(stringr::str_sub(yyyymm, -2))) %>%
  rename(tb = tbl, bm = b_m) %>% 
  select(yyyymm, year, month, dp, dy, ep, de, bm, ntis, tb, lty, 
         ltr, ts, def, dfr, rtb, rbr, infl, rfree) %>% 
  filter(yyyymm >= 192612 & yyyymm <= 201812)

# kennet-french -----------------------------------------------------------

kf_factors <- openxlsx::read.xlsx("data-raw/data_raw.xlsx", sheet = 2)

kf_tbl <- kf_factors %>% 
  as_tibble() %>% 
  mutate_all(list(~ as.numeric(.))) %>% 
  janitor::clean_names() %>% 
  filter(yyyymm >= 192612) %>% 
  rename(mkt = mkt_rf) %>% 
  mutate_at(vars(mkt:hml), list(~ . / 100)) %>% 
  mutate(year = stringr::str_sub(yyyymm, 1, 4),
         month = as.numeric(stringr::str_sub(yyyymm, -2))) %>%
  filter(yyyymm >= 192612 & yyyymm <= 201812) %>% 
  select(yyyymm, year, month, mkt, smb, hml)

# short term reversal -----------------------------------------------------

st_reversal <- openxlsx::read.xlsx("data-raw/data_raw.xlsx", sheet = 3)

st_tbl <- st_reversal %>% 
  janitor::clean_names() %>%
  mutate(str = st_rev / 100,
         year = stringr::str_sub(yyyymm, 1, 4),
         month = as.numeric(stringr::str_sub(yyyymm, -2))) %>%
  filter(yyyymm >= 192612 & yyyymm <= 201812) %>% 
  select(yyyymm, year, month, str)

# long sample -------------------------------------------------------------

long_sample_tbl <- left_join(gw_tbl, kf_tbl) %>% 
  left_join(st_tbl)

long_sample_tbl %>% 
  filter(yyyymm >= 198301 & yyyymm <= 201012) %>% 
  mutate_at(vars(ntis:str), list(~ 100 * (.))) %>% 
  summarise_at(vars(dp:str), list(~ mean(.)))

# save dataset
library("openxlsx")

wb <- createWorkbook()

addWorksheet(wb, "predictors")
writeData(wb, "predictors", long_sample_tbl)

addWorksheet(wb, "volatility")
writeData(wb, "volatility", rv_stocks)

saveWorkbook(wb, "data/long_sample_updated.xlsx", overwrite = TRUE)
