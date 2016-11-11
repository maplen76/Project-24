library(xlsx)
library(dplyr)
library(RODBC)
library(stringr)

Sys.setenv(TZ='UTC') # set the timezone as UTC

# the mapping from country to currency
ctr_currency <- data.frame(countryCode = c('AR', 'AU', 'BG', 'BR', 'CA', 'CH', 'CN', 'CZ', 'DK', 'DZ', 'GB', 'HK', 'HR', 'HU', 'ID', 'IL', 'IN', 'JP'
                                       , 'KR', 'MA', 'MX', 'MY', 'NO', 'NZ', 'PH', 'PL', 'RO', 'RU', 'SE', 'SG', 'TH', 'TR', 'TW', 'US', 'ZA'),
                           currency = c('ARS', 'AUD', 'BGN', 'BRL', 'CAD', 'CHF', 'CNY', 'CZK', 'DKK', 'DZD', 'GBP', 'HKD', 'HRK', 'HUF', 'IDR'
                                        , 'ILS', 'INR', 'JPY', 'KRW', 'MAD', 'MXN', 'MYR', 'NOK', 'NZD', 'PHP', 'PLN', 'RON', 'RUB', 'SEK', 'SGD', 'THB', 'TRY', 'TWD', 'USD', 'ZAR'), stringsAsFactors = F)


# loading csv file from HQ
# make sure the right path and right file name
ios_path <- paste("F:\\Mobile - Rabbids\\data tracking\\Data iOS - Houston\\houston transaction raw\\Rabbids_Runner_Transactions_Apple_20161110.csv")

ios <- read.csv(file = ios_path
                ,header =  F
                ,col.names = c("appName", "store", "id", "transaction_id", "purchase_date", "item_id", "quantity", "original_purchase_date"
                            , "original_transaction_id", "is_restore", "countryCode", "device_id", "sandbox", "price_local")
                ,stringsAsFactors = F) %>% tbl_df() %>%
    filter(sandbox == 0, countryCode != "CN") %>%
    mutate(Platform = rep("iOS", n()), appId = rep("e46e616e-3347-4891-b86e-e3dd9ec4eb29", n())) %>%
    mutate(price_local1 = as.numeric(str_replace_all(str_extract_all(price_local, "[0-9]+(\\.|,)[0-9]+"), ",", "\\.") ) ) %>%
    mutate(ccode = coalesce(str_extract(str_extract(price_local, "([A-Z]+\\$)|\\$") , "[A-Z]+"), countryCode) ) %>%
    left_join(ctr_currency, by = c("ccode" = "countryCode") ) %>%
    select(Platform, appId, id, transaction_id, purchase_date, item_id,  countryCode, device_id, currency, price_local, price_local1) %>%
    mutate(date1 = as.POSIXct(purchase_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), transaction_id = as.character(transaction_id)) %>%
    select(Platform:transaction_id, purchase_date = date1, item_id: price_local1)


# loading android csv file
android_path <- ("F:\\Mobile - Rabbids\\data tracking\\Data iOS - Houston\\houston transaction raw\\Rabbids_Runner_Transactions_Google_20161110.csv")

raw_adr <- read.csv(file = android_path
                    ,header =  F
                    ,col.names = c("appName", "store", "id", "transaction_id", "purchase_date", "item_id", 
                                   "countryCode", "device_id", "purchase_state", "price_local")
                    ,stringsAsFactors = F) %>% tbl_df() %>%
    filter(price_local != "[PROMO_CODE]", countryCode != "CN") %>%
    mutate(Platform = rep("Android", n()), appId = rep("18706323-c903-4323-83a9-0d570a588525", n())) %>%
    mutate(price_local1 = as.numeric(str_replace_all(str_extract_all(price_local, "[0-9]+(\\.|,)[0-9]+"), ",", "\\.") ) ) %>%
    mutate(ccode = coalesce(str_extract(str_extract(price_local, "([A-Z]+\\$)|\\$") , "[A-Z]+"), countryCode) ) %>%
    left_join(ctr_currency, by = c("countryCode" = "countryCode") ) 

ctr_currency_1 <- data.frame(countryCode = c('DE', 'PH', 'NL'), currency = c('EUR', 'EUR', 'EUR'), stringsAsFactors = F)

android <- left_join(raw_adr, ctr_currency_1, by = c("ccode" = "countryCode")) %>%
    mutate(currency = coalesce(currency.y, currency.x))  %>%
    mutate(date1 = as.POSIXct(purchase_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    select(Platform, appId, id, transaction_id, purchase_date = date1, item_id, 
           countryCode, device_id, currency, price_local, price_local1)

# union ios data and android data
houston <- rbind.data.frame(ios, android)

# extrac history data from SQL server database

ch <- odbcConnect("rabbids")
hdata <- sqlQuery(channel = ch
                  ,query = "SELECT * FROM DW_RABBIDS_CRAZYRUSH_POSTLAUNCH.[user].houston_transaction"
                  ,stringsAsFactors = F) %>% tbl_df()

odbcClose(channel = ch)

t <- max(hdata$purchase_date)

# attach new transactions
f <- hdata %>%
    rbind.data.frame(filter(houston, purchase_date > max(hdata$purchase_date)))

file_path <- paste("F:\\Mobile - Rabbids\\data tracking\\Data iOS - Houston\\"
              ,format(Sys.Date(), format="%Y.%m.%d"), "_", "houston_transaction.xlsx", collapse = "", sep = "")

write.xlsx(x = as.data.frame(f), file = file_path, row.names = F, showNA = FALSE)
