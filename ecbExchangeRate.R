# https://sdw.ecb.europa.eu/export.do?node=2018794&CURRENCY=&start=01-01-2016&end=&FREQ=D&DATASET=0&exportType=sdmx
library(xml2)
library(dplyr)
library(xlsx)

url <- "https://sdw.ecb.europa.eu/export.do?node=2018794&CURRENCY=&start=01-01-2016&end=&FREQ=D&DATASET=0&exportType=sdmx"
download.file(url = url, destfile = "F:\\Mobile - Rabbids\\data tracking\\ecbExchangeRate\\exchangeRate.xml")

doc1 <- read_xml("F:\\Mobile - Rabbids\\data tracking\\ecbExchangeRate\\exchangeRate.xml")


# "LTL","CYP","EEK","GRD","ISK","LVL","MTL","SIT","SKK" has been yielded into EUR
currency_code <- c('ARS', 'AUD', 'BGN', 'BRL', 'CAD', 'CHF', 'CNY', 'CZK', 'DKK', 
                   'DZD', 'GBP', 'HKD', 'HRK', 'HUF', 'IDR', 'ILS', 'INR', 'JPY', 
                   'KRW', 'MAD', 'MXN', 'MYR', 'NOK', 'NZD', 'PHP', 'PLN', 'RON', 
                   'RUB', 'SEK', 'SGD', 'THB', 'TRY', 'TWD', 'USD', 'ZAR')

exchangeRate_daily <- data.frame()
exchangeRate_weekly <- data.frame()

# The default namespace for all elements is given by
ns <- c(d1 = "http://www.SDMX.org/resources/SDMXML/schemas/v2_0/message",
        xsi = "http://www.w3.org/2001/XMLSchema-instance",
        d2 = "http://www.ecb.int/vocabulary/stats/exr")


# to mine out daily exchange rate
for (i in 1:length(currency_code)) {
# build xpath like for each currency code "//d2:Series[@CURRENCY = 'ARS']"
path <- paste("//d2:Series[@CURRENCY=", "'", currency_code[i], "']", sep = "", collapse = "")

sub_ <- xml_find_all(doc1, path, ns)

rates <- xml_attr(xml_children(sub_), "OBS_VALUE") %>% as.numeric()
date <- xml_attr(xml_children(sub_), "TIME_PERIOD") %>% as.Date('%Y-%m-%d')
exr_daily <- data.frame(date = date, rate = rates) %>% 
    filter(date >= '2014-12-01')

date_scope <- seq(as.Date("2014-12-01"), Sys.Date()-1, by = "day")
exr_daily_full <- data.frame(date = date_scope) %>%
    left_join(exr_daily, by = "date") 

    for (j in 1:nrow(exr_daily_full)) {
        if (is.na(exr_daily_full[j,2]) ) {
            exr_daily_full[j,2] <- exr_daily_full[j-1,2] 
        }
    }

exr_dly <- exr_daily_full %>%
    mutate(currency = rep(x = currency_code[i], nrow(exr_daily_full)) )

# to aggregate exchange rate by weeks, the week start from Monday, 
exr_week <- exr_daily_full %>%
    filter(date >= '2015-01-01') %>%
    mutate(weekNumber = as.numeric(format(date, "%W"))+1, 
           yearNumber = as.numeric(format(date, "%Y")), 
           wkDay = weekdays(date)) %>%
    filter(wkDay != "Saturday" & wkDay != "Sunday") %>%
    group_by(yearNumber, weekNumber) %>%
    summarise(exchangeRate = mean(rate, na.rm = T)) %>%
    select(yearNumber, weekNumber, exchangeRate) 

# to take last exchange rate as which of next 2 weeks
exr_latest <- exr_week %>%
    group_by(yearNumber) %>%
    summarise(weekNumber = max(weekNumber)) %>%
    arrange(yearNumber) %>%
    tail(1) %>%
    left_join(exr_week, by =c("yearNumber", "weekNumber"))

exr_next2Wk <- if (exr_latest$weekNumber != 53) {
    exr_next <- exr_latest[c(1,1),] %>%
    mutate(weekNumber = weekNumber + 1:2)
    } else {
    exr_next <- exr_latest[c(1,1),]
    exr_next$yearNumber <- exr_latest$yearNumber+1
    exr_next$weekNumber <- 1:2
    }
     
exr_weekly <- rbind(exr_week, exr_next2Wk)

exr_weekly$currency <- rep(x = currency_code[i], nrow(exr_weekly))
exchangeRate_weekly <- rbind.data.frame(exchangeRate_weekly, exr_weekly)

exchangeRate_daily <- rbind.data.frame(exchangeRate_daily, exr_dly) 

}

print(exchangeRate_weekly)

# save document as xlsx format
filePath2 <- paste("F:/Mobile - Rabbids/data tracking/ecbExchangeRate/", format(Sys.Date(), format="%Y.%m.%d"), " - ", "exchangeRate.xlsx", collapse = "", sep = "")
write.xlsx(as.data.frame(exchangeRate_weekly), filePath2, showNA = FALSE, row.names = F)
as.numeric(format(Sys.Date(), "%W"))

