# Has to be updated when new device is released
library(XML)
library(dplyr)
x <- readLines(con = "http://www.enterpriseios.com/wiki/iOS_Devices")
a <- readHTMLTable(x) %>%
    as.data.frame.list() %>%
    select(2:4)
names(a) <- c('friendlyName', 'identifier', 'introduced')
a$AppId <- rep('e46e616e-3347-4891-b86e-e3dd9ec4eb29', nrow(a))
a$os <- rep('iOS', nrow(a))

xlsxPath <- paste("F:/Mobile - Rabbids/data tracking/", format(Sys.Date(), format="%Y.%m.%d"), "_", "iOS_device.xlsx", collapse = "", sep = "")
write.xlsx(as.data.frame(a), xlsxPath, showNA = FALSE, row.names = F)
