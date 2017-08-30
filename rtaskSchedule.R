library(taskscheduleR)
library(dplyr)

## run script to extract the exchange rate every Monday
taskscheduler_create(taskname = "extractEcbExchangeRate", 
                     rscript = "D:/Rtest/ecbExchangeRateAll.R", 
                     schedule = "WEEKLY", 
                     starttime = "13:00",
                     days = "MON",
                     startdate = format(Sys.Date(), "%m/%d/%Y")  # note: have to keep the same date format as operating system
                     )

## run script weekly on Mon. Wed. Fri
taskscheduler_create(taskname = "extractPublishSummary_MON", 
                     rscript = "D:/Rtest/extractPublisherSummary.R", 
                     schedule = "WEEKLY", 
                     starttime = "12:00",
                     days = "MON",
                     startdate = format(Sys.Date(), "%m/%d/%Y")  # note: have to keep the same date format as operating system
                     )

taskscheduler_create(taskname = "extractPublishSummary_WED", 
                     rscript = "D:/Rtest/extractPublisherSummary.R", 
                     schedule = "WEEKLY", 
                     starttime = "12:00",
                     days = "WED",
                     startdate = format(Sys.Date(), "%m/%d/%Y")  # note: have to keep the same date format as operating system
                     )


taskscheduler_create(taskname = "extractPublishSummary_FRI", 
                     rscript = "D:/Rtest/extractPublisherSummary.R", 
                     schedule = "WEEKLY", 
                     starttime = "12:00",
                     days = "FRI",
                     startdate = format(Sys.Date(), "%m/%d/%Y")  # note: have to keep the same date format as operating system
)

## delete the tasks
taskscheduler_delete(taskname = "extractEcbExchangeRate")
taskscheduler_delete(taskname = "extractPublishSummary_MON")
taskscheduler_delete(taskname = "extractPublishSummary_WED")
taskscheduler_delete(taskname = "extractPublishSummary_FRI")

## log file is at the place where the helloworld.R script was located

a <- taskscheduler_ls()
View(filter(a, Author == 'jing.wang'))
