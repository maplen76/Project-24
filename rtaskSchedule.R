library(taskscheduleR)
library(dplyr)

## run script to extract the exchange rate every Monday
taskscheduler_create(taskname = "extractEcbExchangeRate", 
                     rscript = "D:/Rtest/ecbExchangeRateAll.R", 
                     schedule = "WEEKLY", 
                     starttime = "13:00",
                     days = "MON"
                     )

## run script weekly on Mon. Wed. Fri
taskscheduler_create(taskname = "extractPublishSummary_MON", 
                     rscript = "D:/Rtest/extractPublisherSummary.R", 
                     schedule = "WEEKLY", 
                     starttime = "12:00",
                     days = "MON"
                     )

taskscheduler_create(taskname = "extractPublishSummary_WED", 
                     rscript = "D:/Rtest/extractPublisherSummary.R", 
                     schedule = "WEEKLY", 
                     starttime = "12:00",
                     days = "WED"
                     )


taskscheduler_create(taskname = "extractPublishSummary_FRI", 
                     rscript = "D:/Rtest/extractPublisherSummary.R", 
                     schedule = "WEEKLY", 
                     starttime = "12:00",
                     days = "FRI"
)

## delete the tasks
taskscheduler_delete(taskname = "extractEcbExchangeRate")
taskscheduler_delete(taskname = "extractPublishSummary_MON")
taskscheduler_delete(taskname = "extractPublishSummary_WED")
taskscheduler_delete(taskname = "extractPublishSummary_FRI")

## log file is at the place where the helloworld.R script was located

a <- taskscheduler_ls()
View(filter(a, Author == 'jing.wang'))
