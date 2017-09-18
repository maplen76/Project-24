Sys.setlocale(category = "LC_ALL", locale = "chs") # in order to chinese character can be exported
setwd("F:\\Mobile - Marketing Research\\r_environment\\r_environment")
library(dplyr)
library(sqldf)
library(xlsx)
library(stringr)
library(reshape2)
# build table dim_quarter
dim_quarter <- data.frame(month = 1:12, quarter = c(rep("Q1",3), rep("Q2",3), rep("Q3",3), rep("Q4",3)))

#### collect data from local tableau ####
# Save the data as csv file manually, 
# And just need to add new data into the csv file every month by copying data from tableau reports
revenue <- read.csv(file = "revenue.csv", stringsAsFactors = F) %>% tbl_df()

# import total history download data
download <- read.csv(file = "download.csv", stringsAsFactors = F) %>% tbl_df()

# import total history dim_game data
dim_game_releaseMonth <- read.csv(file = "dim_game.csv", stringsAsFactors = F, na.strings=c("","NA")) %>% 
  tbl_df() %>% 
  mutate(releaseDate = as.Date(releaseDate, "%m/%d/%Y")) %>%
  group_by(appId) %>%
  summarise(releasMonth = min(releaseMonthFix, na.rm = T),
            releasDate = min(releaseDate, na.rm = T),
            gameAge = n()) %>%
  arrange(desc(releasMonth)) %>%
  select(appId, releasMonth, gameAge)

# find the release month of each game
revenue_release <- revenue %>%
  group_by(appId) %>%
  summarise(releasMonth = min(period), gameAge = n()) %>%
  arrange(desc(releasMonth))

download_release <- download %>%
  group_by(appId) %>%
  summarise(releasMonth = min(period), gameAge = n()) %>%
  arrange(desc(releasMonth))

release <- rbind.data.frame(revenue_release, download_release, dim_game_releaseMonth)

release_month_date <- release %>%
  group_by(appId) %>%
  summarise(releasMonth = min(releasMonth), gameAge = max(gameAge))  %>%
  arrange(desc(releasMonth)) %>%
  left_join(select(dim_game_release, appId, releasDate), by = 'appId') %>%
  mutate(year = substr(releasMonth, 1, 4), month = as.numeric(substr(releasMonth,6,7))) %>%
  left_join(dim_quarter, by = "month") %>%
  mutate(releaseQuarter = paste(year, quarter, sep = "-")) %>%
  select(appId, releasMonth, releaseQuarter, gameAge, releasDate)

write.csv(download_release, "relaseMonth_download.csv", row.names = F)
write.csv(revenue_release, "relaseMonth_revenue.csv", row.names = F)
write.csv(release_month_date, "relaseMonth.csv", row.names = F)

# import data of Sep 2015
sep2015 <- read.csv(file = "sep2015.csv", stringsAsFactors = F) %>% tbl_df()
sep <- select(sep2015, appId, genre)
sep_rpg <- filter(sep, genre == "RPG") %>% select(appId)

sep_rank <- filter(revenue_download_complete, period == '2015-09') %>%
  select(appId, revenueRank, revenueUSD, cumRevenue, downloadRank, download, cumDownload)

### measure difference between top 20% and Bottom20%####
# the objective is to measure the risk of different released genre
diff_t_b <- sep2015 %>%
  arrange(genre, desc(revenueUSD)) %>%
  group_by(genre) %>%
  mutate(r = row_number()
         ,i = max(r)
         ,p = r/i
         ,location = cut(p, breaks = c(0, 0.2, 0.8, 1), labels = c("Top20", "Middle60", "Bottom20"))) %>%
  group_by(genre, location) %>%
  summarise(mean = mean(revenueUSD)) %>%
  dcast(genre ~ location) %>%
  mutate(m = Top20/Bottom20) %>%
  arrange(desc(m))

write.csv(diff_t_b, "diff_top_bottom.csv", row.names = F)

### breakdown revenue by game genre
sep2015 %>%
  group_by(genre) %>%
  summarise(max = max(revenueUSD), min = min(revenueUSD)) %>%
  arrange(desc(r))

####################################################################################
revenueWithRank <- revenue %>%
  arrange(desc(period),desc(revenueUSD)) %>%
  group_by(period) %>%
  mutate(revenueRank = row_number()) %>% # rank revenue by appName and period
  arrange(appId, period) %>%
  group_by(appId) %>%
  mutate(nthMonth = row_number()) %>% # add month number after app released
  arrange(appId, period) %>%
  group_by(appId) %>%
  mutate(cumRevenue = cumsum(revenueUSD)) # add cumulative revenue by month
  
####################################################################################
downloadWithRank <- download %>%
  arrange(desc(period),desc(download)) %>%
  group_by(period) %>%
  mutate(downloadRank = row_number()) %>% # rank download by appName and period
  arrange(appId, period) %>%
  group_by(appId) %>%
  mutate(nthMonth = row_number()) %>% # add month number after app released
  arrange(appId, period) %>%
  group_by(appId) %>%
  mutate(cumDownload = cumsum(as.numeric(download))) # add cumulative revenue by month

####################################################################################
# find revenue data and download data for sep data 
sep_rpg_revenue <- left_join(x = sep_rpg, y = revenueWithRank, by = "appId")
sep_rpg_download <- left_join(x = sep_rpg, y = downloadWithRank, by = "appId")

# combine revenue and download
revenue_download <- sqldf("SELECT a.appId
                                  ,a.genre
                                  ,a.period
                                  ,a.revenueUSD
                                  ,a.revenueRank
                                  ,a.nthMonth
                                  ,a.cumRevenue
                                  ,b.download
                                  ,CASE WHEN b.appId IS NULL THEN 'y' else 'n' END as data_lost
                                  ,b.cumDownload
                                  ,b.downloadRank
                                  ,a.cumRevenue/b.cumDownload AS arpd
                            FROM sep_rpg_revenue a 
                            LEFT JOIN sep_rpg_download b 
                              ON a.appId = b.appId and a.period = b.period"
                            ,stringsAsFactors = F) %>% tbl_df()

list_lostData <- filter(revenue_download, data_lost == "y") %>%
                  select(appId) %>%
                  distinct()

# tag apps that lost data
revenue_download_complete <- sqldf("
                                   SELECT a.*
                                         ,CASE WHEN b.appId IS NOT NULL THEN 'n' ELSE 'y' END AS complete
                                     FROM revenue_download a
                                     LEFT JOIN list_lostData b
                                     ON a.appId = b.appId
                                   ") %>%  tbl_df()

# find data the first 3 months
m3 <- filter(revenue_download_complete, nthMonth == 3) %>%
        select(appId, period, nthMonth, cumRevenue, cumDownload, arpd, complete) 

# find data the first 12 months
m12 <- filter(revenue_download_complete, nthMonth == 12) %>%
        select(appId, period, nthMonth, cumRevenue, cumDownload)

# calculate average rank of first 3 month)
avg_rank_m3 <- sqldf("
                     SELECT a.appId
                           ,avg(b.revenueRank*1.0) as revenueRank_m3
                           ,avg(b.downloadRank*1.0) as downloadRank_m3
                       FROM m3 a
                       LEFT JOIN revenue_download b
                       ON a.appId = b.appId
                       WHERE b.nthMonth <= 3
                       GROUP BY a.appId
                     ", stringsAsFactors = F) %>% tbl_df()

# calulcate percentage of first 12 months)
m3_m12 <- sqldf("
                SELECT m3.appId
                      ,m3.cumRevenue/m12.cumRevenue AS revenuePercentage
                      ,m3.cumDownload/m12.cumDownload AS  downloadPercentage
                      ,CASE WHEN m12.appId IS NULL THEN 'n' ELSE 'y' END AS moreThan12month
                  FROM m3
                  LEFT JOIN m12
                    ON m3.appId = m12.appId
                ", stringsAsFactors = F)  %>%  tbl_df()



m3_final <- sqldf("
                  SELECT a.*
                        ,b.revenueRank_m3
                        ,b.downloadRank_m3
                        ,c.revenuePercentage, c.downloadPercentage, c.moreThan12month
                        ,d.revenueRank AS revenueRank_sep
                        ,d.revenueUSD AS revenueUSD_sep
                        ,d.cumRevenue AS cumRevenue_sep
                        ,d.downloadRank AS downloadRank_sep
                        ,d.download AS download_sep
                        ,d.cumDownload AS cumDownload_sep
                    FROM m3 a
                    JOIN avg_rank_m3 b
                      ON a.appId = b.appId
                    JOIN m3_m12 c
                      ON a.appId = c.appId
                    JOIN sep_rank d
                      ON a.appId = d.appId
                  ",stringsAsFactors = F)  %>%  
              tbl_df() %>%
              arrange(desc(cumRevenue)) %>%
              mutate(category = cut(revenueRank_m3
                                    ,breaks = c(1,10,50,100,200,300, Inf), include.lowest = T
                                    ,labels = c("Top 10", "Top 11-50", "Top 51-100", 
                                                 "Top 101-200", "Top 201-300", "Beyond 300")
                                     )
                     )
              
#### export the final result into Excel ####
write.csv(m3_final, "m3.csv", row.names = F)
