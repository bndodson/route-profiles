#Route Profiles Data Cleaning
#Ben Dodson
#8 May 2020
#(c) SEPTA 2020

library(readr)
library(dplyr)
library(lubridate)
library(tidyverse)

setwd("C:/Users/bndodson/Documents/GitHub/route-profiles")

LUCYGOLD_TL <- read_csv("data/LUCYGOLD_TRIPLIST.csv")
RT16_TL <- read_csv("data/RT16_TRIPLIST.csv")
RT18_TL <- read_csv("data/RT18_TRIPLIST.csv")
RT21_TL <- read_csv("data/RT21_TRIPLIST.csv")
RT23_TL <- read_csv("data/RT23_TRIPLIST.csv")
RT42_TL <- read_csv("data/RT42_TRIPLIST.csv")
RT310_TL <- read_csv("data/RT310_TRIPLIST.csv")
TL_data <- rbind(LUCYGOLD_TL, RT16_TL, RT21_TL, RT23_TL, RT42_TL, RT310_TL)
names(TL_data) <- make.names(names(TL_data), unique=TRUE)
TL_data <- subset(TL_data, Service.Type=='WEEKDAY')
TL_data <- TL_data[complete.cases(TL_data), ]

TL_data$Period[TL_data$Period=='Early AM'] <- '1-Early AM'
TL_data$Period[TL_data$Period=='AM Peak'] <- '2-AM Peak'
TL_data$Period[TL_data$Period=='Midday'] <- '3-Midday'
TL_data$Period[TL_data$Period=='PM Peak'] <- '4-PM Peak'
TL_data$Period[TL_data$Period=='Evening'] <- '5-Evening'
TL_data$Period[TL_data$Period=='Late Night'] <- '6-Late Night'

TL_data$Duration <- hms(TL_data$Avg..Time)
TL_data$Duration <- (hour(TL_data$Duration)*60)+minute(TL_data$Duration)+(second(TL_data$Duration)/60)

TL_data <- TL_data %>%
  group_by(Route, Period) %>%
  summarise(mean_ons = mean(Avg..Ons), mean_duration = mean(Duration), mean_ppvh = mean(Avg..P.Vh), 
            mean_dist = mean(Avg..Dist),  mean_mph = mean(Avg..M.H), mean_ppm = mean(Avg..P.M),
            mean_pm = mean(Avg..PM), max_load = max(Max.Load))
TL_data <- TL_data %>% 
  mutate_if(is.numeric, round)

write.csv(TL_data, "data.csv")

OTP_hist <- read_csv("data/OTP_hist.csv")
OTP_hist_long <- OTP_hist %>% 
  pivot_longer(c(`pct_early`, `pct_ontime`, `pct_late`), names_to = "category", values_to = "value")
OTP_hist_long$Date[OTP_hist_long$Month=='apr_19'] <- '2019-04'
OTP_hist_long$Date[OTP_hist_long$Month=='may_19'] <- '2019-05'
OTP_hist_long$Date[OTP_hist_long$Month=='jun_19'] <- '2019-06'
OTP_hist_long$Date[OTP_hist_long$Month=='jul_19'] <- '2019-07'
OTP_hist_long$Date[OTP_hist_long$Month=='aug_19'] <- '2019-08'
OTP_hist_long$Date[OTP_hist_long$Month=='sep_19'] <- '2019-09'
OTP_hist_long$Date[OTP_hist_long$Month=='oct_19'] <- '2019-10'
OTP_hist_long$Date[OTP_hist_long$Month=='nov_19'] <- '2019-11'
OTP_hist_long$Date[OTP_hist_long$Month=='dec_19'] <- '2019-12'
OTP_hist_long$Date[OTP_hist_long$Month=='jan_20'] <- '2020-01'
OTP_hist_long$Date[OTP_hist_long$Month=='feb_20'] <- '2020-02'
OTP_hist_long$Date[OTP_hist_long$Month=='mar_20'] <- '2020-03'

write.csv(OTP_hist_long, "data/OTP_hist_long.csv")


#Key
#H: Avg. Ons: mean_ons
#I: Avg. Offs: N/A
#J: Avg. Time: mean_duration
#K: Avg. P/Vh: mean_ppvh
#L: Avg. Dist: mean_dist
#M: Avg. M/H: mean_mph
#N: Avg. P/M: mean_ppm
#O: Avg. PM: mean_pm
#P: Max Load: max_load