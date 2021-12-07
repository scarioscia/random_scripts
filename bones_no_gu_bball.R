# Bones and no bones vs. Georgetown basketball schedule 

# bones data: https://www.digitalmomblog.com/is-it-a-bones-day/
# GU basketball data: https://guhoyas.com/sports/mens-basketball/schedule/2021-22

#import libraries 
library(dplyr)

# read in data - not in good shape 
bones_data <- read.table("/Users/saracarioscia/mccoy-lab/random_project_seeds/bones_days.txt", header = FALSE, fill = TRUE)
bones_or_no_days <- bones_data

# Georgetown's season started November 13th so only keep rows with November and December in the first two columns 
bones_or_no_days <- bones_or_no_days[which(bones_or_no_days$V2 %in% c("November", "December")),]

# save the month and date in a new column and remove original date info
bones_or_no_days <- bones_or_no_days %>%
  mutate(month = V2) %>% 
  mutate(date = V3) %>%
  select(-c(1:4))

# change date format to match that of Georgetown 
bones_or_no_days$date <- as.numeric(gsub(",", "", bones_or_no_days$date))
bones_or_no_days[bones_or_no_days$month == "November",]$month <- 11
bones_or_no_days[bones_or_no_days$month == "December",]$month <- 12
bones_or_no_days <- bones_or_no_days %>% unite('Merged', month, date, sep = "/")

# subset to just game days 
gu_games <- c("11/13", "11/16", "11/19", "11/25", "11/26", "11/30", "12/5")
bones_or_no_days_games <- bones_or_no_days[bones_or_no_days$Merged %in% gu_games,]

# n/a indicates no video; replace string n/a with the NA value 
bones_or_no_days_games[bones_or_no_days_games == "n/a"] <- NA

# add info on bones and GU wins
# ideally would automate for more games 
# 1 is bones, 0 is no bones, NA is no video 
bones_or_no_days_games$bones <- c(NA, 0, NA, 1, 0, 1, NA)
# 1 is win, 0 is loss 
gu_record <- c(0, 1, 1, 0, 0, 1, 0) 
bones_or_no_days_games$GU_win <- gu_record

# make summary tables
# when there was no video
bones_or_no_days_games %>% select(bones, GU_win) %>%
  filter(is.na(bones)) %>% 
  count(GU_win)
# bones day 
bones_or_no_days_games %>% select(bones, GU_win) %>%
  filter(bones == 1) %>% 
  count(GU_win)
# no bones day 
bones_or_no_days_games %>% select(bones, GU_win) %>%
  filter(bones == 0) %>% 
  count(GU_win)

