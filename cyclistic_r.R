library(tidyverse) # install package
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(geodist)
library(data.table) # install package
library(scales)


# set wd
getwd()
setwd("D:/R projects/cyclistic-case-study/data")

# create list of all files in this folder
myfiles = list.files(pattern='*.csv', full.names = T) 

# upload all the files and create a data set
all_data = ldply(myfiles, read_csv)

colnames(all_data)
head(all_data)

sapply(all_data, function(x) sum(is.na(x)))
sum(duplicated(all_data$ride_id))

all_data <- all_data %>% 
  mutate(ride_length = difftime(ended_at,started_at, units= 'mins')) %>% 
  mutate(weekday = weekdays(started_at))

# changed data type to numeric from date time
all_data$ride_length <- as.numeric(all_data$ride_length)

# calculate distance in meters using Haversine formula
all_data$dist <- geodist_vec(
  x1 = all_data$start_lng
  , y1 = all_data$start_lat
  , x2 = all_data$end_lng
  , y2 = all_data$end_lat
  , paired = TRUE
  , measure = "haversine"
)

all_data$dist <- all_data$dist/1609 # convert meter to miles
data_copy <- all_data

all_data %>% 
  select(ride_length, dist) %>% 
  summary()

# makes sure distance and ride length is greater than 0
all_data <- all_data %>% 
  filter(ride_length > 0) %>% 
  filter(dist >0 | is.na(dist))

# find summary info based on months
all_data$started_at <- as.Date(all_data$started_at)

all_data$month <- month(ymd(all_data$started_at))

mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")

all_data$month <- mymonths[all_data$month]

all_data %>% 
  group_by(month) %>% 
  dplyr::summarize(n = n()) %>% 
  arrange(desc(n))


# makes sure distance and ride length is greater than 0
# this actually makes rows all NA (incorrect syntax, use filter instead)
all_data <- all_data[all_data$dist > 0,]
all_data <- all_data[all_data$ride_length > 0,]

casual <- subset(all_data, member_casual == 'casual')
member <- subset(all_data, member_casual == 'member')

member %>% 
  select(ride_length, dist) %>% 
  summary()

casual %>% 
  select(ride_length, dist) %>% 
  summary()

casual %>% 
  drop_na(start_station_name) %>% 
  group_by(start_station_name) %>% 
  dplyr::summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

member %>% 
  drop_na(start_station_name) %>% 
  group_by(start_station_name) %>% 
  dplyr::summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)


# analysis
unique(member$rideable_type) #classic, electric
table(member$rideable_type)
#classic: 1,663,850 ; electric: 1,608,156

unique(casual$rideable_type)
table(casual$rideable_type)  
# classic:813414, docked:146764, electric:1205809

# trying to create barchart with just one df
# this runs into the error of not being able to use facetwrap(~member_casual)
all_ridetype <- all_data %>% 
  dplyr::count(rideable_type) %>% 
  mutate(perc = prop.table(n)*100) 

# almost works, just problem with facet_wrap()
ggplot(all_ridetype, aes(x = rideable_type, fill = rideable_type))+
  geom_bar() +
  geom_text(aes(x = rideable_type, y = n,
             label = paste0(n, " (", round(perc,1),"%)")),
             vjust = -0.5) +
  facet_wrap(~member_casual) +
  labs(title = 'Ride Type for Casual Riders vs. Members')+
  theme_bw()
  
# create new df to show percentages
casual_ridetype <- casual %>% 
  dplyr::count(rideable_type) %>% 
  mutate(perc = n/sum(n) *100)

# add commas to the count
casual_ridetype$n <- format(casual_ridetype$n, big.mark = ',', scientific = FALSE)

# plot barchart with count and percents for casual riders
r1 <- ggplot(casual_ridetype, aes(x = rideable_type, y = n, fill=rideable_type)) + 
  geom_col() +
  geom_text(aes(x = rideable_type, y = n,
                    label =paste0(n, " (", round(perc,1),"%)")),
                    vjust = -0.5) +
  labs(title = 'Casual Riders (Bike Types)', y = 'count') +
  scale_fill_manual(values=c("green", "red", "blue"))


# create new df to show percentages
member_ridetype <- member %>% 
  dplyr::count(rideable_type) %>% 
  mutate(perc = prop.table(n) *100)

# add commas to the count
member_ridetype$n <- format(member_ridetype$n, big.mark = ',', scientific = FALSE)

r2 <- ggplot(member_ridetype, aes(x = rideable_type, y = n, fill=rideable_type)) +
  geom_col() +
  geom_text(aes(x = rideable_type, y = n,
                    label =paste0(n, " (", round(perc,1),"%)")),
                    vjust = -0.5) + 
  labs(title = 'Annual Members (Bike Types)', y = 'count') +
  scale_fill_manual(values=c("green", "blue"))

# see values for bike rides for every weekday
table(member$weekday)

# barchart for values for bike rides for every weekday
# are you able to have commas for the values
p1 <- ggplot(member, aes(x = weekday, fill=weekday, label = after_stat(count))) +
  geom_bar() +
  geom_text(stat = 'count', vjust = -0.5, size = 3)

p1 <- p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p1

table(casual$weekday)
p2 <- ggplot(casual, aes(x = weekday, fill=weekday, label = after_stat(count))) +
  geom_bar() +
  geom_text(stat = 'count', vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# HYPOTHESIS TESTING
# sample t-test
casual %>% 
  select(ride_length, dist) %>% 
  summary()
member %>% 
  select(ride_length, dist) %>% 
  summary()

mean(member$dist,na.rm= T) #1.365
mean(casual$dist,na.rm= T) #1.464

#t-test
t.test(member$dist, casual$dist, alternative = 'two.sided')
# 95% confidence: distance for members will be 0.087 to 0.11 miles 
# shorter than distance for casual riders.
# p-value = 0 < 0.05, statistically significant
# This means that casual riders bike for longer distances than members.

mean(member$ride_length,na.rm= T) #12.67
mean(casual$ride_length,na.rm= T) #28.83

t.test(member$ride_length, casual$ride_length, alternative = 'two.sided')
# 95% confidence: ride length is 15.8 to 16.5 mins longer
# for casual riders than for members.
# p-value = 0 < 0.05, statistically significant
# This means that casual riders bike for longer time frames than members.