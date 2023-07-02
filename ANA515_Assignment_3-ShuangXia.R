# ANA515 Assignment 3 - Shuang Xia

# load libraries
library(tidyverse)
library(datasets)
library(ggplot2)

# read the csv
data <- read_csv("StormEvents_details-ftp_v1.0_d1991_c20220425.csv")

# select variables
myvars <- c("BEGIN_YEARMONTH", "EPISODE_ID", "STATE", "STATE_FIPS", "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE")
newdata <- data[myvars]

# arrange by STATE
newdata <- arrange(newdata, STATE)

# change state and county names to title case
newdata$STATE <- str_to_title(data$STATE)
newdata$CZ_NAME <- str_to_title(data$CZ_NAME)

# limit to CZ_TYPE type C
newdata <- newdata %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE)

# pad and combine state and county FIPS code
newdata$STATE_FIPS <- str_pad(newdata$STATE_FIPS, width = 2, side = "left", pad = "0")
newdata$CZ_FIPS <- str_pad(newdata$CZ_FIPS, width = 3, side = "left", pad = "0")
newdata <- unite(newdata, "fips", STATE_FIPS, CZ_FIPS, sep = "", remove = TRUE)

# change column names to lower case
newdata <- rename_all(newdata, tolower)

# retrieve state info from base R
data("state")
us_state_info<-data.frame(state=state.name, region=state.region, area=state.area)

# find number of events per state with state info
events_per_state <- data.frame(table(newdata$state))
names(events_per_state) <- c("state", "number_of_events")
merged_data <- merge(us_state_info, events_per_state, by.x = "state", by.y = "state", all = FALSE)


# plot land area vs storm count in 1991
storm_plot <- ggplot(merged_data, aes(x = area, y = number_of_events)) +
  geom_point(aes(color = region)) +
  labs(x = "Land area (square miles)",
       y = "# of storm events in 1991")
storm_plot
