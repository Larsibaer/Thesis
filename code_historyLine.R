# History Line Test
libraries = c( "fpp3", "ggplot2", "dplyr", "tidyr", "readxl", "forecast", "zoo", "tsibble", "GGally", "lubridate")

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(libraries, library, quietly = TRUE, character.only = TRUE)

rm(list=ls())

setwd("C:/Thesis")

history_data <- read_csv("Data/sys_history_line.csv", locale = locale(encoding = "UTF-8"))
data <- history_data

data$set <- trimws(gsub("Case:", " ", data$set))
data$update_time <- as.POSIXct(strptime(data$update_time, "%d-%m-%Y %H:%M:%S", tz="UTC"))

data_all <- read_csv("Data/sn_customerservice_case_utf8_old.csv", locale = locale(encoding = "UTF-8"))
#Merge opened and closed from data_new to data by number
data_all <- select(data_all, number, account, opened_at, closed_at)
data_all$opened_at <- as.POSIXct(strptime(data_all$opened_at, "%d-%m-%Y %H:%M:%S", tz="UTC"))
data_all$closed_at <- as.POSIXct(strptime(data_all$closed_at, "%d-%m-%Y %H:%M:%S", tz="UTC"))
data <- merge(data, data_all, by.x = "set", by.y = "number", all.x = FALSE)

# Add Column for the time difference between opened_at and update_time
data <- data %>%
  mutate(time_diff_opened = as.numeric(update_time - opened_at))

# Add Column for the time difference between update_time and the previous update_time
data <- data %>%
  group_by(set) %>%
  arrange(update_time) %>%
  mutate(time_diff_previous = as.numeric(update_time - lag(update_time))) %>%
  ungroup()

#Change time_diff_previous to hours
data$time_diff_opened <- data$time_diff_opened/3600
data$time_diff_previous <- data$time_diff_previous/3600

# Remove all entries where update_time after closed_at
data <- data %>% filter(update_time <= closed_at)

#List of field values that will be filtered
field_values <- c("state", "assignment_group", "assigned_to", "u_total_time_worked", "comments")

# Only remain rows where field value is in field_values
data <- data %>% filter(field %in% field_values)
data <- select(data, set, opened_at, closed_at, update_time,time_diff_opened,time_diff_previous, user_id, field, old, new)


data_ts <- data %>%
  distinct(set, update_time, field, .keep_all = TRUE) %>%
  pivot_wider(
    names_from = field, 
    values_from = c(old, new),
    names_glue = "{field}_{.value}"
  ) %>%
  select(update_time, set, user_id, sort(tidyselect::peek_vars()))



data_ts <- data_ts %>% arrange(set, update_time) %>%
  # Group by set to ensure filling is done within each set
  group_by(set) %>%
  fill(state_new, assignment_group_new, assigned_to_new, u_total_time_worked_new, comments_new, .direction = "downup") %>%
  # Fill NA values in the new columns with the old values from the previous row
  # Ungroup to avoid unexpected grouping behavior later
  ungroup()

print(data_ts)

# Gerenate a time series plot
data_ts %>%
  ggplot(aes(x = update_time, y = time_diff_previous)) +
  geom_line() +
  labs(title = "Time difference between opened_at and update_time",
       x = "Time",
       y = "Time difference (in seconds)") +
  theme_minimal()

# Plot mean time difference between opened_at and update_time per assignment_group
data_ts %>%
  group_by(assignment_group_new) %>%
  summarise(mean_time_diff_opened = mean(time_diff_opened, na.rm = TRUE)) %>%
  ggplot(aes(x = assignment_group_new, y = mean_time_diff_opened, fill )) +
  geom_col() +
  labs(title = "Mean time difference between opened_at and update_time per assignment_group",
       x = "Assignment Group",
       y = "Mean time difference (in seconds)") +
  theme_minimal()

# group by assignment group and calculate the mean time difference between opened_at and update_time
data_ts %>%
  group_by(assignment_group_new) %>%
  summarise(mean_time_diff_opened = mean(time_diff_opened, na.rm = TRUE)) %>%
  ggplot(aes(x = assignment_group_new, y = mean_time_diff_opened)) +
  geom_col() +
  labs(title = "Mean time difference between opened_at and update_time per assignment_group",
       x = "Assignment Group",
       y = "Mean time difference (in seconds)") +
  theme_minimal()

#----------------------
set.seed(12103)

# Select a random Series ID using the sample() function
id <- sample(unique(aus_retail$`Series ID`), 1)

# Filter the tsibble to extract the corresponding time series
A3349564W  <- aus_retail |>
  filter(`Series ID` == id)
