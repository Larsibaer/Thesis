# History Line Test
libraries = c( "fpp3", "ggplot2", "dplyr", "tidyr", "readxl", "forecast", "zoo", "tsibble", "GGally", "lubridate", "tidyverse", "stringi")

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(libraries, library, quietly = TRUE, character.only = TRUE)

rm(list=ls())

setwd("C:/Thesis")

#Filter: https://kiwi.unico.ch/now/nav/ui/classic/params/target/sys_history_line_list.do%3Fsysparm_query%3Dset.tableSTARTSWITHsn_cus%255Efield%253Dstate%255EORfield%253Dassignment_group%255EORfield%253Dassigned_to%255Eset.initial_valuesLIKE%253Cu_case_type%253ETrouble%253C%252Fu_case_type%253E%255EORDERBYset%26sysparm_first_row%3D1%26sysparm_view%3D
history_data <- read_csv("Data/sys_history_line.csv", locale = locale(encoding = "UTF-8"))
data <- history_data

data$set <- trimws(gsub("Case:", " ", data$set))
data$update_time <- as.POSIXct(strptime(data$update_time, "%d-%m-%Y %H:%M:%S", tz="UTC"))

data_all <- read_csv("Data/data.csv", locale = locale(encoding = "UTF-8"))
#Merge opened and closed from data_new to data by number
data_all <- select(data_all, number, account, opened, closed, openedToClosed)
data <- merge(data, data_all, by.x = "set", by.y = "number", all.x = FALSE)

# Add Column for the time difference between opened_at and update_time
data <- data %>%
  mutate(time_diff_opened = as.numeric(update_time - opened))

# Add Column for the time difference between update_time and the previous update_time for state and assignment_group

data_state <- data %>%
  filter(field == "state") %>%
  group_by(set) %>%
  arrange(update_time) %>%
  mutate(time_diff_previous_state = (as.numeric(update_time - lag(update_time))/3600)) %>%
  mutate(sum_time_diff_state = sum(time_diff_previous_state, na.rm = TRUE)) %>%
  mutate(percentage_time_diff_state = time_diff_previous_state / sum_time_diff_state * 100) %>%
  ungroup()

data_assignment_group <- data %>%
  filter(field == "assignment_group") %>%
  group_by(set) %>%
  arrange(update_time) %>%
  mutate(time_diff_previous_assignment_group = (as.numeric(update_time - lag(update_time))/3600)) %>%
  mutate(sum_time_diff_assignment_group = sum(time_diff_previous_assignment_group, na.rm = TRUE)) %>%
  mutate(percentage_time_diff_assignment_group = time_diff_previous_assignment_group / sum_time_diff_assignment_group * 100) %>%
  ungroup()

data_assigned_to <- data %>%
  filter(field == "assigned_to") %>%
  group_by(set) %>%
  arrange(update_time) %>%
  mutate(time_diff_previous_assigned_to = (as.numeric(update_time - lag(update_time))/3600)) %>%
  mutate(sum_time_diff_assigned_to = sum(time_diff_previous_assigned_to, na.rm = TRUE)) %>%
  mutate(percentage_time_diff_assigned_to = time_diff_previous_assigned_to / sum_time_diff_assigned_to * 100) %>%
  ungroup()





# Remove all entries where update_time after closed_at
data <- data %>% filter(update_time <= closed)
#List of field values that will be filtered
field_values <- c("state", "assignment_group")

# Only remain rows where field value is in field_values
data <- data %>% filter(field %in% field_values)

data$new <- stringi::stri_replace_all_fixed(
  data$new,
  c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
  vectorize_all = FALSE
)

# Replace space with _ in column Assignment Group
data$new <- gsub(" ", "_", data$new)
# Make new Column with merging field and new
data <- data %>% mutate(fields = paste(field, new, sep = "_"))

one_hot_encode <- function(df, columns) {
  # Ensure 'columns' is a character vector
  if(!is.character(columns)) {
    stop("columns argument must be a character vector specifying column names to encode")
  }
  
  # Check if specified columns exist in the dataframe
  if(!all(columns %in% names(df))) {
    stop("Not all specified columns exist in the dataframe")
  }
  
  # Start with the original dataframe minus the columns to be encoded
  result_df <- df[, !(names(df) %in% columns)]
  
  # Iterate over the columns and perform one-hot encoding
  for(column in columns) {
    # Create a temporary dataframe to avoid altering the original data
    temp_df <- data.frame(df[[column]], stringsAsFactors = FALSE)
    colnames(temp_df) <- column
    
    # Apply model.matrix(), remove intercept column, and convert to dataframe
    encoded_matrix <- model.matrix(~ . - 1, data = temp_df)
    encoded_df <- data.frame(encoded_matrix)
    
    # Rename encoded columns to include original column name for clarity
    colnames(encoded_df) <- gsub("X", column, colnames(encoded_df))
    
    # Combine with the result dataframe
    result_df <- cbind(result_df, encoded_df)
  }
  
  return(cbind(df[1], result_df))
}
# One-hot encoding
encoded_df <- one_hot_encode(data, c("fields"))

#replace all 1 in encode_df[23:46] with the time_diff_previous
encoded_df[23:46] <- encoded_df[23:46] * encoded_df$time_diff_previous





data <- select(data, set, opened_at, closed_at, update_time,time_diff_opened,time_diff_previous, user_id, field, old, new)


data_ts <- data %>%
  distinct(set, update_time, field, .keep_all = TRUE) %>%
  pivot_wider(
    names_from = field, 
    values_from = c(old, new),
    names_glue = "{field}_{.value}"
  ) %>%
  select(update_time, set, user_id, sort(tidyselect::peek_vars()))



# data_ts <- data_ts %>% arrange(set, update_time) %>%
#   # Group by set to ensure filling is done within each set
#   group_by(set) %>%
#   fill(state_new, assignment_group_new, assigned_to_new, .direction = "downup") %>%
#   # Fill NA values in the new columns with the old values from the previous row
#   # Ungroup to avoid unexpected grouping behavior later
#   ungroup()

print(data_ts)

outlier <- function(x){
  quantiles <- quantile(x, c(.05, .95))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}

#if time_diff_previous is NA, then set it to 0
data_ts$time_diff_previous[is.na(data_ts$time_diff_previous)] <- 0


#Use function outlier for the dataset
data_ts['time_diff_previous'] <- map_df(data_ts['time_diff_previous'], outlier)
data_ts['time_diff_opened'] <- map_df(data_ts['time_diff_opened'], outlier)

# Gerenate a time series plot
data_ts %>%
  ggplot(aes(x = update_time, y = time_diff_previous)) +
  geom_line() +
  labs(title = "Time difference between opened_at and update_time",
       x = "Time",
       y = "Time difference (in seconds)") +
  theme_minimal()


# Plot timeline
data_ts %>%
  filter(!is.na(assignment_group_old)) %>%
  ggplot(aes(x = update_time, y = assignment_group_new)) +
  geom_point() +
  labs(title = "Timeline of updates per group",
       x = "Time",
       y = "Group") +
  theme_minimal()

# Plot Histogramm of time_diff_previous
data_ts %>%
  ggplot(aes(x = time_diff_previous)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Histogram of time difference between update_time and the previous update_time",
       x = "Time Since Last Action (in seconds)",
       y = "Frequency") +
  theme_minimal()

# Analyze only when the assignment_group_old is not NA. Plot mean_diff_previous per Changes from assignment_group
data_ts %>%
  filter(!is.na(assignment_group_new)) %>%
  group_by(assignment_group_new) %>%
  filter(n() > 10) %>%
  summarise(mean_time_diff_previous = mean(time_diff_previous, na.rm = TRUE)) %>%
  ggplot(aes(x = assignment_group_new, y = mean_time_diff_previous)) +
  geom_col() +
  labs(title = "Mean time difference between update_time and the previous update_time per assignment_group",
       x = "Assignment Group",
       y = "Mean time difference (in seconds)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


data_ts %>%
  filter(!is.na(assigned_to_old)) %>%
  group_by(assigned_to_new) %>%
  summarise(mean_time_diff_previous = mean(time_diff_previous, na.rm = TRUE)) %>%
  ggplot(aes(x = assigned_to_new, y = mean_time_diff_previous)) +
  geom_col() +
  labs(title = "Mean time difference between update_time and the previous update_time per assignment_group",
       x = "Assigned to",
       y = "Mean time difference (in seconds)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
