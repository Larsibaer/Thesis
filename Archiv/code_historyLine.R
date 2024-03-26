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

# Remove all entries where update_time after closed_at. Because Flows can Change fields after they are closed
data <- data %>% filter(update_time <= closed)

# Add Column for the time difference between opened_at and update_time
data <- data %>%
  mutate(time_diff_opened = as.numeric(update_time - opened))

# Add Column for the time difference between update_time and the previous update_time for state and assignment_group

data_state <- data %>%
  filter(field == "state") %>%
  filter(!is.na(new)) %>%
  group_by(set) %>%
  arrange(update_time) %>%
  mutate(time_diff_previous_state = (as.numeric(update_time - lag(update_time))/3600)) %>%
  mutate(sum_time_diff_state = sum(time_diff_previous_state, na.rm = TRUE)) %>%
  mutate(percentage_time_diff_state = time_diff_previous_state / sum_time_diff_state * 100) %>%
  select(set, new, percentage_time_diff_state) %>%
  ungroup()

data_assignment_group <- data %>%
  filter(field == "assignment_group") %>%
  filter(!is.na(new)) %>%
  group_by(set) %>%
  arrange(update_time) %>%
  mutate(time_diff_previous_assignment_group = (as.numeric(update_time - lag(update_time))/3600)) %>%
  mutate(sum_time_diff_assignment_group = sum(time_diff_previous_assignment_group, na.rm = TRUE)) %>%
  mutate(percentage_time_diff_assignment_group = time_diff_previous_assignment_group / sum_time_diff_assignment_group * 100) %>%
  select(set, new, percentage_time_diff_assignment_group) %>%
  ungroup()

data_assigned_to <- data %>%
  filter(field == "assigned_to") %>%
  filter(!is.na(new)) %>%
  group_by(set) %>%
  arrange(update_time) %>%
  mutate(time_diff_previous_assigned_to = (as.numeric(update_time - lag(update_time))/3600)) %>%
  mutate(sum_time_diff_assigned_to = sum(time_diff_previous_assigned_to, na.rm = TRUE)) %>%
  mutate(percentage_time_diff_assigned_to = time_diff_previous_assigned_to / sum_time_diff_assigned_to * 100) %>%
  select(set, new, percentage_time_diff_assigned_to) %>%
  ungroup()






#List of field values that will be filtered
field_values <- c("state", "assignment_group", "assigned_to")

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
# encoded_df <- one_hot_encode(data, c("fields"))
encoded_df_state <- one_hot_encode(data_state, c("new"))
encoded_df_assignmet_group <- one_hot_encode(data_assignment_group, c("new"))
encoded_df_assigned_to <- one_hot_encode(data_assigned_to, c("new"))

#remove second column
encoded_df_state <- encoded_df_state[,-2]
encoded_df_assignmet_group <- encoded_df_assignmet_group[,-2]
encoded_df_assigned_to <- encoded_df_assigned_to[,-2]

#replace all 1 with the time_diff_previous
encoded_df_state[,c(-1,-2)] <- encoded_df_state[,c(-1,-2)] * encoded_df_state$percentage_time_diff_state
encoded_df_assignmet_group[,c(-1,-2)] <- encoded_df_assignmet_group[,c(-1,-2)] * encoded_df_assignmet_group$percentage_time_diff_assignment_group
encoded_df_assigned_to[,c(-1,-2)] <- encoded_df_assigned_to[,c(-1,-2)] * encoded_df_assigned_to$percentage_time_diff_assigned_to

#Round to 2 decimal places
encoded_df_state[,-1] <- round(encoded_df_state[,-1], 2)
encoded_df_assignmet_group[,-1] <- round(encoded_df_assignmet_group[,-1], 2)
encoded_df_assigned_to[,-1] <- round(encoded_df_assigned_to[,-1], 2)

#Group by set and sum all columns
grouped_encoded_df_state <- encoded_df_state %>%
  group_by(set) %>%
  summarise(across(c(-1), sum, na.rm = TRUE)) %>%
  ungroup()

grouped_encoded_df_assignmet_group <- encoded_df_assignmet_group %>%
  group_by(set) %>%
  summarise(across(c(-1), sum, na.rm = TRUE)) %>%
  ungroup()

grouped_encoded_df_assigned_to <- encoded_df_assigned_to %>%
  group_by(set) %>%
  summarise(across(c(-1), sum, na.rm = TRUE)) %>%
  ungroup()

#Replace all columns header new with "percentage"
colnames(grouped_encoded_df_state) <- gsub("new", "prop_state_", colnames(grouped_encoded_df_state))
colnames(grouped_encoded_df_assignmet_group) <- gsub("new", "prop_group_", colnames(grouped_encoded_df_assignmet_group))
colnames(grouped_encoded_df_assigned_to) <- gsub("new", "prop_user_", colnames(grouped_encoded_df_assigned_to))

#Merge all dataframes
final_df <- merge(grouped_encoded_df_state, grouped_encoded_df_assignmet_group, by = "set", all = TRUE)
final_df <- merge(final_df, grouped_encoded_df_assigned_to, by = "set", all = TRUE)

#Rename set to number
colnames(final_df)[1] <- "number"
