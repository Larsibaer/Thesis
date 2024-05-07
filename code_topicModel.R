###################################################################################################
### TOPIC MODELING                                                                                        
### (c) Patrick Cichy, Berner Fachhochschule BFH
###################################################################################################

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
### STEP 1: LOAD PACKAGES AND SET WORKING DIRECTORY
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(tidyverse)
library(quanteda)
library(stopwords)
library(topicmodels)
library(tidytext)
library(quanteda.textplots)
options(scipen=999)

rm(list=ls())

# Set working directory
setwd("C:/Thesis")
#read json file
cases_data <- read_csv("Data/TopicModel_VectorizedText_description.csv", locale = locale(encoding = "UTF-8"))
descriptions <- cases_data


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 2: LOAD AND SELECT DATA (E.G. AMAZON descriptions) 
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# OPTIONAL: Specify minimum text length (number of characters)
descriptions <- subset(descriptions, descriptions$description > 100)

# remove NAs in description
descriptions <- descriptions[!is.na(descriptions$description),]

# OPTIONAL: Further selection (select variable to filter)
# descriptions<- subset(descriptions, descriptions$product == "Fitbit Charge 2")

# OPTIONAL: Create random sample
# descriptions <- sample_n(descriptions, 1000)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 3: PREPARE TEXT DATA (TEXT PRE-PROCESSING)
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Remove all "-" where they are not needed
descriptions$description <- gsub("-", " ", descriptions$description)

# Transform words into tokens, select basic text preprocessing steps
tokens <- tokens(descriptions$description,
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_numbers = TRUE,
                 remove_url = TRUE,
                 remove_separators = TRUE)


# Create n-grams of any length
tokens <- tokens_ngrams(tokens, n = 1:2)

# Create Document-feature-matrix
myDfm <-dfm(tokens)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 4: ANALYZE TEXT
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create LDA model (specify number of topics)
descriptions_lda <- LDA(myDfm, k = 7, control = list(seed = 123))
topics <- as.data.frame(terms(descriptions_lda, 50))

# Convert into tidy-format to visualize results 
descriptions_lda_td <- tidy(descriptions_lda)

# Extract top-terms per topic 
top_terms <- descriptions_lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualize top-terms and their loadings (can you assign topic labels based on this information?) 
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Link results to metadata
tmResult <- posterior(descriptions_lda)
theta <- tmResult$topics
lda_results <- cbind(descriptions, theta)
rm (theta,descriptions_lda_td,tmResult,top_terms,tokens)



# add all rows from cases_data to lda_results, where number is missing and add the value 0 to all topics

lda_results <- lda_results[-2] %>% 
  full_join(cases_data, by = "number")

# fill NA with 0
lda_results[is.na(lda_results)] <- 0

# remove last column
lda_results <- lda_results[-ncol(lda_results)]

# Rename columns
colnames(lda_results) <- c("number", "topic_network_server", "topic_performance_responseIssues", "topic_vdi_hostedDesktop", "topic_authentication_accounts", "topic_officeApplications", "topic_printing_drive", "topic_support_infrastructure")

# Save the final dataframe
write_csv(lda_results, "Data/topicModel_description.csv")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 5: GO BACK TO STEP 3 AND 4 TO RECONSIDER PREPROCESSING, STOPWORDS AND THE NUMBER OF TOPICS. 
###         ITERATE MULTIPLE TIMES AND OBSERVE HOW RESULTS CHANGE. GOOD LUCK! 
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### OPTIONAL: CHOOSE THE BEST NUMBER OF TOPICS BASED ON A METRIC
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library (ldatuning) 
### Calculate different metrics to estimate the most preferable number of topics for LDA model
## Be aware: The procedure is computation intensive 
# ldatuning uses parallelism, specify the correct number of CPU cores in mc.core parameter to archive best performance

# Calculate selected metrics
result <- FindTopicsNumber(
  myDfm,
  topics = seq(from = 2, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE)

# plot results
FindTopicsNumber_plot(result)



###################################################################################################
### All Text Information                                                                                        
###################################################################################################

close_notes <- read_csv("Data/TopicModel_VectorizedText_close_notes.csv", locale = locale(encoding = "UTF-8"))
cause <- read_csv("Data/TopicModel_VectorizedText_cause.csv", locale = locale(encoding = "UTF-8"))

# Combine all text information
all_text <- merge(close_notes, cause, by = "number", all = TRUE)
all_text <- merge(all_text, descriptions, by = "number", all = TRUE)

# Paste all text information together
all_text$all_text <- paste(all_text$close_notes, all_text$cause, all_text$description, sep = " ")

#Replace Text containing LOG with ''
all_text$all_text <- gsub("NA", "", all_text$all_text)

# Trim white spaces at beginning and end of text
all_text$all_text <- trimws(all_text$all_text)

# Fill empty text with NA
all_text$all_text[all_text$all_text == ""] <- NA

# Remove column close_notes, cause and description
all_text <- all_text[-2]
all_text <- all_text[-2]
all_text <- all_text[-2]




### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 2: LOAD AND SELECT DATA (E.G. AMAZON descriptions) 
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# OPTIONAL: Specify minimum text length (number of characters)
all_text <- subset(all_text, all_text$all_text > 100)

# remove NAs in description
all_text <- all_text[!is.na(all_text$all_text),]

# Transform words into tokens, select basic text preprocessing steps
new_tokens <- tokens(all_text$all_text,
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     remove_separators = TRUE)


# Create n-grams of any length
new_tokens <- tokens_ngrams(new_tokens, n = 1:2)

# Create Document-feature-matrix for new data
new_myDfm <- dfm(new_tokens, verbose = FALSE)

# Apply the trained LDA model to the new data
new_descriptions_lda <- LDA(new_myDfm, model = descriptions_lda)

# Extract topics distribution for the new data
new_topics <- as.data.frame(terms(new_descriptions_lda, 50))

# Link results to metadata
tmResult <- posterior(new_descriptions_lda)
theta <- tmResult$topics
lda_results2 <- cbind(all_text, theta)
rm (theta,descriptions_lda_td,tmResult,top_terms,tokens)

# add all rows from cases_data to lda_results, where number is missing and add the value 0 to all topics

lda_results2 <- lda_results2[-2]

# fill NA with 0
lda_results2[is.na(lda_results)] <- 0


# Rename columns
colnames(lda_results2) <- c("number", "topic_network_server", "topic_performance_responseIssues", "topic_vdi_hostedDesktop", "topic_authentication_accounts", "topic_officeApplications", "topic_printing_drive", "topic_support_infrastructure")

# Save the final dataframe
write_csv(lda_results2, "Data/topicModel_allText.csv")

