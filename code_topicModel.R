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
reviews <- cases_data


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 2: LOAD AND SELECT DATA (E.G. AMAZON REVIEWS) 
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# OPTIONAL: Specify minimum text length (number of characters)
reviews <- subset(reviews, reviews$description > 100)

# remove NAs in description
reviews <- reviews[!is.na(reviews$description),]

# OPTIONAL: Further selection (select variable to filter)
# reviews<- subset(reviews, reviews$product == "Fitbit Charge 2")

# OPTIONAL: Create random sample
# reviews <- sample_n(reviews, 1000)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 3: PREPARE TEXT DATA (TEXT PRE-PROCESSING)
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Remove all "-" where they are not needed
reviews$description <- gsub("-", " ", reviews$description)

# Transform words into tokens, select basic text preprocessing steps
tokens <- tokens(reviews$description,
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
reviews_lda <- LDA(myDfm, k = 6, control = list(seed = 123))
topics <- as.data.frame(terms(reviews_lda, 50))

# Convert into tidy-format to visualize results 
reviews_lda_td <- tidy(reviews_lda)

# Extract top-terms per topic 
top_terms <- reviews_lda_td %>%
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
tmResult <- posterior(reviews_lda)
theta <- tmResult$topics
lda_results <- cbind(reviews, theta)
rm (theta, reviews_lda,reviews_lda_td,tmResult,top_terms,tokens)



# add all rows from cases_data to lda_results, where number is missing and add the value 0 to all topics

lda_results <- lda_results[-2] %>% 
  full_join(cases_data, by = "number")

# fill NA with 0
lda_results[is.na(lda_results)] <- 0

# remove last column
lda_results <- lda_results[-ncol(lda_results)]

# Rename columns
colnames(lda_results) <- c("number", "Desktop_Issues", "VDI_Troubleshooting", "Functionality_Problems", "Hardware_Concerns", "Login_Access", "Email_Communication")

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
### Wordclouds                                                                                        
### (c) Patrick Cichy, Berner Fachhochschule BFH
###################################################################################################

# Create a wordcloud from your Document-Feature-Matrix
textplot_wordcloud(myDfm,
                   min_size = 0.5,
                   max_size = 4,
                   min_count = 10,
                   max_words = 300,
                   color = "darkblue")

