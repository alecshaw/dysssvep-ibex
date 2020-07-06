# DYSSSVEP Word Plausibility Pretest
# Alec Shaw <alecshaw@hi.is>
# 2020-06-29

library(tidyverse)

# Read data --------------------------------------------------------------------
data <- read.csv("results/results_20200629.csv", header = FALSE,
                 comment.char = "#",
                 colClasses = c("integer", "character", "character", "integer",
                                "NULL", "character", "NULL", "character",
                                "integer", "NULL", "integer"),
                 col.names = c("time", "MD5", "controller", "item", "NULL",
                               "condition", "NULL", "prompt", "rating", "NULL",
                               "RT"))

# Format data ------------------------------------------------------------------
items <- subset(data, is.na(data$rating))
responses <- subset(data, !is.na(data$rating))

# Change "prompt" column name to "word"
colnames(items)[6] <- "word"

subj_count <- length(unique(responses$MD5))

# Join "word" to "responses" data frame
responses$word <- items$word

responses <- separate(responses, condition,
                      into = c("condition_num", "condition"), sep = "(?!\\d+)")

responses$condition <- factor(responses$condition,
                              labels = c("Non-word", "Pseudoword", "Word"))

str(responses)

# Visualizations ---------------------------------------------------------------

theme_set(theme_light())

ggplot(responses, aes(x = rating, fill = condition)) +
  geom_bar(position = "dodge")

condition_ratings <- group_by(responses, condition) %>%
  summarize(n = length(rating), mean = mean(rating), sd = sd(rating))
condition_ratings

word_ratings <- group_by(responses, word, condition) %>%
  summarize(n = length(rating), mean = mean(rating), sd = sd(rating))
word_ratings

# Mean
ggplot(word_ratings, aes(x = condition, y = mean, label = word,
                         color = condition)) +
  geom_text(position = "jitter") +
  labs(title = "Mean rating by condition", x = "Condition", y = "Mean rating") +
  guides(color = FALSE)

# Standard deviation
ggplot(word_ratings, aes(x = condition, y = sd, label = word,
                         color = condition)) +
  geom_text(position = "jitter") +
  labs(title = "Standard deviation by condition", x = "Condition",
       y = "Standard deviation") +
  guides(color = FALSE)

# What pseudowords are within one SD of the pseudoword mean?
word_ratings %>%
  filter(condition == "Pseudoword", mean >= 5)

word_ratings %>%
  filter(condition == "Pseudoword", mean <= 2)
