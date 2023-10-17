source("scripts/00_setting_up.R")
source("scripts/01_data_cleaning.R")

# tidy text
tidy_abstracts <- abstracts %>% 
  unnest_tokens(word, text)

# remove stopwords
data(stop_words)
my_stopwords <- tibble(word = c("alzheimer's", "ad"))

tidy_abstracts_clean <- tidy_abstracts %>%
  anti_join(stop_words)
tidy_abstracts_clean <- tidy_abstracts_clean %>%
  anti_join(my_stopwords)

# OR
# find most common words
words <- tidy_abstracts %>%
  count(word, sort = TRUE) %>% 
  filter(n > 50) %>% 
  select(word)
# manually remove most common words as stop words
tidy_abstracts_clean <- tidy_abstracts %>% 
  anti_join(words, by = "word")

# Plot most common words among all abstracts
tidy_abstracts_clean %>%  
  count(word, sort = TRUE) %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
