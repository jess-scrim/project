source("scripts/00_setting_up.R")

# Tokenise
abstract_unigrams <- tidy_abstracts %>% 
  mutate(type = case_when(date <= leca_approv ~ "pre-leca",
                          date > leca_approv ~ "post-leca")) %>% 
  unnest_tokens(word, text)

# Remove stopwords
data(stop_words)

abstract_unigrams_clean <- abstract_unigrams %>%
  anti_join(stop_words)
abstract_unigrams_clean <- abstract_unigrams_clean %>%
  anti_join(my_stopwords)

# save tidy_abstracts_clean to data file
abstract_unigrams_clean %>%
  write_csv("results/abstract_unigrams_clean.csv")

# Plot most common words among all abstracts
abstract_unigrams_clean %>%  
  count(word, sort = TRUE) %>%
  filter(n > 2500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
