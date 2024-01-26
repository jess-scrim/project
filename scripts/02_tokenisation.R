source("scripts/00_setting_up.R")

# Tokenise
tidy_abstracts <- abstracts %>% 
  filter(!text == "NA") %>% 
  mutate(type = case_when(date <= leca_approv ~ "pre-lena",
                          date > leca_approv ~ "post-lena")) %>% 
  unnest_tokens(word, text)

# Remove stopwords
data(stop_words)

tidy_abstracts_clean <- tidy_abstracts %>%
  anti_join(stop_words)
tidy_abstracts_clean <- tidy_abstracts_clean %>%
  anti_join(my_stopwords)

# save tidy_abstracts_clean to data file
 tidy_abstracts_clean %>%
  write_csv("results/tidy_abstracts_clean.csv")

# Plot most common words among all abstracts
tidy_abstracts_clean %>%  
  count(word, sort = TRUE) %>%
  filter(n > 2500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
