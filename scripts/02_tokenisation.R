source("scripts/00_setting_up.R")
#source("scripts/01_data_cleaning.R")

# tidy text
tidy_abstracts <- abstracts %>% 
 # head(1000) %>% 
  filter(!text == "NA") %>% 
  mutate(type = case_when(date <= "2023-01-06" ~ "pre-lena",
                          date > "2023-01-06" ~ "post-lena")) 
tidy_abstracts <- tidy_abstracts %>% 
  unnest_tokens(word, text)

# remove stopwords
data(stop_words)
my_stopwords <- tibble(word = c("alzheimer's", "ad"))

tidy_abstracts_clean <- tidy_abstracts %>%
  anti_join(stop_words)
tidy_abstracts_clean <- tidy_abstracts_clean %>%
  anti_join(my_stopwords)

# Plot most common words among all abstracts
tidy_abstracts_clean %>%  
  count(word, sort = TRUE) %>%
  filter(n > 2500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
