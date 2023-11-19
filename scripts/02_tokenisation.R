source("scripts/00_setting_up.R")
#source("scripts/01_data_cleaning.R")

# tidy text
tidy_abstracts <- abstracts %>% 
  filter(!text == "NA") %>% 
  mutate(type = case_when(date <= "2023-01-06" ~ "pre-leca",
                          date > "2023-01-06" ~ "post-leca")) 

# remove stopwords
data(stop_words)
my_stopwords <- tibble(word = c("alzheimer's", "ad"))

tidy_abstracts_clean <- tidy_abstracts %>% 
  unnest_tokens(word, text)
tidy_abstracts_clean <- tidy_abstracts %>%
  anti_join(stop_words)
tidy_abstracts_clean <- tidy_abstracts_clean %>%
  anti_join(my_stopwords)

### Plot most common words ###

## All abstracts
tidy_abstracts_clean %>%  
  count(word, sort = TRUE) %>%
  head(15) %>% 
  ggplot(aes(n, reorder(word, n))) +
  geom_col() +
  labs(y = NULL) +
  ggtitle("Most Common Term: All Abstracts")
  

## Pre-leca
tidy_abstracts_clean %>%  
  filter(type == "pre-leca") %>% 
  count(word, sort = TRUE) %>%
  head(15) %>% 
  ggplot(aes(n, reorder(word, n))) +
  geom_col() +
  labs(y = NULL) +
  ggtitle("Most Common Term: Pre-Leca")

## Post-leca
tidy_abstracts_clean %>%  
  filter(type == "post-leca") %>% 
  count(word, sort = TRUE) %>%
  head(15) %>% 
  ggplot(aes(n, reorder(word, n))) +
  geom_col() +
  labs(y = NULL) +
  ggtitle("Most Common Term: Post-Leca")
