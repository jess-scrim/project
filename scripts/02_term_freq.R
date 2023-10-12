#### Analysing Term and Document Frequency ####
#     Most frequent words may be stop words, however depending on their frequency in total content, e.g. within whole doc, frequency might be important - term frequency

# Determine word frequency in each book
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

# Total words per book
total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

# join total to word frequency per book
book_words <- left_join(book_words, total_words)

book_words

# Visualise term frequency
ggplot(book_words, aes((n/total), fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

## Zipf’s law states that the frequency that a word appears is inversely proportional to its rank. ##

# Zipf's law
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()
freq_by_rank

# Visualise Zipf's law - always a negative slope
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() + # need to be in log
  scale_y_log10()

#
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)
rank_subset

# Finding exponent for the middle section - linear regression
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
# Coefficients:
#   (Intercept)  log10(rank)  
#       -0.6226      -1.1125 

# visualise exponent
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, # from coefficients
              color = "gray50", linetype = 2) + # dotted line
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


### Using bind_tf_idf function ### 
#  Find common words, unique to each book
book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

# inverse
book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# visualise tf_idf
library(forcats)

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>% # top 15 words per book
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
#   Most top words are proper nouns specific to each book

