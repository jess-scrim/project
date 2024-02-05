# Create document term matrix of bigrams
bigram_dtm <- bigrams_separated %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(abstract, bigram) %>% 
  cast_dtm(abstract, bigram, n) 

# Create 2 topic LDA model for all abstracts
all_lda <- LDA(bigram_dtm, k = 2, control = list(seed = 1234))

# Get beta score for each bigram
tidy_lda <- tidy(all_lda, 
                            matrix = "beta")

top_bigram_all <- tidy_lda %>%
  filter(!term %in% c("neurodegenerative diseases", "parkinson\'s disease", "cognitive impairment")) %>%  
  group_by(topic) %>%
  slice_max(beta, n = 20, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_bigram_all

# Visualise which are the top 20 bigrams per topic
top_bigram_all <- top_bigram_all %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 bigrams in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
top_bigram_all


## Gamma

# Get gamma scores
tidy_lda_gamma <- tidy(all_lda, 
                 matrix = "gamma")

# Join results with abstract to get the date variable
abstract_gamm <- tidy_lda_gamma %>%
  mutate(abstract = as.numeric(document)) %>% 
  left_join(bigrams_separated, by = "abstract") %>% 
  select(document, gamma, topic, date) %>% 
  mutate(type = case_when(date <= leca_approv ~ "pre-leca",
                          date > leca_approv ~ "post-leca"))

#augment(all_lda, bigram_dtm)

# Does not show much - majority are at  1.00 or 0.00 gamma score
abstract_gamm %>% 
  distinct(document, gamma, topic, date, type) %>%
  ggplot() +
  geom_point(aes(x = date, y = gamma), 
             alpha = 0.1) +
  facet_wrap(~ topic )

# Does not show much
abstract_gamm %>% 
  distinct(document, gamma, topic, date, type) %>% 
  ggplot() +
  geom_point(aes(x = type, y = gamma), 
             alpha = 0.1) +
  facet_wrap(~ topic )

abstract_gamm %>% 
  distinct(document, gamma, topic, date, type) %>%
  ggplot() +
  geom_boxplot(aes(x = type, y = gamma)) +
  geom_jitter(aes(x = type, y = gamma))+
  facet_wrap(~ topic )

## See if significant difference
wilcox.test(abstract_gamm$gamma)

# Wilcoxon signed rank test with continuity correction
# 
# data:  abstract_gamm$gamma
# V = 2.7275e+11, p-value < 2.2e-16
# alternative hypothesis: true location is not equal to 0