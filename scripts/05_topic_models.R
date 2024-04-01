### Term Frequency ###
## How likely a term is going to appear in one period over another ##

# Calculate tf-idf - term frequency 
abstract_tf_idf <- tidy_abstracts_clean %>% 
  count(abstract, word, sort = TRUE) %>%
  bind_tf_idf(word, abstract, n)

# Highest tf-idf
abstract_tf_idf %>% 
  arrange(-tf_idf)

### Topic Models ###
# LDA - Latent Dirichlet Allocation
word_count <- tidy_abstracts_clean %>%
  count(word, abstract, sort = TRUE) %>% 
  ungroup()
word_count
# word      abstract     n
# 
# 1 mg             317    22
# 2 sleep          823    21
# 3 aß             905    20
# 4 aß1            338    20
# 5 cognitive      806    20
# 6 tau            761    20
# 7 lt             192    19
# 8 metal          905    19
# 9 0.01           192    18
# 10 mtbi          120    18


# Cast the word counts into a document term matrix
abstract_dtm <- word_count %>%
  cast_dtm(abstract, word, n) # 

abstract_dtm
# <<DocumentTermMatrix (documents: 1000, terms: 14604)>>
# Non-/sparse entries: 88983/14515017
# Sparsity           : 99%
# Maximal term length: 27
# Weighting          : term frequency (tf)

# be aware that running this model is time intensive
abstract_lda <- LDA(abstract_dtm, k = 10, control = list(seed = 1234))
abstract_lda
# A LDA_VEM topic model with 10 topics.

# Interpret the model
tidy_lda <- tidy(abstract_lda)

View(tidy_lda)

# Top 10 terms per topic
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms
# topic term         beta
#
#   1     1 disease   0.00778
# 2     1 cognitive 0.00589
# 3     1 study     0.00586
# 4     1 activity  0.00530
# 5     1 potential 0.00483
# 6     1 ache      0.00460
# 7     1 memory    0.00419
# 8     1 1         0.00407
# 9     1 based     0.00407
# 10     1 binding   0.00378

# Visualise
top_terms <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
top_terms

# 
lda_gamma <- tidy(abstract_lda, matrix = "gamma")

lda_gamma

ggplot(lda_gamma, aes(gamma)) +
  geom_histogram(alpha = 0.8) +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))



## Topic modelling bi-grams ##
bigram_tf_idf <- abstract_bigrams %>% 
  count(abstract, bigram, sort = TRUE) %>% 
  bind_tf_idf(bigram, abstract, n)

# Highest bigram tf-idf
bigram_tf_idf %>% 
  arrange(-tf_idf)

### Topic Models ###
# LDA - Latent Dirichlet Allocation
bigram_count_united <- bigrams_united %>% 
  count(bigram, abstract, sort = T)

# Cast the bigram counts into a document term matrix
bigram_dtm <- bigrams_separated %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(abstract, bigram) %>% 
  cast_dtm(abstract, bigram, n) # 

bigram_dtm
# <<DocumentTermMatrix (documents: 943, terms: 45719)>>
# Non-/sparse entries: 60484/43052533
# Sparsity           : 100%
# Maximal term length: 39
# Weighting          : term frequency (tf)

# be aware that running this model is time intensive
bigram_lda <- LDA(bigram_dtm, k = 10, control = list(seed = 1234))
bigram_lda
# A LDA_VEM topic model with 10 topics.

# Interpret the model
tidy_bigram_lda <- tidy(bigram_lda)

View(tidy_bigram_lda)

# Top 10 terms per topic
top_bigram_terms <- tidy_bigram_lda %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_bigram_terms
# topic term                          beta
# 
# 1     1 cognitive impairment       0.00346
# 2     1 tau pet                    0.00271
# 3     1 neurodegenerative diseases 0.00256
# 4     1 ic 50                      0.00181
# 5     1 cognitive decline          0.00165
# 6     1 dementia care              0.00165
# 7     1 glp 1                      0.00165
# 8     1 parkinson's disease        0.00150
#  9     1 brain tissue               0.00150
# 10     1 clinical trials            0.00135

# Visualise
top_bigram_terms <- top_bigram_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
top_bigram_terms
# 
bigram_lda_gamma <- tidy(bigram_lda, matrix = "gamma")

bigram_lda_gamma

ggplot(bigram_lda_gamma, aes(gamma)) +
  geom_histogram(alpha = 0.8, bins = 10) +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

{r}
#| label: LDA Topic Model Bigrams
#| include: false
#| cache: true

# Cast the bigram counts into a document term matrix
bigram_dtm_pre <- bigrams_united %>%
  filter(type == "pre-leca") %>% 
  count(abstract, bigram) %>% 
  cast_dtm(abstract, bigram, n) 

bigram_dtm_post <- bigrams_united %>%
  filter(type == "post-leca") %>% 
  count(abstract, bigram) %>% 
  cast_dtm(abstract, bigram, n)

bigram_lda_pre <- LDA(bigram_dtm_pre, k = 10, control = list(seed = 1234))
# A LDA_VEM topic model with 10 topics
bigram_lda_post <- LDA(bigram_dtm_post, k = 10, control = list(seed = 1234))
# A LDA_VEM topic model with 10 topics.

# Interpret the model
tidy_bigram_lda_pre <- tidy(bigram_lda_pre, 
                            matrix = "beta")

tidy_bigram_lda_post <- tidy(bigram_lda_post,
                             matrix = "beta")

# Top 10 terms per topic
#   Not including 'neurodegenerative diseases', parkinson\'s disease' and 'cognitive impairment' as these were common to all bar one topics

top_bigram_terms_pre <- tidy_bigram_lda_pre %>%
  filter(!term %in% c("neurodegenerative disease", "parkinson\'s disease", "amyloid beta")) %>%
  # mutate(topic = case_when(topic == 1 ~ "Mechanisms",
  #                          topic == 2 ~ "Protein",
  #                          topic == 3 ~ "Something else1",
  #                          topic == 4 ~ "Something else2",
  #                          topic == 5 ~ "Something else3",
  #                          topic == 6 ~ "Something else4",
  #                          topic == 7 ~ "Something else5",
  #                          topic == 8 ~ "Something else6",
  #                          topic == 9 ~ "Something other",
  #                          topic == 10 ~ "Something else7")) %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_bigram_terms_post <- tidy_bigram_lda_post %>%
  filter(!term %in% c("neurodegenerative disease", "parkinson\'s disease")) %>%
  # mutate(topic = case_when(topic == 1 ~ "Mechanisms",
  #                          topic == 2 ~ "Protein",
  #                          topic == 3 ~ "Something else1",
  #                          topic == 4 ~ "Something else2",
  #                          topic == 5 ~ "Something else3",
  #                          topic == 6 ~ "Something else4",
  #                          topic == 7 ~ "Something else5",
  #                          topic == 8 ~ "Something else6",
  #                          topic == 9 ~ "Something other",
  #                          topic == 10 ~ "Something else7")) %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualise
top_bigram_terms_pre <- top_bigram_terms_pre %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 bigrams in each LDA topic: Pre-leca",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")


top_bigram_terms_post <- top_bigram_terms_post %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 bigrams in each LDA topic: Post-leca",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

{r}
#| label: LDA Topic Model Trigrams
#| include: false
#| cache: true

# Cast the bigram counts into a document term matrix
trigram_dtm_pre <- trigrams_united %>%
  filter(type == "pre-leca") %>% 
  count(abstract, trigram) %>% 
  cast_dtm(abstract, trigram, n) 

trigram_dtm_post <- trigrams_united %>%
  filter(type == "post-leca") %>% 
  count(abstract, trigram) %>% 
  cast_dtm(abstract, trigram, n)

trigram_lda_pre <- LDA(trigram_dtm_pre, k = 10, control = list(seed = 1234))
# A LDA_VEM topic model with 10 topics
trigram_lda_post <- LDA(trigram_dtm_post, k = 10, control = list(seed = 1234))
# A LDA_VEM topic model with 10 topics.

# Interpret the model
tidy_trigram_lda_pre <- tidy(trigram_lda_pre, 
                             matrix = "beta")

tidy_trigram_lda_post <- tidy(trigram_lda_post,
                              matrix = "beta")

# Top 10 terms per topic
#   Not including 'neurodegenerative diseases', parkinson\'s disease' and 'cognitive impairment' as these were common to all bar one topics

top_trigram_terms_pre <- tidy_trigram_lda_pre %>%
  filter(!term %in% c("central nervous system", "parkinson's disease pd", "blood-brain barrier")) %>%
  # mutate(topic = case_when(topic == 1 ~ "Mechanisms",
  #                        topic == 2 ~ "Protein",
  #                        topic == 3 ~ "Something else1",
  #                        topic == 4 ~ "Something else2",
  #                        topic == 5 ~ "Something else3",
  #                        topic == 6 ~ "Something else4",
  #                        topic == 7 ~ "Something else5",
  #                        topic == 8 ~ "Something else6",
  #                        topic == 9 ~ "Something other",
  #                        topic == 10 ~ "Something else7")) %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_trigram_terms_post <- tidy_trigram_lda_post %>% 
  filter(!term %in% c("central nervous system", "parkinson's disease pd")) %>%
  # mutate(topic = case_when(topic == 1 ~ "Mechanisms",
  #                          topic == 2 ~ "Protein",
  #                          topic == 3 ~ "Something else1",
  #                          topic == 4 ~ "Something else2",
  #                          topic == 5 ~ "Something else3",
  #                          topic == 6 ~ "Something else4",
  #                          topic == 7 ~ "Something else5",
  #                          topic == 8 ~ "Something else6",
  #                          topic == 9 ~ "Something other",
  #                          topic == 10 ~ "Something else7")) %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)


# Visualise
top_trigram_terms_pre <- top_trigram_terms_pre %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(title = "Top 10 trigrams in each LDA topic: Pre-leca",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")


top_trigram_terms_post <- top_trigram_terms_post %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(title = "Top 10 trigrams in each LDA topic: Post-leca",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")