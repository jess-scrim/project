# --------------------------------------------------------------------------#
## Topic modelling bi-grams ##
# LDA - Latent Dirichlet Allocation

# Cast the bigram counts into a document term matrix
bigram_dtm_pre <- bigrams_separated %>%
  filter(type == "pre-leca") %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(abstract, bigram) %>% 
  cast_dtm(abstract, bigram, n) 

bigram_dtm_pre
# <<DocumentTermMatrix (documents: 3437, terms: 108699)>>
# Non-/sparse entries: 186811/373411652
# Sparsity           : 100%
# Maximal term length: 69
# Weighting          : term frequency (tf)

bigram_dtm_post <- bigrams_separated %>%
  filter(type == "post-leca") %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(abstract, bigram) %>% 
  cast_dtm(abstract, bigram, n)

bigram_dtm_post
# <<DocumentTermMatrix (documents: 2718, terms: 91932)>>
# Non-/sparse entries: 151678/249719498
# Sparsity           : 100%
# Maximal term length: 41
# Weighting          : term frequency (tf)

# be aware that running this model is time intensive
bigram_lda_pre <- LDA(bigram_dtm_pre, k = 10, control = list(seed = 1234))
bigram_lda_pre
# A LDA_VEM topic model with 10 topics
bigram_lda_post <- LDA(bigram_dtm_post, k = 10, control = list(seed = 1234))
bigram_lda_post
# A LDA_VEM topic model with 10 topics.

# Interpret the model
tidy_bigram_lda_pre <- tidy(bigram_lda_pre, 
                            matrix = "beta")
View(tidy_bigram_lda_pre)

tidy_bigram_lda_post <- tidy(bigram_lda_post,
                             matrix = "beta")
View(tidy_bigram_lda_post)

# Top 10 terms per topic
#   Not including 'neurodegenerative diseases', parkinson\'s disease' and 'cognitive impairment' as these were common to all bar one topics
#
 top_bigram_terms_pre %>% group_by(term) %>% filter(n_distinct(topic) >8) %>% View()

top_bigram_terms_pre <- tidy_bigram_lda_pre %>%
  filter(!term %in% c("neurodegenerative diseases", "parkinson\'s disease", "cognitive impairment")) %>%  
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_bigram_terms_pre

top_bigram_terms_post <- tidy_bigram_lda_post %>%
  filter(!term %in% c("neurodegenerative diseases", "parkinson\'s disease", "cognitive impairment")) %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_bigram_terms_post

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
top_bigram_terms_pre

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
top_bigram_terms_post
