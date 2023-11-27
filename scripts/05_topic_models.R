### Topic Models ###
# LDA - Latent Dirichlet Allocation
word_count_pre <- tidy_abstracts_clean %>%
  filter(type == "pre-leca") %>% 
  count(word, abstract, sort = TRUE) %>% 
  ungroup()
word_count_pre
# word     abstract     n
# 1 hdl          5323    29
# 2 medicine     5744    28
# 3 0            3511    26
# 4 tau          4883    26
# 5 dementia     2793    24
# 6 tea          5405    24
# 7 studies      4728    22
# 8 0            5548    21
# 9 afs          3235    21
# 10 2            4388    20

word_count_post <- tidy_abstracts_clean %>%
  filter(type == "post-leca") %>% 
  count(word, abstract, sort = TRUE) %>% 
  ungroup()
word_count_post
# word      abstract     n
# 1 mm3           2189    30
# 2 donepezil      297    28
# 3 disease       1974    25
# 4 speech        1550    24
# 5 lb             227    23
# 6 covid         2505    22
# 7 evidence      2544    22
# 8 mg              46    22
# 9 treatment     2189    22
# 10 19            2505    21


# Cast the word counts into a document term matrix
abstract_dtm_pre <- word_count_pre %>%
  cast_dtm(abstract, word, n) 
abstract_dtm_pre
# <<DocumentTermMatrix (documents: 3437, terms: 22711)>>
# Non-/sparse entries: 299986/77757721
# Sparsity           : 100%
# Maximal term length: 34
# Weighting          : term frequency (tf)

abstract_dtm_post <- word_count_post %>%
  cast_dtm(abstract, word, n)
abstract_dtm_post
# <<DocumentTermMatrix (documents: 2718, terms: 20589)>>
# Non-/sparse entries: 241566/55719336
# Sparsity           : 100%
# Maximal term length: 29
# Weighting          : term frequency (tf)

# be aware that running this model is time intensive
abstract_lda_pre <- LDA(abstract_dtm_pre, k = 10, control = list(seed = 1234))
abstract_lda_pre
# A LDA_VEM topic model with 10 topics.
abstract_lda_post <- LDA(abstract_dtm_post, k = 10, control = list(seed = 1234))
abstract_lda_post
# A LDA_VEM topic model with 10 topics.

# Interpret the model - per-topic-per-word probabilities, called β (“beta”)
#   - Determines how likely (beta) the term is to be associated with each of the 10 topics
tidy_lda_pre <- tidy(abstract_lda_pre,
                     matrix = "beta") %>% 
  mutate(type = "pre_leca")
tidy_lda_post <- tidy(abstract_lda_post,
                      matrix = "beta") %>% 
  mutate(type = "post_leca")

View(tidy_lda_pre)
View(tidy_lda_post)

# Top 10 terms per topic
#   Not including 'disease' as this was common to all topics
#
# 'review' is common to 6 topics
# top_terms_pre %>% group_by(term) %>% filter(n_distinct(topic) > 5) %>% View()

# Pre-leca
top_terms_pre <- tidy_lda_pre %>%
  filter(term != "disease") %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Post-leca
top_terms_post <- tidy_lda_post %>%
  filter(term != "disease") %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualise - top 10 terms per topic
top_terms_pre <- top_terms_pre %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic: Pre-Leca",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
top_terms_pre

top_terms_post <- top_terms_post %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic: Post Leca",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
top_terms_post

# top_terms <- top_terms %>%
#   mutate(term = reorder_within(term, beta, topic)) %>%
#   group_by(topic, term, type) %>%    
#   arrange(desc(beta)) %>%  
#   ungroup() %>%
#   ggplot(aes(beta, term, fill = as.factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   scale_y_reordered() +
#   facet_wrap(~ topic, ncol = 4, scales = "free")
#   labs(title = "Top 10 terms in each LDA topic: Pre-Leca",
#        x = expression(beta), y = NULL) +
#   facet_wrap(~ topic, ncol = 4, scales = "free")
# top_terms


# Determine terms with the greatest difference in beta between pre-leca and post-leca

top_terms <- bind_rows(tidy_lda_pre, 
                       tidy_lda_post)
top_terms <- top_terms %>%
  filter(term != "disease") %>% 
  group_by(topic, type) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

beta_wide <- top_terms %>%
  #mutate(type = paste0(type, "_topic_", topic)) %>%
  pivot_wider(names_from = type, values_from = beta) %>% 
  filter(pre_leca > .001 | post_leca > .001) %>%
  mutate(log_ratio = log2(pre_leca / post_leca))

beta_wide

beta_wide %>%
  group_by(direction = log_ratio > 0) %>%
  slice_max(abs(log_ratio), n = 10) %>% 
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta pre-leca / post-leca", y = NULL)# +
  facet_wrap(~ topic, ncol = 4, scales = "free")
