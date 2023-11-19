### Term Frequency ###

# Pre-leca
# Calculate tf-idf - term frequency 
pre_tf_idf <- tidy_abstracts_clean %>% 
  filter(type == "pre-leca") %>% 
  count(abstract, word, sort = TRUE) %>%
  bind_tf_idf(word, abstract, n)

# Highest tf-idf
pre_tf_idf %>% 
  arrange(-tf_idf)

# Post-leca
# Calculate tf-idf - term frequency 
post_tf_idf <- tidy_abstracts_clean %>% 
  filter(type == "post-leca") %>% 
  count(abstract, word, sort = TRUE) %>%
  bind_tf_idf(word, abstract, n)

# Highest tf-idf
post_tf_idf %>% 
  arrange(-tf_idf)

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
# <<DocumentTermMatrix (documents: 3437, terms: 22713)>>
# Non-/sparse entries: 300358/77764223
# Sparsity           : 100%
# Maximal term length: 34
# Weighting          : term frequency (tf)

abstract_dtm_post <- word_count_post %>%
  cast_dtm(abstract, word, n)
abstract_dtm_post
# <<DocumentTermMatrix (documents: 2718, terms: 20591)>>
# Non-/sparse entries: 241923/55724415
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

# Interpret the model
tidy_lda_pre <- tidy(abstract_lda_pre)
tidy_lda_post <- tidy(abstract_lda_post)

View(tidy_lda_pre)
View(tidy_lda_post)

# Top 10 terms per topic

# Pre-leca
top_terms_pre <- tidy_lda_pre %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_pre
# topic term        beta
# 1     1 disease  0.0172 
# 2     1 2        0.0130 
# 3     1 insulin  0.0114 
# 4     1 diabetes 0.0107 
# 5     1 brain    0.00932
# 6     1 risk     0.00763
# 7     1 covid    0.00762
# 8     1 1        0.00748
# 9     1 19       0.00728
# 10     1 type     0.00666

# Post
top_terms_post <- tidy_lda_post %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_post
# topic term          beta
# 1     1 disease    0.0255 
# 2     1 clinical   0.0202 
# 3     1 dementia   0.0130 
# 4     1 biomarkers 0.0103 
# 5     1 treatment  0.00970
# 6     1 review     0.00969
# 7     1 diagnosis  0.00905
# 8     1 trials     0.00860
# 9     1 based      0.00657
# 10     1 imaging    0.00650

# Visualise
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

# 
lda_gamma_pre <- tidy(abstract_lda_pre, matrix = "gamma")
lda_gamma_pre
lda_gamma_post <- tidy(abstract_lda_post, matrix = "gamma")
lda_gamma_post

ggplot(lda_gamma_pre, aes(gamma)) +
  geom_histogram(alpha = 0.8) +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for Pre-leca Topics",
       y = "Number of documents", x = expression(gamma))

ggplot(lda_gamma_post, aes(gamma)) +
  geom_histogram(alpha = 0.8) +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for Post-leca Topics",
       y = "Number of documents", x = expression(gamma))

ggplot(lda_gamma_pre, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic: Pre-leca",
       y = "Number of documents", x = expression(gamma))

ggplot(lda_gamma_post, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic: Post-leca",
       y = "Number of documents", x = expression(gamma))

# --------------------------------------------------------------------------#
## Topic modelling bi-grams ##
bigram_tf_idf_pre <- abstract_bigrams %>% 
  filter(type == "pre-leca") %>% 
  count(abstract, bigram, sort = TRUE) %>% 
  bind_tf_idf(bigram, abstract, n)

bigram_tf_idf_post <- abstract_bigrams %>% 
  filter(type == "post-leca") %>% 
  count(abstract, bigram, sort = TRUE) %>% 
  bind_tf_idf(bigram, abstract, n)

# Highest bigram tf-idf
bigram_tf_idf_pre %>% 
  arrange(-tf_idf)
# abstract bigram                      n     tf   idf tf_idf
# 1     5916 occupational complexity     8 0.0533  8.14  0.434
# 2     4609 kojic acid                 11 0.0519  8.14  0.422
# 3     4071 ng2 glia                    8 0.0503  8.14  0.410
# 4     4588 k2 7                       16 0.0502  8.14  0.408
# 5     5597 ca 2                       17 0.0829  4.85  0.402
# 6     5363 vessel regression           8 0.0537  7.45  0.400
# 7     4824 synaptic clearance          9 0.0486  8.14  0.396
# 8     5161 neurexin 3                  7 0.0486  8.14  0.396
# 9     4021 clock genes                 7 0.0551  7.04  0.388
# 10     3721 il 22                       6 0.0469  8.14  0.382
bigram_tf_idf_post %>% 
  arrange(-tf_idf)
# abstract bigram                       n     tf   idf tf_idf
# 1      227 ad lb                       18 0.0657  7.91  0.519
# 2     1374 subcellular localization    14 0.0725  6.52  0.473
# 3     1410 5p mir                      10 0.0671  6.52  0.438
# 4     2722 caffeic acid                 4 0.0635  6.81  0.432
# 5     1138 kptn gene                    2 0.0513  7.91  0.406
# 6        6 audiovisual integration      7 0.0470  7.91  0.372
# 7     1205 painting therapy            17 0.0467  7.91  0.369
# 8     1613 502 3p                       8 0.0449  7.91  0.355
# 9     2564 apoc iii                    10 0.0437  7.91  0.345
# 10     1168 a tatarinowii                7 0.0432  7.91  0.342

### Topic Models ###
# LDA - Latent Dirichlet Allocation

# Cast the bigram counts into a document term matrix
bigram_dtm_pre <- bigrams_separated %>%
  filter(type == "pre-leca") %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(abstract, bigram) %>% 
  cast_dtm(abstract, bigram, n) 

bigram_dtm_pre
# <<DocumentTermMatrix (documents: 3437, terms: 109397)>>
# Non-/sparse entries: 188223/375809266
# Sparsity           : 100%
# Maximal term length: 69
# Weighting          : term frequency (tf)

bigram_dtm_post <- bigrams_separated %>%
  filter(type == "post-leca") %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(abstract, bigram) %>% 
  cast_dtm(abstract, bigram, n)

bigram_dtm_post
# <<DocumentTermMatrix (documents: 2718, terms: 92600)>>
# Non-/sparse entries: 153039/251533761
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
tidy_bigram_lda_pre <- tidy(bigram_lda_pre)
View(tidy_bigram_lda_pre)

tidy_bigram_lda_post <- tidy(bigram_lda_post)
View(tidy_bigram_lda_post)

# Top 10 terms per topic
top_bigram_terms_pre <- tidy_bigram_lda_pre %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_bigram_terms_pre
# topic term                          beta
# 1     1 neurodegenerative diseases 0.00413
# 2     1 clinical trials            0.00352
# 3     1 oxidative stress           0.00268
# 4     1 cognitive impairment       0.00224
# 5     1 parkinson's disease        0.00217
#  6     1 amyloid ß                  0.00214
#  7     1 cognitive decline          0.00208
#  8     1 95 ci                      0.00183
#  9     1 nervous system             0.00172
# 10     1 alzheimer disease          0.00161

top_bigram_terms_post <- tidy_bigram_lda_post %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_bigram_terms_post
# topic term                          beta
# 1     1 cognitive impairment       0.00644
# 2     1 oxidative stress           0.00488
# 3     1 nervous system             0.00415
# 4     1 neurodegenerative diseases 0.00403
# 5     1 brain barrier              0.00374
# 6     1 blood brain                0.00373
# 7     1 central nervous            0.00365
# 8     1 parkinson's disease        0.00330
#  9     1 mild cognitive             0.00177
# 10     1 clinical trials            0.00173

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
# 
bigram_lda_gamma_pre <- tidy(bigram_lda_pre, matrix = "gamma")
bigram_lda_gamma_pre

bigram_lda_gamma_post <- tidy(bigram_lda_post, matrix = "gamma")
bigram_lda_gamma_post

ggplot(bigram_lda_gamma_pre, aes(gamma)) +
  geom_histogram(alpha = 0.8, bins = 10) +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics: Pre-leca",
       y = "Number of documents", x = expression(gamma))

ggplot(bigram_lda_gamma_post, aes(gamma)) +
  geom_histogram(alpha = 0.8, bins = 10) +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics: Post-leca",
       y = "Number of documents", x = expression(gamma))
