### Term Frequency ###

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
# 1 mir             68    17
# 2 adherence        9    14
# 3 auditory        12    14
# 4 reading          8    14
# 5 cord            79    13
# 6 spinal          79    13
# 7 tau             53    13
# 8 pain             4    12
# 9 activity        85    11
# 10 dementia        25    11


# Cast the word counts into a document term matrix
desc_dtm <- word_count %>%
  cast_dtm(abstract, word, n) # 

desc_dtm
# <<DocumentTermMatrix (documents: 100, terms: 3749)>>
# Non-/sparse entries: 8535/366365
# Sparsity           : 98%
# Maximal term length: 22
# Weighting          : term frequency (tf)

# be aware that running this model is time intensive
desc_lda <- LDA(desc_dtm, k = 10, control = list(seed = 1234))
desc_lda
# A LDA_VEM topic model with 10 topics.

# Interpret the model
tidy_lda <- tidy(desc_lda)

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
# 1     1 apoe      0.0191 
# 2     1 <U+025B>4 0.0184 
# 3     1 amyloid   0.0122 
# 4     1 patients  0.0115 
# 5     1 aß        0.00994
# 6     1 cognitive 0.00992
# 7     1 dementia  0.00916
# 8     1 risk      0.00837
# 9     1 studies   0.00765
# 10    1 ß         0.00765

# Visualise
top_terms %>%
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

# 
lda_gamma <- tidy(desc_lda, matrix = "gamma")

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
