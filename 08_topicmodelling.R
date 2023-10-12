### Topic Modelling ###
# Unsupervised classification of documents into natural groups

## LDA - Latent Dirichlet allocation ##
# Treats each document as a mixture of topics, and each topic as a mixture of words
#   - relating topics therefore will have an overlap of the same words

# Using LDA - Data from 2246 American news agency articles
library(topicmodels)

data("AssociatedPress")
AssociatedPress

# Use k = 2 to create a two-topic LDA model
# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
#> A LDA_VEM topic model with 2 topics.

library(tidytext)
# Finding the per-topic-per-word probablities (beta)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics
#>    topic term           beta
#>    <int> <chr>         <dbl>
#>  1     1 aaron      1.69e-12
#>  2     2 aaron      3.90e- 5
#>  3     1 abandon    2.65e- 5
#   I.e. the LDA model gives the probability of generating that word from that topic.
# E.g. For example, the term “aaron” has a 1.686917×10^−12 probability of being generated from topic 1, but a 3.8959408×10^−5 probability of being generated from topic 2.

# Find most common words per topic
library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Results: Most common words in Topic 1 are 'percent', 'million', 'billion' so could be financial news. 
# Most common words in Topic 2 are 'president', 'government' so could be political news
# Some words overlap, e.g. 'people', 'new' which is an advantage to "hard-clustering" methods

# Repeat but find the most common words by the difference in beta scores using log ratio
library(tidyr)

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide
# term              topic1      topic2 log_ratio
# <chr>              <dbl>       <dbl>     <dbl>
#   1 administration 0.000431  0.00138         1.68 
# 2 ago            0.00107   0.000842       -0.339
# 3 agreement      0.000671  0.00104         0.630
# 4 aid            0.0000476 0.00105         4.46 

# Visualise
beta_wide %>% 
  arrange(desc(abs(log_ratio))) %>%
  head(20) %>%
  mutate(term = reorder(term, log_ratio)) %>% 
  ggplot(aes(log_ratio, term)) +
  geom_col(show.legend = FALSE)
# Results more accurate: Topic 2 (+ve log) - Politics 'democratic' 'vote' 'dukakis'
# Topic 1 (-ve log) - Financial 'yen' 'dollar'


# Modelling documents by topics - per-document-per-topic probabilities
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents
#   Gamma gives the proportion of that topic in that document
 #   document topic    gamma
 #      <int> <int>    <dbl>
 # 1        1     1 0.248   
 # 2        2     1 0.362   
 # 3        3     1 0.527   
 # 4        4     1 0.357   
 # 5        5     1 0.181
 # 6        6     1 0.000588
# E.g. Document 6 had almost only topic 2 (0% topic 1) - politics 
#   - Actually about American government and Panamanian dictator Manuel Noriega 
tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

### Could not complete as package gutenbergr is no longer available ###
# Activity looked at classifying pages from four novels back into the correct novel using LDA by associating words to the four book 'topics' then correctly identifying them