abstract_trigrams <- abstracts %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) # remove NA

# Count most common bigrams
abstract_trigrams %>%
  count(trigram, sort = TRUE)
#    trigram                    n
# 
#  1 alzheimer's disease ad  2633
#  2 as well as              1148
#  3 in this review          1143
#  4 the development of       968
#  5 this review we           919
#  6 disease ad is            844
#  7 of alzheimer's disease   831
#  8 the role of              776
#  9 central nervous system   757
# 10 the treatment of         734

# Remove stop words in trigrams
trigrams_separated <- abstract_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
trigrams_separated <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word &
           !word1 %in% my_stopwords$word) %>% # remove word1 if stopword
  filter(!word2 %in% stop_words$word &
           !word2 %in% my_stopwords$word) %>% # remove word2 if stopword
  filter(!word3 %in% stop_words$word &
           !word3 %in% my_stopwords$word) # remove word3 if stopword

# Count trigrams
trigram_counts <- trigrams_separated %>% 
  count(word1, word2, word3, sort = TRUE)
trigram_counts
# word1       word2       word3          n
#
# 1 central     nervous     system       757
# 2 parkinson's disease     pd           461
# 3 amyotrophic lateral     sclerosis    438
# 4 blood       brain       barrier      426
# 5 mild        cognitive   impairment   408
# 6 disease     parkinson's disease      338
# 7 nervous     system      cns          294
# 8 amyloid     ß           aß           209
# 9 cognitive   impairment  mci          209
# 10 sars        cov         2            204

# join word1, word2 and word3 back together into trigram
trigrams_united <- trigram_counts %>%
  unite(trigram, word1, word2, word3, sep = " ") # 'bigram' name of new column

trigrams_united

#### Visualise network of trigrams ####

# filter for only relatively common combinations
trigram_graph <- trigram_counts %>%
  filter(n > 100) %>%
  graph_from_data_frame() # most common word1->word2

trigram_graph

set.seed(2017) # set random 2017

# Visualise how word1 relates to word2 relates to word3
ggraph(trigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# 