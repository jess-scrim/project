abstract_trigrams <- tidy_abstracts %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) # remove NA

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

## Count trigrams ##

# All abstracts
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

# Pre-leca
trigrams_separated %>% 
  filter(type == "pre-leca") %>% 
  count(word1, word2, word3, sort = TRUE)
# word1       word2       word3          n
#
#  1 central     nervous     system       420
#  2 parkinson's disease     pd           248
#  3 amyotrophic lateral     sclerosis    245
#  4 blood       brain       barrier      231
#  5 mild        cognitive   impairment   214
#  6 disease     parkinson's disease      196
# 7 nervous     system      cns          158
# 8 amyloid     ß           aß           126
# 9 sars        cov         2            126
# 10 type        2           diabetes     121

# Post-leca
trigrams_separated %>% 
  filter(type == "post-leca") %>% 
  count(word1, word2, word3, sort = TRUE)
# word1       word2       word3          n
#  1 central     nervous     system       337
#  2 parkinson's disease     pd           213
#  3 blood       brain       barrier      195
#  4 mild        cognitive   impairment   194
#  5 amyotrophic lateral     sclerosis    193
#  6 disease     parkinson's disease      142
# 7 nervous     system      cns          136
# 8 cognitive   impairment  mci           98
# 9 amyloid     ß           aß            83
# 10 reactive    oxygen      species       78


# join word1, word2 and word3 back together into trigram
trigrams_united <- trigram_counts %>%
  unite(trigram, word1, word2, word3, sep = " ") # 'bigram' name of new column

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

# Visualise strength
bigram_counts %>% 
  filter(n > 300) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()