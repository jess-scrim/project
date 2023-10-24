#### Undersanding relationships between words: n-grams & correlations ####

abstract_bigrams <- abstracts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) # remove NA

# Count most common bigrams
abstract_bigrams %>%
  count(bigram, sort = TRUE)
# bigram                  n
# 
#   1 in the               8671
# 2 of the               7268
# 3 alzheimer's disease  6015
#  4 such as              2821
#  5 this review          2797
#  6 disease ad           2774
#  7 of ad                2670
#  8 and the              2654
#  9 to the               2543
# 10 on the               2492

# Remove stop words in bigrams
bigrams_separated <- abstract_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word &
         !word1 %in% my_stopwords$word) %>% # remove word1 if stopword
  filter(!word2 %in% stop_words$word &
         !word2 %in% my_stopwords$word) # remove word2 if stopword

# Count bigrams
bigram_counts <- bigrams_separated %>% 
  count(word1, word2, sort = TRUE)
bigram_counts
# word1             word2          n
#
#  1 neurodegenerative diseases    2004
#  2 parkinson's       disease     1393
#  3 cognitive         impairment  1257
#  4 95                ci          1045
#  5 nervous           system      1029
#  6 cognitive         decline      819
#  7 oxidative         stress       775
#  8 central           nervous      774
#  9 meta              analysis     757
# 10 clinical          trials       735


# join word1 and word2 back together into bigram
bigrams_united <- bigram_counts %>%
  unite(bigram, word1, word2, sep = " ") # 'bigram' name of new column
bigrams_united

### Analysing bigrams ###
# Most common disease
bigrams_separated %>%
  filter(word2 == "disease") %>%
  count(word1, sort = TRUE)
# word1                 n
# 
# 1 parkinson's        1393
# 2 neurodegenerative   544
# 3 huntington's        325
# 4 alzheimer           309
# 5 cardiovascular      131
# 6 parkinson            59
# 7 vessel               55
# 8 coronavirus          49
# 9 neurological         48
# 10 cerebrovascular      43

# Common phrases with 'neuro'
bigrams_separated %>%
  filter(grepl("^neuro", word1)) %>%
  count(word2, sort = TRUE)
# word2          n
# 
# 1 diseases     171
# 2 disorders     96
# 3 disease       43
# 4 disorder      33
# 5 initiative    25
# 6 conditions    24
# 7 cells         21
# 8 effects       20
# 9 loss          20
# 10 tangles       20

bigrams_separated %>%
  filter(grepl("^neuro", word2)) %>%
  count(word1, sort = TRUE)
# word1            n
# 
# 1 disease         30
# 2 hippocampal     18
# 3 progressive     15
# 4 related         14
# 5 common          13
# 6 including       13
# 7 induced         13
# 8 excitatory      10
# 9 derived          9
# 10 dopaminergic     8

#### Visualise network of bigrams ####
bigram_counts
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 300) %>%
  graph_from_data_frame() # most common word1->word2

bigram_graph

set.seed(2017) # set random 2017

# Visualise how word1 relates to word2
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# visualise - thickness of line determines how strong the relationship is
set.seed(2020)

bigram_counts %>% 
  filter(n > 300) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
