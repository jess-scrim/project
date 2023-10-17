#### Undersanding relationships between words: n-grams & correlations ####

abstract_bigrams <- abstracts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) # remove NA

# Count most common bigrams
abstract_bigrams %>%
  count(bigram, sort = TRUE)
#   bigram                  n
#
#  1 in the                152
#  2 of the                114
#  3 alzheimer's disease    98
#  4 to the                 44
#  5 associated with        43
#  6 disease ad             42
#  7 and the                38
#  8 of ad                  32
#  9 on the                 30
# 10 with the               30

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
#     word1             word2          n
# 
# 1 cognitive         impairment    23
# 2 apoe              <U+025B>4     22
# 3 neurodegenerative diseases      19
# 4 spinal            cord          18
# 5 ic                50            16
# 6 95                ci            15
# 7 2                 diabetes      13
# 8 amyloid           beta          13
# 9 oxidative         stress        13
# 10 type              2            13


# join word1 and word2 back together into bigram
bigrams_united <- bigrams_separated %>%
  unite(bigram, word1, word2, sep = " ") # 'bigram' name of new column

bigrams_united

### Analysing bigrams ###
# Most common disease
bigrams_separated %>%
  filter(word2 == "disease") %>%
  count(word1, sort = TRUE)
# word1                 n
# 
# 1 parkinson's          12
# 2 neurodegenerative     5
# 3 huntington's          2
# 4 alzheimer             1
# 5 amyloid               1
# 6 availability          1
# 7 brain                 1
# 8 chronic               1
# 9 determined            1
# 10 disability            1
# 11 immune                1
# 12 kidney                1
# 13 liver                 1
# 14 subclinical           1
# 15 subsequent            1
# 16 vessel                1

#### Visualise network of bigrams ####
bigram_counts
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame() # most common word1->word2

bigram_graph

set.seed(2017) # set random 2017

# Visualise how word1 relates to word2
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# visualise differently - shows direction of relationship
set.seed(2020)

bigram_counts %>% 
  filter(n > 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
