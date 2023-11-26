#### Understanding relationships between words: n-grams & correlations ####

abstract_bigrams <- tidy_abstracts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) # remove NA

# abstract_bigrams <- read_csv("results/abstract_bigrams")

# Remove stop words in bigrams
bigrams_separated <- abstract_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word &
         !word1 %in% my_stopwords$word) %>% # remove word1 if stopword
  filter(!word2 %in% stop_words$word &
         !word2 %in% my_stopwords$word) # remove word2 if stopword

## Count bigrams ##

# All abstracts
bigram_counts <- bigrams_separated %>% 
  group_by(type) %>% 
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

# Pre-leca
bigram_counts %>% 
  filter(type == "pre-leca")
# word1             word2          n
# 
#  1 neurodegenerative diseases    1153
#  2 parkinson's       disease      790
#  3 cognitive         impairment   651
#  4 nervous           system       562
#  5 95                ci           527
#  6 cognitive         decline      458
#  7 central           nervous      433
#  8 oxidative         stress       417
#  9 meta              analysis     412
# 10 neurodegenerative disorders    412

# Post-leca
bigram_counts %>% 
  filter(type == "post-leca")
# word1             word2          n
# 
#  1 neurodegenerative diseases     851
#  2 cognitive         impairment   606
#  3 parkinson's       disease      603
#  4 95                ci           518
#  5 nervous           system       467
#  6 cognitive         decline      361
#  7 oxidative         stress       358
#  8 meta              analysis     345
#  9 central           nervous      341
# 10 clinical          trials       332

# join word1 and word2 back together into bigram
bigrams_united <- bigram_counts %>%
  group_by(type) %>% 
  unite(bigram, word1, word2, sep = " ") # 'bigram' name of new column

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
# 1 diseases    2483
# 2 disorders   1423
# 3 disease      633
# 4 disorder     338
# 5 tangles      293
# 6 effects      239
# 7 conditions   194
# 8 symptoms     140
# 9 loss         133
# 10 death        108

bigrams_separated %>%
  filter(grepl("^neuro", word2)) %>%
  count(word1, sort = TRUE)
# word1           n
# 
# 1 common        215
# 2 progressive   184
# 3 related       168
# 4 including     106
# 5 chronic        82
# 6 derived        72
# 7 induced        61
# 8 prevalent      58
# 9 treating       55
# 10 disease        54

#### Visualise network of bigrams ####

# Filter for only relatively common combinations
bigram_graph <- bigrams_united %>%
  filter(n > 300) %>%
  graph_from_data_frame() # most common word1->word2

bigram_graph

set.seed(2017) # set random 2017

# Visualise how word1 relates to word2
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Bigram Relations")

# Pre-leca
pre_leca_graph <- bigrams_separated %>%
  filter(type == "pre-leca") %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n > 200) %>%
  graph_from_data_frame()
pre_leca_graph
ggraph(pre_leca_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Pre-leca Bigram Relations")

# Post-leca
post_leca_graph <- bigrams_separated %>%
  filter(type == "post-leca") %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n > 200) %>%
  graph_from_data_frame()
post_leca_graph
ggraph(pre_leca_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Post-leca Bigram Relations")

# Visualise - thickness of line determines how strong the relationship is
set.seed(2020)

bigram_counts %>% 
  filter(n > 400) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void() +
  ggtitle("Bigram Relations")

# Pre-leca
pre_leca_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void() +
  ggtitle("Pre-leca Bigram Relations")


# Pre-leca
post_leca_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void() +
  ggtitle("Post-leca Bigram Relations")
