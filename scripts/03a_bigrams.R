#### Understanding relationships between words: n-grams & correlations ####

abstract_bigrams <- tidy_abstracts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) # remove NA

# abstract_bigrams <- read_csv("results/abstract_bigrams.csv")

# Remove stop words in bigrams
bigrams_separated <- abstract_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word &
         !word1 %in% my_stopwords$word) %>% # remove word1 if stopword
  filter(!word2 %in% stop_words$word &
         !word2 %in% my_stopwords$word) # remove word2 if stopword

# join word1 and word2 back together into bigram
bigrams_united <- bigrams_separated %>%
  unite(bigram, word1, word2, sep = " ")  # 'bigram' name of new column
  
## Map words and remove abbreviations ##
bigrams_united <- bigrams_united %>% 
 # filter(grepl("alzheimer*\\b*disease*", bigram)) %>% 
  mutate(bigram = str_replace_all(bigram, 
                              "\\b(neurodegenerative dis(?:ease|eases|order|orders)?)\\b|\\b(neurological dis(?:ease|eases|order|orders)?)\\b", 
                              "neurodegenerative disease"),
         bigram = str_replace_all(bigram,
                                  "\\b(central nervous)\\b|\\b(system cns)\\b",
                                  "cns"),
         bigram = str_replace_all(bigram,
                                  "\\b(parkinson's disease)\\b|\\b(disease pd)\\b",
                                  "parkinson's disease"),
         bigram = str_replace_all(bigram,
                                  "\\b(blood brain)\\b|\\b(brain barrier)\\b",
                                  "blood brain"),
         bigram = str_replace_all(bigram,
                                  "\\b(amyloid\\s*\\p{Greek})\\b|\\b(amyloid beta)\\b|\\b(amyloid a\\p{Greek})\\b|\\b(beta amyloid)\\b|\\b(\\p{Greek} amyloid)\\b",
                                   "amyloid beta"),
         bigram = str_replace_all(bigram,
                                  "\\b(2019 covid)\\b|\\b(covid 19)\\b",
                                  "covid 19"))

## Count bigrams ##

# All abstracts
bigram_counts <- bigrams_united %>% 
  group_by(type) %>% 
  count(bigram, sort = TRUE) %>% 
  ungroup()
bigram_counts %>% View()

# Pre-leca
bigram_counts %>% 
  filter(type == "pre-leca") %>% 
  select(-type)
# word1             word2          n
#   1 neurodegenerative diseases    1153
#  2 parkinson's       disease      790
#  3 cognitive         impairment   651
#  4 nervous           system       562
#  5 cognitive         decline      458
#  6 central           nervous      433
#  7 oxidative         stress       417
#  8 meta              analysis     412
#  9 neurodegenerative disorders    412
# 10 clinical          trials       403

# Post-leca
bigram_counts %>% 
  filter(type == "post-leca") %>% 
  select(-type)
# word1             word2          n
#
#   1 neurodegenerative diseases     851
# 2 cognitive         impairment   606
# 3 parkinson's       disease      603
#  4 nervous           system       467
#  5 cognitive         decline      361
#  6 oxidative         stress       358
#  7 meta              analysis     345
#  8 central           nervous      341
#  9 clinical          trials       332
# 10 neurodegenerative disorders    281

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
  count(bigram) %>% 
  filter(n > 200) %>% 
  separate(bigram, c("word1", "word2"), sep = " ")
  
bigram_graph <- bigram_graph %>%
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
pre_leca_graph <- bigrams_united %>%
  filter(type == "pre-leca") %>%
  count(bigram) %>% 
  filter(n > 200) %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

pre_leca_graph <- pre_leca_graph %>%
  graph_from_data_frame()
pre_leca_graph
ggraph(pre_leca_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Pre-leca Bigram Relations")

# Post-leca
post_leca_graph <- bigrams_united %>%
  filter(type == "post-leca") %>%
  count(bigram) %>% 
  filter(n > 200) %>% 
  separate(bigram, c("word1", "word2"), sep = " ")
post_leca_graph <- post_leca_graph %>%
  graph_from_data_frame()
post_leca_graph

ggraph(post_leca_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Post-leca Bigram Relations")

# Visualise - thickness of line determines how strong the relationship is
set.seed(2020)

bigram_graph %>% 
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
