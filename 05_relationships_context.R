#### Undersanding relationships between words: n-grams & correlations ####

# use ' token = "ngrams" ' in unnest_tokens and specify number of consecutive words, n

# Create bigrams (two consecutive words) in Jane Austen
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) # remove NA

# Count most common bigrams
austen_bigrams %>%
  count(bigram, sort = TRUE)

# Remove stop words in bigrams
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% # remove word1 if stopword
  filter(!word2 %in% stop_words$word) # remove word2 if stopword

# Count bigrams
bigram_counts <- bigrams_separated %>% 
  count(word1, word2, sort = TRUE)
#   character names are most common
#>    word1   word2         n
#>    <chr>   <chr>     <int>
#>  1 sir     thomas      266
#>  2 miss    crawford    196
#>  3 captain wentworth   143
#>  4 miss    woodhouse   143

# join word1 and word2 back together into bigram
bigrams_united <- bigrams_separated %>%
  unite(bigram, word1, word2, sep = " ") # 'bigram' name of new column

bigrams_united

# Find most common trigrams (n=3)
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
#   character names are still most common
#>    word1     word2     word3         n
#>    <chr>     <chr>     <chr>     <int>
#>  1 dear      miss      woodhouse    20
#>  2 miss      de        bourgh       17
#>  3 lady      catherine de           11
#>  4 poor      miss      taylor       11
#>  5 sir       walter    elliot       10

# ETC..

### Analysing bigrams ###
#  Most common street
bigrams_separated %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)
# book                word1             n
# <fct>               <chr>       <int>
#   1 Sense & Sensibility harley       16
# 2 Sense & Sensibility berkeley       15
# 3 Northanger Abbey    milsom         10
# 4 Northanger Abbey    pulteney       10

# Identify most common bigrams per book
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf
# book                bigram                n     tf   idf tf_idf
# <fct>               <chr>             <int>  <dbl> <dbl>  <dbl>
# 1 Mansfield Park      sir thomas          266 0.0304  1.79 0.0545
# 2 Persuasion          captain wentworth   143 0.0290  1.79 0.0519
# 3 Mansfield Park      miss crawford       196 0.0224  1.79 0.0402
# 4 Persuasion          lady russell        110 0.0223  1.79 0.0399
# 5 Persuasion          sir walter          108 0.0219  1.79 0.0392
# 6 Emma                miss woodhouse      143 0.0173  1.79 0.0309
# 7 Northanger Abbey    miss tilney          74 0.0165  1.79 0.0295

# visualise
bigram_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>% # top 15 words per book
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
#  still most common bigrams are chr names

### Providing sentiment to bigrams ###

# Take words preceded by not
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# check which words are preceded and are of sentiment
AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% # make word2 = word
  count(word2, value, sort = T) # gets most common 'not' pair and if +Ve or -ve
not_words

# check how much this effects the sentiment of word
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>% # arrange by absolute
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>% # arrange by +ve to -ve
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")
#  Conclude, 
#     'not like' and 'not help' massively increased the sentiment of the text despite being in negative context
#     'not fail' and 'not afraid' made text seem more negative than seemed

# check for other negation words
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)
negated_words

# visualise
negated_words %>%
  mutate(contribution = n * value) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10, with_ties = F) %>%
  ungroup() %>%
  ggplot(aes(n * value, fct_reorder(word2, contribution), fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word1, ncol = 2, scales = "free") +
  labs(x = "Sentiment value * number of occurrences", 
       y = "Words preceded by negation term")

#### Visualise network of bigrams ####
library(igraph)

# original counts
bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame() # most common word1->word2

bigram_graph

library(ggraph)
set.seed(2017) # set random 2017

# Visualise how word1 relates to word2
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# visualise differently - shows direction of relationship
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#### Visualising bigrams in other texts

# Create functions

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# E.g. in Bible
# the King James version is book 10 on Project Gutenberg:
library(gutenbergr)
kjv <- gutenberg_download(10)
library(stringr)

kjv_bigrams <- kjv %>%
  count_bigrams()
# filter out rare combinations, as well as digits
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()