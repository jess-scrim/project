# Split words into 10 word sections, then tokenise into non-stop words
austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

# Use widyr to find most common two words per sections
library(widyr)

# count words co-occurring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs
# item1     item2         n
# <chr>     <chr>     <dbl>
# 1 darcy     elizabeth   144
# 2 elizabeth darcy       144
# E.g. most common pairs are 'Darcy' & 'Elizabeth' as they are two main characters
word_pairs %>%
  filter(item1 == "darcy")

#### Pairwise correlation ####
# Finding how much more common pairs occur together than neither occurring - phi coefficient

# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE) # pairwise correlation

word_cors
# A tibble: 154,842 x 3
# item1     item2     correlation
# <chr>     <chr>           <dbl>
# 1 bourgh    de              0.951
# 2 de        bourgh          0.951
# 3 pounds    thousand        0.701
# 4 thousand  pounds          0.701

# Most commonly correlated words for 'elizabeth', 'pounds', 'married', 'pride'
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# Using the word cloud
set.seed(2016)
# Relationships are equal, so no arrow is shown as correlation is the same for 'thousand' 'pounds' and 'pounds' 'thousand'
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
