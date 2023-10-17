##### Sentiment Analysis of Tidy Text #####
#    Lexicons contain many English words and the words are assigned scores for           positive/negative sentiment, and also possibly emotions like joy, anger,            sadness, and so forth


library(tidytext)

get_sentiments("afinn")
#  AFINN lexicon assigns words with a score that runs between -5 and 5
#> # A tibble: 2,477 × 2
#>    word       value
#>    <chr>      <dbl>
#>  1 abandon       -2
#>  2 abandoned     -2
#>  3 abandons      -2

get_sentiments("bing")
#  bing lexicon categorizes words in a binary fashion into positive and negative       categories
#> # A tibble: 6,786 × 2
#>    word        sentiment
#>    <chr>       <chr>    
#>  1 2-faces     negative 
#>  2 abnormal    negative 
#>  3 abolish     negative 

get_sentiments("nrc")
#  nrc lexicon categorizes words in a binary fashion (“yes”/“no”) into categories of    positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise,     and trust
#> # A tibble: 13,901 × 2
#>    word        sentiment
#>    <chr>       <chr>    
#>  1 abacus      trust    
#>  2 abandon     fear     
#>  3 abandon     negative


#### Finding Sentiment in Jane Austen ####

library(janeaustenr)
library(dplyr)
library(stringr)

# Tidy text of all Jane Austen books with: book, linenumber, chapter, word
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# Find joy lexicons in NCR
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# inner_join to jane austen's Emma
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

#> # A tibble: 303 × 2
#>    word        n
#>    <chr>   <int>
#>  1 good      359
#>  2 young     192
#>  3 friend    166
#>  4 hope      143

# Determine sentiment per 80 line section
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% # Join to sentiment words in bing: positive or negative
  count(book, index = linenumber %/% 80, sentiment) %>% # split text into 80 line sections
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) # add column of average sentiment per section

# Visualise sentiment
jane_austen_sentiment %>% 
  ggplot(aes(index, sentiment, fill = book)) + # create plot with 80 line section vs average sentiment
  geom_col(show.legend = F) + # 
  facet_wrap(~book, ncol = 2, scales = "free_x")# different plot per book, 2 column view and scales are maximised per plot


## Comparing best lexicon sentiment ##
##  Using Pride and Prejudice   ##
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice

# AFINN
afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% # split into 80 lines
  summarise(sentiment = sum(value)) %>%  # 
  mutate(method = "AFINN")

# Bing and NRC
bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative")) # get only positive or negative sentiments from nrc
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative) # average sentiment

# Visualise ncr, bing and afinn
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Conclusions:
#   - AFINN lexicon has the highest lexicon values with most positive
#   - Bing et al. lexicon has most negative values, and longer negative sections
#   - NCR is a mixture of two
#


# Differences in positive and negative words

#  NRC
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)
# sentiment     n
#     <chr>    <int>
# 1 negative   3316
# 2 positive   2308

#  Bing
get_sentiments("bing") %>% 
  count(sentiment)
# sentiment     n
#     <chr>     <int>
# 1 negative   4781
# 2 positive   2005

## Most Common Positive and Negative words ##
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts
#   word     sentiment     n
#    <chr>    <chr>     <int>
# 1 miss     negative   1855   # Error as Miss used for young females not -ve
# 2 well     positive   1523
# 3 good     positive   1380
# 4 great    positive    981

# Visualise
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% # top 10 of positive and negative words
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


# Create custom stop_words to prevent miss being wrongly captured
custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)

custom_stop_words

#### Sentiment by sentence ####

# Tokenise p&p sentence
p_and_p_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

p_and_p_sentences$sentence[2]

# Split books per chapter
austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

# How many chapters does each book have
austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

# For each book, which chapter has the most negative words?
#   Find negative words using Bing lexicon
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

# word count for each chapter
wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n()) # counts words

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>% # add wordcounts
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  slice_max(ratio, n = 1) %>% # choose top numbered negatively worded chapter
  ungroup()