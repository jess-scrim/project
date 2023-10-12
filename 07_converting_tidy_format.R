library(tm)
library(topicmodels)

data("AssociatedPress", package = "topicmodels")
AssociatedPress # data source from independent new article with 2246 documents with 10473 terms, 99% sparse with zero value in the matrix
terms <- Terms(AssociatedPress)
head(terms)

library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress) # tidy() puts into tidydata format
ap_td
# Add sentiment
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments
# Visualise
library(ggplot2)

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col() +
  labs(x = "Contribution to sentiment", y = NULL)

#### DFM approach ####
# Using inaugral speeches of US presidents
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- data_corpus_inaugural %>%
  quanteda::tokens() %>%
  quanteda::dfm(verbose = FALSE)
inaug_dfm
#> Document-feature matrix of: 59 documents, 9,439 features (91.84% sparse) and 4 docvars.

#tidy
inaug_td <- tidy(inaug_dfm)
inaug_td

# Calculate tf-idf - term frequency - i.e. which word is most associated with each inaugral speech
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf
# Visualise
inaug_tf_idf %>%
  filter(document %in% c("1861-Lincoln", "1933-Roosevelt", "1961-Kennedy", "2009-Obama")) %>% 
  group_by(document) %>%
  slice_max(tf_idf, n = 15) %>% # top 15 words per book
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(term, tf_idf), fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# 
library(tidyr)

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))
year_term_counts
# How frequency of words change over time
year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "% frequency of word in inaugural address")


## Reversing casting
ap_td %>%
  cast_dtm(document, term, count)

ap_td %>%
  cast_dfm(document, term, count)

library(Matrix)

# cast into a Matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)
#> [1] "dgCMatrix"
#> attr(,"package")
#> [1] "Matrix"
dim(m)
#> [1]  2246 10473

# E.g. From Jane Austen
library(janeaustenr)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm


#### Tidying Corpus data
## NB// Does not Install ##
data("acq")
acq
#> <<VCorpus>>
#> Metadata:  corpus specific: 0, document level (indexed): 0
#> Content:  documents: 50

# first document
acq[[1]]
#> <<PlainTextDocument>>
#> Metadata:  15
#> Content:  chars: 1287

# Construct a one row per doc table
acq_td <- tidy(acq)
acq_td
#> # A tibble: 50 × 16
#>    author        datetimestamp       descr…¹ heading id    langu…² origin topics
#>    <chr>         <dttm>              <chr>   <chr>   <chr> <chr>   <chr>  <chr> 
#>  1 <NA>          1987-02-26 15:18:06 ""      COMPUT… 10    en      Reute… YES   
#>  2 <NA>          1987-02-26 15:19:15 ""      OHIO M… 12    en      Reute… YES   
#>  3 <NA>          1987-02-26 15:49:56 ""      MCLEAN… 44    en      Reute… YES   

# Tokenise, then find the most common word
acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# most common words
acq_tokens %>%
  count(word, sort = TRUE)
#> # A tibble: 1,566 × 2
#>    word         n
#>    <chr>    <int>
#>  1 dlrs       100
#>  2 pct         70
#>  3 mln         65
#>  4 company     63
#>  5 shares      52

# Calculate tf-idf
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))
#> # A tibble: 2,853 × 6
#>    id    word         n     tf   idf tf_idf
#>    <chr> <chr>    <int>  <dbl> <dbl>  <dbl>
#>  1 186   groupe       2 0.133   3.91  0.522
#>  2 128   liebert      3 0.130   3.91  0.510


### E.g. Extract from Web
## NB// DOES NOT WORK ##
library(tm.plugin.webmining)
library(purrr)

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol  <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", 
             "TWTR", "IBM", "YHOO", "NFLX")

download_articles <- function(symbol) {
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}
stock_articles <- tibble(company = company,
                         symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))

## Did not continute ###