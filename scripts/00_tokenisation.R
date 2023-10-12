#### Using unnest_tokens() function ####

## Basic: Emily Dickinson Poem ##

# Produce tibble
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

# Produce data frame

library(readr)
library(tidyverse)
text_df <- tibble(line = 1:4, text = text)

text_df

# Produce tokenisation (separate sentences into words)
#   Removes punctuation
#   Converts to lowercase
library(tidytext)

text_df %>%
  unnest_tokens(word, text)


#### Tidying Jane Austen ####

#     janeaustenr package provides text in one-row-per-line format

library(janeaustenr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>% 
  mutate(linenumber = row_number(), # convert row_number to column name 'linenumber'
         chapter = cumsum(str_detect(text, # new chapter when text starts
                                     regex("^chapter [\\divxlc]", # with 'Chapter'
                                           ignore_case = TRUE)))) %>%
  ungroup()

original_books

# separate individual words rather than text

tidy_books <- original_books %>% 
  unnest_tokens(word, text)

tidy_books # n = 725,055 words

# Remove 'stop words', i.e. common English words such as 'of' 'the' 'to'

data(stop_words) # collect stop word library

tidy_books_clean <- tidy_books %>%
  anti_join(stop_words) # removes words which match in stop_words & tidy_books
# n = 217,609 words

# OR... Find most common words in words list (could then remove this way)
words <- tidy_books %>%
  count(word, sort = TRUE) %>% 
  filter(n > 1000 & !word == "mr" &!word == "mrs" ) %>% 
  select(word)

tidy_books_clean <- tidy_books %>% 
  anti_join(words, by = "word") 
  

#### Visualise most common words > 600 counts ####

library(ggplot2)

tidy_books %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)