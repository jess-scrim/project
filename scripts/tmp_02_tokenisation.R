# Create corpus
my_corpus <- VCorpus(VectorSource(txt))
my_corpus

writeLines(head(strwrap(my_corpus[[2]]), 15)) # print first 15 lines

# Create a function called "addspace" that finds a user specified pattern and substitutes the pattern with a space.
addspace <- content_transformer(function(x, pattern) {
  return(gsub(pattern, " ", x))
})
my_corpus <- tm_map(my_corpus, addspace, "-") # remove hyphens, add space
writeLines(head(strwrap(my_corpus[[2]]), 15)) # print first 15 lines

# remove all punctuation
my_corpus <- tm_map(my_corpus, removePunctuation)
my_corpus <- tm_map(my_corpus, removeNumbers)
my_corpus <- tm_map(my_corpus, removeWords, stopwords("english"))

# Transform to lower case (need to wrap in content_transformer)
my_corpus <- tm_map(my_corpus,content_transformer(tolower))
my_corpus <- tm_map(my_corpus, stripWhitespace)

writeLines(head(strwrap(my_corpus[[2]]), 15))

# Applies Porter's word stemmer 
my_corpus <- tm_map(my_corpus, stemDocument)

writeLines(head(strwrap(my_corpus[[2]]), 15))
