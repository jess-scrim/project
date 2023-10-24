# Get search terms from pubmed https://pubmed.ncbi.nlm.nih.gov/advanced/ 
# Search query: Books and Documents, Case Reports, Clinical Study, Clinical Trial, Controlled Clinical Trial, Meta-Analysis, Randomized Controlled Trial, Review, Systematic Review, English, from 2022/1/2 - 2023/10/10, 'alzheimer', 'alzheimer's disease'
naive_results <- import_results(file="data/pubmed-alzheimerA-set.nbib")

nrow(naive_results)
# 6606
naive_results

# Col names
colnames(naive_results)

### Getting Search terms ###

## Keywords ##

# Keywords in  first paper: [1] "Alzheimerâ€™s disease and GABAergic circuits and disinhibition and hippocampus (CA1) and long-range GABAergic neurons"
naive_results[1, "keywords"]
# Quite a lot of keywords may be NA, n = 765
sum(is.na(naive_results[, "keywords"]))

# Get keywords from all papers 
keywords <- extract_terms(keywords=naive_results[, "keywords"], 
                          method="tagged", 
                          min_n = 1, # allows single words
                          min_freq = 10) # only words that appear at least 10 times in keyword search 
keywords


## Title ##
# As not all papers provide keywords

# First title: [1] "Local and long-range GABAergic circuits in hippocampal area CA1 and their link to Alzheimer's disease."
naive_results[1, "title"]

# Remove stop-words from titles
clin_stopwords <- read_lines("data/clin_stopwords.txt")
all_stopwords <- c(get_stopwords("English"), clin_stopwords)

title_terms <- extract_terms(
  text = naive_results[, "title"],
  method = "fakerake",
  min_freq = 10, 
  min_n = 2,
  stopwords = all_stopwords
)

title_terms

# Combine search terms
search_terms <- c(keywords, title_terms)

# Remove duplicates
search_terms <- unique(search_terms)


### Network analysis ###

# Combine title with abstract
docs <- paste(naive_results[, "title"], naive_results[, "abstract"])
docs[1]

# Create matrix of which term appears in which article
dfm <- create_dfm(elements = docs, 
                  features = search_terms)
# E.g.
dfm[1:3, 1:4]

# Create network of linked terms
g <- create_network(dfm, 
                    min_studies = 3)
ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), 
                 hjust="outward", 
                 check_overlap=TRUE) 
  #guides(edge_alpha=FALSE)

## Pruning ##

# Remove terms that are not connected to other terms - strength
strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

term_strengths

# Visualise to determine cutoff
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

# Find cutoff
cutoff_cum <- find_cutoff(g, 
                          method="cumulative", 
                          percent=0.8)

cutoff_cum

# Add to figure
cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")

# Add cutoffs for changes
cutoff_change <- find_cutoff(g, method="changepoint", knot_num=3)

cutoff_change
cutoff_fig +
  geom_hline(yintercept=cutoff_change, linetype="dashed")

# Extra -------------------------------------------------------------------


# Reduce search-terms
get_keywords(reduce_graph(g, cutoff_cum))

# Reduce
g_redux <- reduce_graph(g, cutoff_change[1])
selected_terms <- get_keywords(g_redux)

selected_terms


### Grouping ###
grouped_terms <-list(
  alzheimer = selected_terms[c(5, 6, 7)],
  neurodegeneration = selected_terms[c(2, 4, 9, 42, 69, 70, 71)]
)

grouped_terms

# Write a new search
write_search(
  grouped_terms,
  languages="English",
  exactphrase=TRUE,
  stemming=FALSE,
  closure="left",
  writesearch=TRUE
)

read_in <- cat(read_file("scripts/search-inEnglish.txt"))
# \(\(alzheimer\) AND \(aging OR als OR "amyotrophic lateral sclerosis" OR "huntington's disease" OR parkin\)\)

# Write a new search
new_results <- import_results(file=) #"pubmed-pharmacoth-set.nbib")

# Check how many this gets         
nrow(new_results)
# 

# ## Check naive results were included ##
# naive_results %>%
#   mutate(in_new_results=title %in% new_results[, "title"]) ->
#   naive_results
# 
# naive_results %>%
#   filter(!in_new_results) %>%
#   select(title, keywords)
# 
# ## Gold standard test ##
# # Check important texts were included
# important_titles <- c(
#   "Efficacy of treatments for anxiety disorder: A meta-analysis",
#   "Cognitive behaviour therapy for health anxiety: A systematic review and meta-analysis",
#   "A systematic review and meta-analysis of treatments for agrophobia"
# )
# 
# data.frame(check_recall(important_titles, new_results[, "title"]))