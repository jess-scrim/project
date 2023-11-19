# Get search terms from pubmed https://pubmed.ncbi.nlm.nih.gov/advanced/ 

### Defining the search terms for Alzheimer's drug lecanemab ###

# Using (((lecanemab) OR (leqembi)) OR (BAN2401)) OR (mAb158), n = 207
naive_drug_results <- import_results(file="data/pubmed-lecanemabO-set.nbib")
# Using "lecanemab", n = 178
naive_drug_results <- import_results(file="data/pubmed-lecanemab-set.nbib")
# Using "BAN2401", n = 194
naive_drug_results <- import_results(file="data/pubmed-BAN2401-set.nbib")
# Using "mAb158", n = 18
naive_drug_results <- import_results(file="data/pubmed-mAb158-set.nbib")

nrow(naive_drug_results)

### Getting Search terms ###

## Keywords ##

# Get keywords from all papers 
keywords <- extract_terms(keywords=naive_drug_results[, "keywords"], 
                          method="tagged", 
                          min_n = 1, # allows single words
                          min_freq = 2) # only words that appear at least 10 times in keyword search 

## Title ##
# As not all papers provide keywords

# Remove stop-words from titles
clin_stopwords <- read_lines("data/clin_stopwords.txt")
all_stopwords <- c(get_stopwords("English"), clin_stopwords)

title_terms <- extract_terms(
  text = naive_drug_results[, "title"],
  method = "fakerake",
  min_freq = 2, 
  min_n = 1,
  stopwords = all_stopwords
)

# Combine search terms & remove duplicates
search_terms <- c(keywords, title_terms) %>% unique()

## Network analysis ###

# Combine title with abstract
docs <- paste(naive_drug_results[, "title"], naive_drug_results[, "abstract"])

# Create matrix of which term appears in which article
dfm <- create_dfm(elements = docs, 
                  features = search_terms)

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

## Pruning ##

# Remove terms that are not connected to other terms - strength
strengths <- strength(g)

term_strengths <- data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength)

# Visualise to determine cutoff
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

# Find 80% cutoff
cutoff_cum <- find_cutoff(g, 
                          method="cumulative", 
                          percent=0.8)

# Add to figure
cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")

# Add cutoffs for changes
cutoff_change <- find_cutoff(g, method="changepoint", knot_num=3)

cutoff_fig +
  geom_hline(yintercept=cutoff_change, linetype="dashed")

