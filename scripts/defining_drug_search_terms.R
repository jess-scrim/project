# Get search terms from pubmed https://pubmed.ncbi.nlm.nih.gov/advanced/ 

### Defining the search terms for Alzheimer's drug lecanemab

naive_drug_results <- import_results(file="data/pubmed-mAb158-set.nbib")

nrow(naive_drug_results)
# n = 201
naive_drug_results

# col names
colnames(naive_drug_results)

### Getting Search terms ###

## Keywords ##

# Keywords in  first paper: [1] 
naive_drug_results[1, "keywords"]
# [1] "Aging-associated cognitive decline and Alzheimerâ€™s disease and Amyloid Cascade Hypothesis 2.0 (ACH2.0) and BACE1 and BACE2 activators and clinical trial design and donanemab and intraneuronal AÎ² and lecanemab and verubecestat"
sum(is.na(naive_drug_results[, "keywords"]))
# 86

# Get keywords from all papers 
keywords <- extract_terms(keywords=naive_drug_results[, "keywords"], 
                          method="tagged", 
                          min_n = 1, # allows single words
                          min_freq = 5) # only words that appear at least 10 times in keyword search 
keywords

## Title ##
# As not all papers provide keywords
naive_drug_results[1, "title"]
# [1] "Principles of Design of Clinical Trials for Prevention and Treatment of Alzheimer's Disease and Aging-Associated Cognitive Decline in the ACH2.0 Perspective: Potential Outcomes, Challenges, and Solutions."

# Remove stop-words from titles
clin_stopwords <- read_lines("data/clin_stopwords.txt")
all_stopwords <- c(get_stopwords("English"), clin_stopwords)

title_terms <- extract_terms(
  text = naive_drug_results[, "title"],
  method = "fakerake",
  min_freq = 4, 
  min_n = 2,
  stopwords = all_stopwords
)
title_terms

# Combine search terms
search_terms <- c(keywords, title_terms)

# Remove duplicates
search_terms <- unique(search_terms)
search_terms

## Network analysis ###

# Combine title with abstract
docs <- paste(naive_drug_results[, "title"], naive_drug_results[, "abstract"])
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

