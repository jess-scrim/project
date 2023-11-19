# Get search terms from pubmed https://pubmed.ncbi.nlm.nih.gov/advanced/ 
#
#  Search query: 
# '("alzheimer disease"[MeSH Terms] OR ("alzheimer"[All Fields] AND "disease"[All Fields]) OR "alzheimer disease"[All Fields] OR ("alzheimer s"[All Fields] AND "disease"[All Fields]) OR "alzheimer s disease"[All Fields]) AND ((fha[Filter]) AND (booksdocs[Filter] OR casereports[Filter] OR clinicalstudy[Filter] OR clinicaltrial[Filter] OR controlledclinicaltrial[Filter] OR meta-analysis[Filter] OR randomizedcontrolledtrial[Filter] OR review[Filter] OR systematicreview[Filter]) AND (2022/1/2:2023/10/10[pdat]) AND (english[Filter]))'
#
# Data Accessed: 19/11/2023 

naive_results <- import_results(file="data/pubmed-alzheimerd-set.nbib")

nrow(naive_results)
# 6,738

### Getting Search terms ###

## Keywords ##

# Keywords in first paper: 
naive_results[1, "keywords"]
# [1] "Alzheimerâ€™s disease and bloodâ€“brain-barrier and exosomes and intranasal administration and liposomes and polyunsaturated fatty acids and therapeutics strategies"

# Quite a lot of keywords may be NA, n = 780
sum(is.na(naive_results[, "keywords"]))

# Get keywords from all papers 
keywords <- extract_terms(keywords=naive_results[, "keywords"], 
                          method="tagged", 
                          min_n = 1, # allows single words
                          min_freq = 50) # only words that appear at least 10 times in keyword search 
keywords


## Title ##
# As not all papers provide keywords

# First title: 
naive_results[1, "title"]
# [1] "Alzheimer's Disease: Treatment Strategies and Their Limitations."


# Remove stop-words from titles
clin_stopwords <- read_lines("data/clin_stopwords.txt")
all_stopwords <- c(get_stopwords("English"), clin_stopwords)

title_terms <- extract_terms(
  text = naive_results[, "title"],
  method = "fakerake",
  min_freq = 75, 
  min_n = 1,
  stopwords = all_stopwords
)

# Combine search terms & remove duplicates
search_terms <- c(keywords, title_terms) %>% unique()


### Network analysis ###

# Combine title with abstract
docs <- paste(naive_results[, "title"], naive_results[, "abstract"])

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
  mutate(rank = rank(strength, 
                   ties.method="min")) %>%
  arrange(strength)

# Visualise to determine cutoff
cutoff_fig <- ggplot(term_strengths, aes(x=rank, 
                                         y=strength, 
                                         label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

# Find 80% cutoff
cutoff_cum <- find_cutoff(g, 
                          method="cumulative", 
                          percent=0.8)

# Add to figure
cutoff_fig +
  geom_hline(yintercept = cutoff_cum, 
             linetype = "dashed")

# Add cutoffs for changes
cutoff_change <- find_cutoff(g, 
                             method = "changepoint", 
                             knot_num = 3)
cutoff_fig +
  geom_hline(yintercept = cutoff_change, 
             linetype="dashed")