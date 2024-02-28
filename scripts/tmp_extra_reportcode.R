##### This code either does not work in the .qmd file or is not relevant to the report.

## Distribution of search terms
There were `r nrow(filter(naive_drug_results, naive_drug_results$title %in% abstracts$title))` papers published between `r min(format(as.Date(naive_drug_results$date_published, format = "%Y %b %d"),"%d-%m-%Y"), na.rm = T)` and `r max(format(as.Date(naive_drug_results$date_published, format = "%Y %b %d"),"%d-%m-%Y"), na.rm = T)` that contained the identified terms.

```{r}
#| label: tbl-inclusion-criteria
#| include: true
#| tbl-cap: "Inclusion Criteria"
#| tbl-width: 10
#| tbl-height: 3.5
#| tbl-align: left

criteria <- data.frame(x = c("**Criteria:**",
                             "MeSH term",
                             "Article Type",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "Publication Date",
                             "Language"),
                       y = c("**Filter applied:**",
                             "‘Alzheimer’s Disease’",
                             "Books",
                             "Case Reports",
                             "Clinical Study",
                             "Clinical Trial",
                             "Controlled Clinical Trial",
                             "Meta-analysis",
                             "Randomised Controlled Trial",
                             "Review",
                             "Systematic Review",
                             "1st January 2022 to 1st January 2024 inclusive",
                             "English")
)

# make dataframe filter and criteria

knitr::kable(criteria, col.names = NULL)

```

```{r}
#| label: drug-search-terms
#| include: false 
#| cache: true

naive_drug_results <- import_results(file="data/pubmed-lecanemabO-set.nbib")

keywords <- extract_terms(keywords=naive_drug_results[, "keywords"], 
                          method="tagged", 
                          min_n = 1, # allows single words
                          min_freq = 2) # only words that appear at least 2 times in keyword search 

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

```

### Unigrams

#```{r}
#| label: LDA Topic Modelling
#| include: false
#| cache: true

word_count_pre <- abstract_unigrams_clean %>%
  filter(type == "pre-leca") %>% 
  count(word, abstract, sort = TRUE) %>% 
  ungroup()

word_count_post <- abstract_unigrams_clean %>%
  filter(type == "post-leca") %>% 
  count(word, abstract, sort = TRUE) %>% 
  ungroup()

# Cast the word counts into a document term matrix
abstract_dtm_pre <- word_count_pre %>%
  cast_dtm(abstract, word, n) 

abstract_dtm_post <- word_count_post %>%
  cast_dtm(abstract, word, n)

# Running the LDA model
abstract_lda_pre <- LDA(abstract_dtm_pre, k = 10, control = list(seed = 1234))

abstract_lda_post <- LDA(abstract_dtm_post, k = 10, control = list(seed = 1234))

tidy_lda_pre <- tidy(abstract_lda_pre,
                     matrix = "beta") %>% 
  mutate(type = "pre_leca")
tidy_lda_post <- tidy(abstract_lda_post,
                      matrix = "beta") %>% 
  mutate(type = "post_leca")

# Pre-leca
top_terms_pre <- tidy_lda_pre %>%
  filter(term != "disease" & term != "alzheimers") %>% 
  mutate(topic = case_when(topic == 1 ~ "1: Risk Factors",
                           topic == 2 ~ "2: Neurodegeneration Review",
                           topic == 3 ~ "3: Cellular Pathology",
                           topic == 4 ~ "4: Neurological Disease",
                           topic == 5 ~ "5: Study Terminology",
                           topic == 6 ~ "6: Physical Health",
                           topic == 7 ~ "7: Cognitive Impairment",
                           topic == 8 ~ "8: Treatments",
                           topic == 9 ~ "9: Diagnostics",
                           topic == 10 ~ "10: Abnormal Proteins")) %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Post-leca
top_terms_post <- tidy_lda_post %>%
  filter(term != "disease" & term != "alzheimers") %>% 
  mutate(topic = case_when(topic == 1 ~ "1: Treatments",
                           topic == 2 ~ "2: Study Terminology",
                           topic == 3 ~ "3: Health research",
                           topic == 4 ~ "4: Diagnostics",
                           topic == 5 ~ "5: Studies",
                           topic == 6 ~ "6: Neurological Research",
                           topic == 7 ~ "7: Abnormal Proteins",
                           topic == 8 ~ "8: Genetics",
                           topic == 9 ~ "9: Clinical Studies",
                           topic == 10 ~ "10: Cellular Pathology")) %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_pre <- top_terms_pre %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  theme(axis.title.x = element_text(size = 15),
        strip.text = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  labs(x = expression(beta), y = NULL) +
  facet_wrap(~factor(topic, levels=c("1: Risk Factors",
                                     "2: Neurodegeneration Review",
                                     "3: Cellular Pathology",
                                     "4: Neurological Disease",
                                     "5: Study Terminology",
                                     "6: Physical Health",
                                     "7: Cognitive Impairment",
                                     "8: Treatments",
                                     "9: Diagnostics",
                                     "10: Abnormal Proteins")), 
             ncol = 5,
             scale = "free") 
  # scale_fill_manual(values = c("1: Abnormal Proteins" = "brown",
  #                            "2: Neurodegeneration review" = "purple",
  #                            "3: Treatments" = "darkgreen",
  #                            "4: Study Terminology" = "lightgreen",
  #                            "5: Study Terminology" = "lightgreen",
  #                            "6: Risk Factors" = "red",
  #                            "7: Physical Health" = "grey",
  #                            "8: Diagnosis" = "darkblue",
  #                            "9: Cellular Pathology" = "yellow",
  #                            "10: Diagnosis" = "darkblue"))

top_terms_post <- top_terms_post %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL) +
  theme(axis.title.x = element_text(size = 20),
        strip.text = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  facet_wrap(~ factor(topic,
                      levels = c("1: Treatments",
                                 "2: Study Terminology",
                                 "3: Health research",
                                 "4: Diagnostics",
                                 "5: Studies",
                                 "6: Neurological Research",
                                 "7: Abnormal Proteins",
                                 "8: Genetics",
                                 "9: Clinical Studies",
                                 "10: Cellular Pathology"
                      )),                                    
                      ncol = 5, 
                      scales = "free") 
  # scale_fill_manual(values = c(
  #   "1: Cellular Pathology" = "yellow",
  #   "2: Study Terminology" = "lightgreen",
  #   "3: Diagnosis" = "darkblue",
  #   "4: Common AD Abstract Terms" = "orange",
  #   "5: Risk Factors" = "red",
  #   "6: Treatments" = "darkgreen",
  #   "7: Risk Factors" = "red",
  #   "8: Clinical Trials" = "lightblue",
  #   "9: Abnormal Proteins" = "brown",
  #   "10: Neurodegeneration review" = "purple"
  # ))
#```

The top 10 terms in each pre- and post- leca corpus are shown in fig-topic-model-pre and fig-topic-model-post respectively.

#```{r}
#| label: fig-topic-model-pre
#| include: true
#| fig-cap: "LDA Topic Modelling"
#| fig-width: 15
#| fig-height: 10
#| fig-align: left

# Visualise - top 10 terms per topic
top_terms_pre

#```

#```{r}
#| label: fig-topic-model-post
#| include: true
#| fig-cap: "LDA Topic Modelling"
#| fig-width: 15
#| fig-height: 10
#| fig-align: left
top_terms_post
#```

# Filter for only relatively common combinations
bigram_graph <- bigrams_united %>%
  count(bigram) %>% 
  filter(n > 200) %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_graph <- bigram_graph %>%
  graph_from_data_frame() # most common word1->word2

set.seed(2017) # set random 2017

# Pre-leca
pre_leca_graph <- bigrams_united %>%
  filter(type == "pre-leca") %>%
  count(bigram) %>% 
  filter(n > 200) %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

pre_leca_graph <- pre_leca_graph %>%
  graph_from_data_frame()

# Post-leca
post_leca_graph <- bigrams_united %>%
  filter(type == "post-leca") %>%
  count(bigram) %>% 
  filter(n > 200) %>% 
  separate(bigram, c("word1", "word2"), sep = " ")
post_leca_graph <- post_leca_graph %>%
  graph_from_data_frame()

### Bigram Networks

Relations of bigram networks are shown in @fig-bigram-visualisation.

```{r}
#| label: fig-bigram-visualisation
#| include: true
#| fig-cap: " **‘Disease’ commonly occurs with ‘neurodegenerative’ and ‘parkinson’s’ in bigram relationship networks.** (A) All abstracts. (B) Pre-leca bigram corpus. (C) Post-leca bigram corpus. Increased thickness indicates increased frequency."
#| fig-subcap: 
#|  - "All Bigram Relations"
#|  - "Pre-leca Bigram Relations"
#|  - "Post-leca Bigram Relations"
#| fig-width: 10
#| fig-height: 5
#| fig-align: left

set.seed(2020)

bigram_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void() 

# Pre-leca
pre_leca_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void() 

# Pre-leca
post_leca_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void() 
```
As '*disease*' was the most common unigram, we explored the most frequent bigrams that end in '*disease*' (@fig-bigram-plot). We also explored the most common bigrams that begin with '*neuro*' and those that end in '*neuro*' to understand the context in which these words are used.

```{r}
#| label: fig-bigram-plot
#| include: true
#| fig-cap: "*Bigrams exploration*"
#| fig-subcap: 
#|  - "Most common bigrams ending in 'disease'"
#|  - "Most common bigrams beginning with 'neuro'"
#|  - "Most common bigrams ending in 'neuro'"
#| fig-width: 15
#| fig-height: 10
#| fig-align: left

set.seed(1234)

# bigrams_separated %>%
#   filter(word2 == "disease") %>%
#   count(word1, sort = TRUE) %>% 
#   slice_head(n = 15) %>% 
#   rename("Word" = word1) %>%
#   knitr::kable()

bigrams_separated %>% 
  filter(word2 == "disease") %>%
  count(word1, sort = TRUE) %>% 
  wordcloud2::wordcloud2(size = 1)

# bigrams_separated %>%
#   filter(grepl("^neuro", word1)) %>%
#   count(word2, sort = TRUE) %>% 
#   slice_head(n = 15) %>% 
#   rename("Word" = word2) %>%
#   knitr::kable()
set.seed(1234)

# bigrams_separated %>% 
#   filter(grepl("^neuro", word1)) %>%
#   count(word2, sort = TRUE) %>% 
#   wordcloud2::wordcloud2(size = 1)
# 
# bigrams_separated %>%
#   filter(grepl("^neuro", word2)) %>%
#   count(word1, sort = TRUE) %>% 
#   slice_head(n = 15) %>% 
#   rename("Word" = word1) %>%
#   knitr::kable()
# set.seed(1234)
# 
# bigrams_separated %>% 
#   filter(grepl("^neuro", word2)) %>%
#   count(word1, sort = TRUE) %>% 
#   wordcloud2::wordcloud2(size = 1)
```

### Bigrams

The top 10 bigrams in topic models for pre- and post- leca corpuses are shown in @fig-topic-model-bigrams. We removed the bigrams '*neurodegenerative disease*' and '*parkinson's disease*' from the models as they were present in all topics. Additionally we removed '*amyloid-beta*' from the pre-leca model as this was also present in all topics.

Then most frequent terms:

-   '**cognitive impairment**' was common to 8/10 pre-leca models and 9/10 post-leca

-   '**amyloid beta**' was common to 8/10 post-leca models 

Unique terms which define each topic:

Themes of topics **pre-leca**:

-   **Topic 1:** Risk factors ? - 'diabetes t2d', 'insulin resistance', '

-   **Topic 2:**
  
  -   **Topic 3:**
  
  -   **Topic 4:** "Covid 19" - high beta value

-   **Topic 5:**
  
  -   **Topic 6:** "Amyloid beta" - high beta value

-   **Topic 7:**
  
  -   **Topic 8:**
  
  -   **Topic 9:**
  
  -   **Topic 10:**
  
  Themes of topics **post-leca:**
  
  -   Topic 1: neurodegenerative diseases - "huntington's disease", "ALS"

-   Topic 2:
  
  -   Topic 2:
  
  -   Topic 2:
  
  -   Topic 2:
  
  -   Topic 2:
  
  -   Topic 2:
  


## Trigrams

The top 10 trigrams in topic models for pre- and post- leca corpuses are shown in @fig-topic-model-trigrams. We removed the trigrams '*central nervous system*' and '*parkingson\'s disease pd*' from the models as they were present in all topics and did not provide any additional information.

Themes of topics **pre-leca**:

-   Topic 1:

-   Topic 2:

-   Topic 3:

-   Topic 4: Covid (?)

-   Topic 5:

-   Topic 6:

-   Topic 2:

Themes of topics **post-leca:**

-   Topic 1:

-   Topic 2:

-   Topic 3: "chinese patent medicine"

-   Topic 4: "inflammatory bowel disease"

-   Topic 5: "bstructive sleep apnea"

-   Topic 6:

-   Topic 7: "glp 1 ras"

-   Topic 8:

-   Topic 9: "traditional chinese medicine"

-   Topic 10: "donepezil patch 27.5", "patch 27.5 mg"


# open a txt file
 fileConn<-file("output.txt")
 
 # write the data to the txt file
 writeLines("This is a test.", fileConn)