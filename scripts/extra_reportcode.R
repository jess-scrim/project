##### This code either does not work in the .qmd file or is not relevant to the report.

# abstract_unigrams_clean <- read_csv("results/abstract_unigrams_clean.csv")
# abstract_trigrams <- read_csv("results/abstract_trigrams.csv")
# abstract_bigrams <- read_csv("results/abstract_bigrams.csv")


|                      | Estimated Words (\~3447) |
  |----------------------|--------------------------|
  | Introduction         | 1138                     |
  | Methods              | 739                      |
  | Results & Discussion | 1779                     |
  | Abstract             | 169                      |

  
  ::: {#fig-tokenisation layout="[[50,50],[50],[50]]"}
    ```{r}
    #| label: fig-unigram 
    #| height: 4in
    #| width: 3in
    #| fig-subcap:
    #|  - "Unigram"
    abstract_unigrams_clean %>% 
      group_by(type) %>%
      count(word, sort = TRUE) %>%
      slice_head(n = 15) %>%
      ungroup() %>% 
      ggplot(aes(reorder_within(word, n, type), n, fill = type)) +
      geom_col() +
      labs(x = NULL) +
      ylab("Count") +
      theme(
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(size = 1),
        panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black",
                                 size = 1),
        strip.background = element_rect(color="black", 
                                        fill="white", 
                                        size=1, 
                                        linetype="solid")
      ) +
      facet_wrap(~factor(type, 
                         levels=c('pre-leca','post-leca'), 
                         labels = c("Pre-Lecanemab\n Accelerated Approval", "Post-Lecanemab\n Accelerated Approval")), 
                 scale = "free") +
      scale_fill_manual(values = c("black", "darkgrey")) +
      scale_x_reordered() +
      coord_flip() 
    
    ```
    
    ```{r}
    #| label: fig-glm
    #| height: 4in
    #| width: 3.5in
    #| fig-subcap:
    #|  - "Frequency of Most Common Unigrams"
    
    glm_abstracts %>%  
      ggplot(aes(x = date, y = freq)) +
      geom_line(aes(color = word),
                linewidth = 0.5) +
      ylab("Word Frequency") +
      xlab("Month of Publication") +
      scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
      theme(axis.text.x = element_text(angle = 45, 
                                       vjust = 0.5, 
                                       hjust = 0.5),
            # axis.title.x = element_text(size = 10),
            # axis.title.y = element_text(size = 10),
            # strip.text = element_text(size = 10),
            # axis.text.y = element_text(size = 10),
            axis.ticks = element_line(size = 1),
            legend.key = element_rect(fill = "transparent"),
            panel.background = element_blank(),
            legend.position = "top",
            legend.text = element_text(lineheight = .2, size = 10), 
            legend.key.height = unit(1, "cm"),
            axis.line = element_line(colour = "black",
                                     size = 1)
      ) +
      guides(color=guide_legend(nrow=3, byrow=TRUE)) +
      scale_color_manual(values = c("lightblue1", "cyan2", "darkgreen", "darkcyan", "deepskyblue", "cornflowerblue", "blue", "darkblue", "darkmagenta", "midnightblue", "purple", "plum1", "green", "purple4"),
                         name = "Unigram") +
      geom_vline(xintercept = as.numeric(as.Date("2023-01-06")), 
                 linetype = "dashed", 
                 color = "red", 
                 linewidth = 1) 
    ```
    
    ```{r}
    #| label: fig-bigram
    #| height: 4in
    #| width: 3in
    #| fig-subcap:
    #|  - "Bigram"
    
    
    bigram_counts %>% 
      filter(bigram != "alzheimers disease" & bigram != "95 ci") %>% 
      group_by(type) %>%
      slice_head(n = 15) %>% 
      ggplot(aes(reorder_within(bigram, n, type), n, fill = type)) +
      geom_col() +
      labs(x = NULL) +
      ylab("Count") +
      theme(
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(size = 1),
        panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black",
                                 size = 1),
        strip.background = element_rect(color="black", 
                                        fill="white", 
                                        size=1, 
                                        linetype="solid")
      ) +
      facet_wrap(~factor(type, 
                         levels=c('pre-leca','post-leca'), 
                         labels = c("Pre-Lecanemab\n Accelerated Approval", "Post-Lecanemab\n Accelerated Approval")), 
                 scale = "free") +
      scale_fill_manual(values = c("black", "darkgrey")) +
      scale_x_reordered() +
      coord_flip()
    
    ```
    
    ```{r}
    #| label: fig-trigram
    #| height: 4in
    #| width: 3in
    #| fig-subcap:
    #|  - "Trigram"
    
    trigram_counts %>%
      group_by(type) %>%
      slice_head(n = 15) %>%
      ggplot(aes(reorder_within(trigram, n, type), n, fill = type)) +
      geom_col() +
      labs(x = NULL) +
      ylab("Count") +
      theme(
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(size = 1),
        panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black",
                                 size = 1),
        strip.background = element_rect(color="black", 
                                        fill="white", 
                                        size=1, 
                                        linetype="solid")) +
      facet_wrap(~factor(type, 
                         levels=c('pre-leca','post-leca'), 
                         labels = c("Pre-Lecanemab\n Accelerated Approval", "Post-Lecanemab\n Accelerated Approval")), 
                 scale = "free") +
      scale_fill_manual(values = c("black", "darkgrey"))+
      scale_x_reordered() +
      coord_flip()
    
    ```
    
    
    **Literature use before and after the accelerated approval of lecanemab.** Abstract text for `r abstract_n` abstracts was tokenised and stop words were removed. Corpus type was determined based on the accelerated approval date for lecanemab, `r format(as.Date(leca_approv), "%d-%m-%Y")`. The 15 most frequent (a) unigrams, (c) bigrams and (d) trigrams along with (b) the distribution by month of the top 14 shared most frequent unigrams for both corpuses. Dashed-line represents the accelerated approval date of lecanemab, `r format(as.Date(leca_approv), "%d-%m-%Y")`. ns p >0.05, Generalised Linear Model.
    :::  
  
  
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
top_terms_pre_topics <- tidy_lda_pre %>%
  filter(term != "disease" & term != "alzheimers") %>% 
  mutate(topic = case_when(topic == 1 ~ "Genetic Risk",
                           topic == 2 ~ "Drug Discovery",
                           topic == 3 ~ "Cellular Pathology",
                           topic == 4 ~ "Neurodegeneration",
                           topic == 5 ~ "Study Terminology",
                           topic == 6 ~ "Trial Types",
                           topic == 7 ~ "'Cognitive' Association",
                           topic == 8 ~ "Treatments",
                           topic == 9 ~ "AD Diagnosis",
                           topic == 10 ~ "Molecular Pathology")) %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Post-leca
top_terms_post_topics <- tidy_lda_post %>%
  filter(term != "disease" & term != "alzheimers") %>% 
  mutate(topic = case_when(topic == 1 ~ "Treatments",
                           topic == 2 ~ "Epidemiology",
                           topic == 3 ~ "Cognitive Impairment",
                           topic == 4 ~ "AD Diagnosis",
                           topic == 5 ~ "AD Review",
                           topic == 6 ~ "Treatments ",
                           topic == 7 ~ "Molecular Pathology",
                           topic == 8 ~ "Genetic Risk",
                           topic == 9 ~ "Study Terminology",
                           topic == 10 ~ "Cellular Pathology")) %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_pre <- top_terms_pre_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  theme(axis.title.x = element_text(size = 20),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(size = 1),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black",
                                 size = 1),
        strip.background = element_rect(color="black", 
                                        fill="white", 
                                        size = 1, 
                                        linetype="solid")) +
  labs(x = expression(beta), y = NULL) +
  facet_wrap(~factor(topic, levels=c("Genetic Risk",
                                     "Cellular Pathology",
                                     "Study Terminology",
                                     "Treatments",
                                     "AD Diagnosis",
                                     "Molecular Pathology",
                                     "Drug Discovery",
                                     "Neurodegeneration",
                                     "Trial Types",
                                     "'Cognitive' Association"
                                     )), 
             ncol = 5,
             scale = "free") +
scale_fill_manual(values = c("Genetic Risk" = "lightblue1",
                             "Cellular Pathology" = "cyan2",
                             "Study Terminology" = "darkcyan",
                             "Treatments" = "darkblue",
                             "AD Diagnosis" = "purple",
                             "Molecular Pathology" = "darkmagenta",
                             "Drug Discovery" = "grey1",
                             "Neurodegeneration" = "grey1",
                             "Trial Types" = "grey1",
                             "'Cognitive' Association" = "grey1"
                             ))

top_terms_post <- top_terms_post_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL) +
  theme(axis.title.x = element_text(size = 20),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(size = 1),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black",
                                 size = 1),
        strip.background = element_rect(color="black", 
                                        fill="white", 
                                        size = 1, 
                                        linetype="solid")) +
  facet_wrap(~ factor(topic,
                      levels = c("Genetic Risk",
                                 "Cellular Pathology",
                                 "Study Terminology",
                                 "Treatments",
                                 "Treatments ",
                                 "AD Diagnosis",
                                 "Molecular Pathology",
                                 "Epidemiology",
                                 "Cognitive Impairment",
                                 "AD Review"
                      )),                                    
                      ncol = 5, 
                      scales = "free") +
  scale_fill_manual(values = c(
    "Treatments" = "cyan",
    "Treatments " = "darkblue",
    "AD Diagnosis" = "purple",
    "Molecular Pathology" = "darkmagenta",
    "Genetic Risk" = "lightblue1",
    "Study Terminology" = "darkcyan",
    "Cellular Pathology" = "cyan2",
    "AD Review" = "darkgray",
    "Cognitive Impairment" = "darkgray",
    "Epidemiology" = "darkgrey"
  ))

```


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


# Beta value per word and distribution per topic 
top_terms_post_topics %>% 
  ggplot(aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_bar(stat = "identity") +
  facet_wrap(~factor(topic, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), 
             scale = "free") +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5, 
                                   hjust = 0.5,
                                   size = 15),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        strip.text = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_line(size = 1),
        panel.background = element_blank(),
        legend.position = "right",
        axis.line = element_line(colour = "black",
                                 size = 1)
  ) +
  scale_fill_manual(values = c("lightblue1", "cyan2", "darkgreen", "darkcyan", "deepskyblue", "cornflowerblue", "blue", "darkblue", "darkmagenta", "midnightblue", "purple", "plum1", "green", "purple4"),
                    name = "Topic") +
  ylab("Per-Topic-Per-Word Probability, β") +
  xlab("Unigram") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("Change in Per-Topic-Per-Word Probability, β, for Top 10 Unigrams in Post-Lecanemab Topics") +
  theme(legend.position = "none")

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
 
 
 

# Bigram ------------------------------------------------------------------

 {r}
 #| label: fig-beta-change
 #| include: false
 #| cache: true 
 #| fig-width: 20
 #| fig-height: 15 
 #| fig-align: center
 
 #::: {#fig-beta-change}
 
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
 
 top_terms_pre_topics <- tidy_lda_pre %>%
   filter(term != "disease" & term != "alzheimers") %>% 
   mutate(topic = case_when(topic == 1 ~ "Genetic Risk",
                            topic == 2 ~ "Drug Discovery",
                            topic == 3 ~ "Cellular Pathology",
                            topic == 4 ~ "Neurodegeneration",
                            topic == 5 ~ "Study Terminology",
                            topic == 6 ~ "Physical Health",
                            topic == 7 ~ "Brain Function",
                            topic == 8 ~ "Treatments",
                            topic == 9 ~ "AD Diagnosis",
                            topic == 10 ~ "Molecular Pathology")) %>%
   group_by(topic) %>%
   slice_max(beta, n = 10, with_ties = FALSE) %>%
   ungroup() %>%
   arrange(topic, -beta)
 
 top_terms_post_topics <- tidy_lda_post %>%
   filter(term != "disease" & term != "alzheimers") %>% 
   mutate(topic = case_when(topic == 1 ~ "Treatments",
                            topic == 2 ~ "Epidemiology",
                            topic == 3 ~ "Research",
                            topic == 4 ~ "AD Diagnosis",
                            topic == 5 ~ "Publications",
                            topic == 6 ~ "Research",
                            topic == 7 ~ "Molecular Pathology",
                            topic == 8 ~ "Genetic Risk",
                            topic == 9 ~ "Clinical Studies",
                            topic == 10 ~ "Cellular Pathology")) %>%
   group_by(topic) %>%
   slice_max(beta, n = 10, with_ties = FALSE) %>%
   ungroup() %>%
   arrange(topic, -beta)
 
 top_terms_post_topics %>% 
   rename(beta_post = beta) %>%
   left_join(top_terms_pre_topics %>% 
               rename(beta_pre = beta), by = c("term", "topic")) %>%
   mutate(beta_change = beta_post - beta_pre) %>% 
   filter(!is.na(beta_change)) %>%
   ggplot(aes(x = reorder(term, beta_change), y = beta_change, fill = as.factor(topic))) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ factor(topic,
                       levels = c("AD Diagnosis",
                                  "Cellular Pathology",
                                  "Genetic Risk",
                                  "Molecular Pathology",
                                  "Treatments"
                       )),                                    
              ncol = 3, 
              scales = "free") +
   scale_fill_manual(values = c("AD Diagnosis" ="purple", 
                                "Cellular Pathology" = "cyan2",
                                "Genetic Risk" = "darkmagenta", 
                                "Molecular Pathology" = "lightblue1", 
                                "Treatments" = "darkblue"
   )) +
   theme(axis.title.x = element_text(size = 20),
         strip.text = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         axis.ticks = element_line(size = 1),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black",
                                  size = 1),
         strip.background = element_rect(color="black", 
                                         fill="white", 
                                         size = 1, 
                                         linetype="solid")) 
 #Increase in beta values for tau and amyloid
 #:::  
 
 {r}
 #| label: fig-topic-model-bigrams
 #| include: false
 #| fig-cap: "**Bigram LDA Topic Modelling.** Outputs from the post-lecanemab (after 06/01/2023) corpus model with ten topics. Figures show the top ten most commonly associated bigrams in each topic ordered by their per-topic-per-word probability, β. Topic titles were manually created and added."
 #| fig-subcap: 
 #|  - "Pre-leca"
 #|  - "Post-leca"
 #| fig-width: 15
 #| fig-height: 10
 #| fig-align: left
 
 # Pre-leca
 top_bigram_terms_pre
 
 top_bigram_terms_post
 
 
 #### TRIGRAM
 
 {r}
 #| label: fig-topic-model-trigrams
 #| include: false
 #| fig-cap: "**Trigram LDA Topic Modelling.** LDA model shows the top ten most commonly associated trigrams in each topic ordered by their per-topic-per-word probability, β, for (A) pre-leca and (B) post-leca corpuses. Topic titles were manually created and added."
 #| fig-subcap: 
 #|  - "Pre-leca"
 #|  - "Post-leca"
 #| fig-width: 15
 #| fig-height: 10
 #| fig-align: left
 
 # Pre-leca
 top_trigram_terms_pre
 
 top_trigram_terms_post
 