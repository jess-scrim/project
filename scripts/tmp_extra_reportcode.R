### Unigrams

#```{r}
#| label: LDA Topic Modelling
#| include: false
#| cache: true

word_count_pre <- tidy_abstracts_clean %>%
  filter(type == "pre-leca") %>% 
  count(word, abstract, sort = TRUE) %>% 
  ungroup()

word_count_post <- tidy_abstracts_clean %>%
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
  filter(term != "disease") %>% 
  mutate(topic = case_when(topic == 1 ~ "Clinical Studies",
                           topic == 2 ~ "Studies for Diagnosis",
                           topic == 3 ~ "Risk Factors and Symptoms",
                           topic == 4 ~ "Cells",
                           topic == 5 ~ "Inconclusive",
                           topic == 6 ~ "Pathological mechanisms",
                           topic == 7 ~ "Genetics",
                           topic == 8 ~ "Abnormal Proteins",
                           topic == 9 ~ "Study criteria",
                           topic == 10 ~ "Brain")) %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Post-leca
top_terms_post <- tidy_lda_post %>%
  filter(term != "disease") %>% 
  mutate(topic = case_when(topic == 1 ~ "Study Criteria",
                           topic == 2 ~ "Brain",
                           topic == 3 ~ "Inconclusive",
                           topic == 4 ~ "Inconclusive",
                           topic == 5 ~ "Risk Factors",
                           topic == 6 ~ "Treatments",
                           topic == 7 ~ "Clinical Study",
                           topic == 8 ~ "Diagnosis",
                           topic == 9 ~ "Proteins",
                           topic == 10 ~ "Risk Factor - Diabetes")) %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_post <- top_terms_post %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic: Post Leca",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

top_terms_pre <- top_terms_pre %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic: Pre-Leca",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
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