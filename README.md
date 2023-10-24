# *Impact of Novel Alzheimer's Disease Drugs Discovery on the Research Field Using Text Mining and Topic Models*

## Aims:

1.  Aim 1
2.  Aim 2

## Hypotheses:

1.  Hypothesis 1

2.  Hypothesis 2

## Results

```         
#> [1] "This is a test"

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
```
