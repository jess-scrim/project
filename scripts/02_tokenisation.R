source("scripts/00_setting_up.R")
#tidy_abstracts_clean <- read_csv("results/tidy_abstracts_clean.csv")

# tidy text
tidy_abstracts <- abstracts %>% 
  filter(!text == "NA") %>% 
  mutate(type = case_when(date <= leca_approv ~ "pre-leca",
                          date > leca_approv ~ "post-leca")) 

# remove stopwords
data(stop_words)


tidy_abstracts_clean <- tidy_abstracts %>% 
  unnest_tokens(word, text)
tidy_abstracts_clean <- tidy_abstracts_clean %>%
  anti_join(stop_words)
tidy_abstracts_clean <- tidy_abstracts_clean %>%
  anti_join(my_stopwords)

### Plot most common words ###

## All abstracts
tidy_abstracts_clean %>% 
  group_by(type) %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 15) %>% 
  ggplot(aes(n, reorder(word, n), fill = type)) +
  geom_col() +
  labs(y = NULL) +
  xlab("Count") +
  facet_wrap(~factor(type, levels = c("pre-leca", "post-leca")),
             scale = "free") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank())

## Create Generalised linear model

top_words <- c("disease", "brain", "studies", "diseases", "review", "cognitive", "dementia", "neurodegenerative", "clinical", "patients", "treatment", "disorders", "effects")

# Get word frequency per month
glm_abstracts <- tidy_abstracts_clean %>%  
  filter(word %in% top_words) %>% 
  mutate(date = floor_date(date, "month")) %>% # round date to month
  group_by(date) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() 

# Generalised linear model
glm <- glm( freq ~ date + word, data = glm_abstracts, family = "poisson" )
summary(glm)

# plot just months of date

glm %>% ggplot(aes(x = date, y = freq)) +
  geom_line(aes(color = word)) +
  ylab("Frequency") +
  xlab("Date") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
  theme_classic()

# Call:
#   glm(formula = freq ~ date + word, family = "poisson", data = glm_abstracts)
# 
# Deviance Residuals: 
#   Min         1Q     Median         3Q        Max  
# -0.202554  -0.020064   0.001664   0.027135   0.119861  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)           -5.994e-01  2.778e+01  -0.022    0.983
# date                  -1.290e-04  1.438e-03  -0.090    0.928
# wordclinical           4.052e-13  1.414e+00   0.000    1.000
# wordcognitive          1.321e-12  1.414e+00   0.000    1.000
# worddementia           2.191e-12  1.414e+00   0.000    1.000
# worddisease           -6.794e-13  1.414e+00   0.000    1.000
# worddiseases           6.596e-13  1.414e+00   0.000    1.000
# worddisorders         -4.261e-13  1.414e+00   0.000    1.000
# wordeffects            7.784e-13  1.414e+00   0.000    1.000
# wordneurodegenerative -4.503e-13  1.414e+00   0.000    1.000
# wordpatients           6.286e-13  1.414e+00   0.000    1.000
# wordreview            -1.191e-12  1.414e+00   0.000    1.000
# wordstudies            2.429e-13  1.414e+00   0.000    1.000
# wordtreatment          7.127e-13  1.414e+00   0.000    1.000
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 0.76138  on 285  degrees of freedom
# Residual deviance: 0.75333  on 272  degrees of freedom
# AIC: Inf
# 
# Number of Fisher Scoring iterations: 5

## Individual words
# count by month
tidy_abstracts_clean %>% filter(word == "beta") %>% 
  mutate(date = floor_date(date, "month")) %>% # round date to month
  group_by(date) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>% 
  ggplot(aes(x = date, y = freq)) +
  geom_line() +
  ylab("Frequency") +
  xlab("Date") +
  ggtitle("Beta") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
  theme_classic()
