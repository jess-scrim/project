#### Comparing Word Frequencies in Books ####

library(gutenbergr) # From 'Project Gunther' aim to download books - NB// Package no longer available :(

hgwells <- gutenbergr::gutenberg_download(c(35, 36, 5230, 159)) # Chooses IDs of H.G. Wells books
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) # Remove stop words

tidy_hgwells %>%
  count(word, sort = TRUE) # Count most common words
#> # A tibble: 11,769 × 2
#>    word       n
#>    <chr>  <int>
#>  1 time     454
#>  2 people   302


# Repeat for Bronte sisters
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_bronte %>%
  count(word, sort = TRUE)

# Calculate the frequency of each word in Jane Austen, H.G. Wells and Bronte

library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%  # UTF-8 encoded texts from Project Gutenberg have some examples of words with underscores around them to indicate emphasis (like italics). The tokenizer treated these as words, but we don’t want to count “_any_” separately from “any” etc.
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer(`Brontë Sisters`:`H.G. Wells`,
               names_to = "author", values_to = "proportion")

#> # A tibble: 57,820 × 4
#>    word    `Jane Austen` author          proportion
#>    <chr>           <dbl> <chr>                <dbl>
#>  1 a          0.00000919 Brontë Sisters  0.0000319 
#>  2 a          0.00000919 H.G. Wells      0.0000150 
#>  3 a'most    NA          Brontë Sisters  0.0000159 

# Plot - Frequency of Jane Austen, Bronte & H.G. Wells
library(scales)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

#  - Names close to the line have similar frequencies in both texts
#  - Names close to the right are more common in Jane Austen than Bronte or Wells etc. 
#  - Austen & Bronte words are closer to sloped line and are at lower frequency, so are more similar
#  - Austen & Wells words are further to sloped line and there is a gap at low frequency, so are more different


# Quantifying word frequency
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)
#>  Pearson's product-moment correlation
#> 
#> data:  proportion and Jane Austen
#> t = 119.64, df = 10404, p-value < 2.2e-16
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.7527837 0.7689611
#> sample estimates:
#>       cor 
#> 0.7609907

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)

#>  Pearson's product-moment correlation
#> 
#> data:  proportion and Jane Austen
#> t = 36.441, df = 6053, p-value < 2.2e-16
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.4032820 0.4446006
#> sample estimates:
#>      cor 
#> 0.424162

# Conclude that Austen & Bronte have more similar word frequencies