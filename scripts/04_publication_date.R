# Visualise when abstracts were published

abstracts %>%  
  ggplot(aes(date)) +
  geom_histogram(bins = 100) +
  xlab("Date of Publication") +
  ylab("Number of Abstracts") +
  # 10 month intervals
  scale_x_date(date_breaks = "4 month", date_labels = "%m/%Y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
scale_y_continuous(breaks = seq(0, 1400, by = 200)) 

## Visualise abstracts published with all lena terms
naive_drug_results %>% 
  filter(!is.na(date_published)) %>% 
  ggplot(aes(as.Date(date_published, format = "%Y %b %d"))) +
  geom_histogram(bins = 30,
                 binwidth = 100) +
  xlab("Date of Publication") +
  ylab("Number of Abstracts") +
  # 10 month intervals
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
