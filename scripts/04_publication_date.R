# Visualise when abstracts were published

abstracts %>%  
  ggplot(aes(date)) +
  geom_histogram(bins = 50) +
  xlab("Date of Publication") +
  ylab("Number of Abstracts") +
  # 10 month intervals
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
scale_y_continuous(breaks = seq(0, 1400, by = 200)) 


## Visualise abstracts published with all lena terms
naive_drug_results %>% 
  filter(!is.na(date_published)) %>% 
  ggplot(aes(as.Date(date_published, format = "%Y %b %d"))) +
  geom_histogram(bins = 30,
                 binwidth = 20) +
  xlab("Date of Publication") +
  ylab("Number of Abstracts") +
  # 10 month intervals
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


## Visualise on top
naive_drug_results$date_published <- as.Date(naive_drug_results$date_published, 
                                             format = "%Y %b %d")

ggplot() +
  geom_histogram(aes(x = abstracts$date),
                 bins = 50,
                 binwidth = 20,
                 fill = "blue") +
  geom_histogram(aes(x = naive_drug_results$date_published),
                 bins = 30,
                 binwidth = 20,
                 fill = "red") +
  xlab("Date of Publication") +
  ylab("Number of Abstracts") +
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%Y") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
