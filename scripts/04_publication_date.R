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
