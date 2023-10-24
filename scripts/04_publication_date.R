# Visualise when abstracts were published

abstracts %>%  
  ggplot(aes(date)) +
  geom_histogram(bins = 100) +
  xlab("Date of Publication") +
  ylab("Number of Abstracts") 
  
