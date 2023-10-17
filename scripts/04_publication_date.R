# Viualise when abstracts were published

abstracts %>% 
  ggplot(aes(date)) +
  geom_bar() +
  xlab("Date of Publication") +
  ylab("Number of Abstracts") 
