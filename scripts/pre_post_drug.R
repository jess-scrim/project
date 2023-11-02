# Filter data for pre and post drug 

lena <- c("lenacenmab", 
          "Leqembi",
          "DB14580",
          "BAN2401",
          "lecanemab-irmb",
          "mAb158",
          "TRAILBLAZER-ALZ")

dona <- c("donanemab",
          "LY3002813",
          "N3pG")

rema <- C("remternetug",
          "LY3372993")

tidy_abstracts_clean %>% 
  filter(word %in% tolower(lena)) %>% View()
  
### See if there is a change in the literature before and after the drug was announced ###

pre_lena <- tidy_abstracts_clean %>% 
  filter(date <= "2023-01-06")  # 3,437 abstracts
post_lena <- tidy_abstracts_clean %>% 
  filter(date > "2023-01-06")  

post_lena %>% distinct(abstract) %>% count()

