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