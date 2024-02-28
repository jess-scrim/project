source("scripts/00_setting_up.R")
# load data for abstracts
res <- EUtilsSummary(query,
                     type = type, 
                     db = db, 
                     datetype = datetype,
                     mindate = mindate,
                     maxdate = maxdate,
                     retmax = retmax)

title <- ArticleTitle(EUtilsGet(res))
abstracts <- AbstractText(EUtilsGet(res))
authors <- Author(EUtilsGet(res))
year <- YearPubmed(EUtilsGet(res))
month <- MonthPubmed(EUtilsGet(res))
day <- DayPubmed(EUtilsGet(res))
keyword <- Keyword(EUtilsGet(res))

# convert to data frame
abstract <- tibble(abstract = 1:7055, 
                       title = title,
                       text = abstracts, 
                       date = dmy(paste0(day, "/", month, "/", year))
                       )

# remove abstracts which are NA
# abtsrcats must have 'alzheimer'disease', 'alzheimer' or 'ad' in text and/or title

tidy_abstracts <- abstract %>% 
  filter(grepl("alzheimer\'s disease", tolower(text)) | 
           grepl("\\b(ad)\\b", tolower(text)) | 
           grepl("/(ad)/", tolower(text)) | 
           grepl("alzheimer\'s disease", tolower(title)) | 
           grepl("\\b(ad)\\b", tolower(title)) | 
           grepl("/(ad)/", tolower(title)) |
           grepl("alzheimer", tolower(text)) |
           grepl("alzheimer", tolower(title))) %>% 
  arrange(desc(date)) %>%  # Arrange by date in descending order
  distinct(title, .keep_all = TRUE) %>%
  arrange(desc(date)) %>%  # Arrange again to restore the original order if needed
  distinct(text, .keep_all = TRUE)


the_rest <- abstract %>% 
  anti_join(tidy_abstracts, by = "text")

  
# getting pre-print data
mx_data <- mx_api_content(from_date = "2022-01-01", to_date = "2024-01-01")

# Preprints published & unpublished - 577

preprints <- mx_data %>% 
  select(title, date, abstract) %>% 
  filter(!grepl("^withdrawn", tolower(title))) %>% 
  filter((grepl("alzheimer\'s disease", tolower(abstract)) & 
            grepl("\\b(ad)\\b", tolower(abstract)) | 
            grepl("/(ad)/", tolower(abstract))) | 
           (grepl("alzheimer\'s disease", tolower(title)) & 
              grepl("\\b(ad)\\b", tolower(title)) | 
              grepl("/(ad)/", tolower(title))) |
           grepl("alzheimer", tolower(abstract)) |
           grepl("alzheimer", tolower(title))) %>% 
  arrange(desc(date)) %>%  # Arrange by date in descending order
  distinct(title, .keep_all = TRUE) %>%
  arrange(desc(date)) %>%  # Arrange again to restore the original order if needed
  distinct(abstract, .keep_all = TRUE)

test <- preprints %>% 
  rename("text" = abstract) %>% 
  mutate(abstract = 6167:6743,
         source = "preprint")
test <- tidy_abstracts %>% 
  mutate(source = "pubmed") %>%
  rbind(test)
# In total with pubmed 6744 - no duplicates

preprints_unpub <- mx_data %>% 
  filter(is.na(published)) %>% 
  select(title, date, abstract) %>% 
  filter(!grepl("^withdrawn", tolower(title))) %>% 
  filter((grepl("alzheimer\'s disease", tolower(abstract)) & 
            grepl("\\b(ad)\\b", tolower(abstract)) | 
            grepl("/(ad)/", tolower(abstract))) | 
           (grepl("alzheimer\'s disease", tolower(title)) & 
              grepl("\\b(ad)\\b", tolower(title)) | 
              grepl("/(ad)/", tolower(title))) |
           grepl("alzheimer", tolower(abstract)) |
           grepl("alzheimer", tolower(title))) %>% 
  arrange(desc(date)) %>%  # Arrange by date in descending order
  distinct(title, .keep_all = TRUE) %>%
  arrange(desc(date)) %>%  # Arrange again to restore the original order if needed
  distinct(abstract, .keep_all = TRUE)
# Unpublished from 22-11-23 to 29-12-23