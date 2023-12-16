source("scripts/00_setting_up.R")
# load data
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
abstracts <- tibble(abstract = 1:7000, 
                       title = title,
                       text = abstracts, 
                       date = dmy(paste0(day, "/", month, "/", year)),
                       author = authors
                       )

res <- EUtilsSummary(leca_query,
                     type = type, 
                     db = db, 
                     datetype = datetype,
                     mindate = mindate,
                     maxdate = maxdate,
                     retmax = retmax)

leca_abstracts <- tibble(abstract = 1:7000,
                         title = title,
                         text = abstracts, 
                         date = dmy(paste0(day, "/", month, "/", year)),
                         author = authors
                         )