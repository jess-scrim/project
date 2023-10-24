source("scripts/00_setting_up.R")
# load data, n = 28,601
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

# convert to data frame
abstracts <- tibble(abstract = 1:6199, 
                       title = title,
                       text = abstracts, 
                       date = dmy(paste0(day, "/", month, "/", year)),
                       author = authors
                       )
