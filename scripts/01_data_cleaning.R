source("scripts/00_setting_up.R")
# load data, n = 28,601
res_100 <- EUtilsSummary(query,
                     type = type, 
                     db = db, 
                     datetype = datetype,
                     mindate = mindate,
                     maxdate = maxdate,
                     retmax = retmax)

title_100 <- ArticleTitle(EUtilsGet(res_100))
abstracts_100 <- AbstractText(EUtilsGet(res_100))
authors_100 <- Author(EUtilsGet(res_100))
year_100 <- YearPubmed(EUtilsGet(res_100))
month_100 <- MonthPubmed(EUtilsGet(res_100))
day_100 <- DayPubmed(EUtilsGet(res_100))

# convert to data frame
abstracts <- tibble(abstract = 1:100, 
                       title = title,
                       text = abstracts, 
                       date = dmy(paste0(day, "/", month, "/", year)),
                       author = authors
                       )
