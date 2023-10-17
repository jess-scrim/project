# get pdf
source("scripts/00_setting_up.R")

pdf <- "data/sample_pdf.pdf"
txt <- pdf_text(pdf)
toc <- pdf_toc(pdf)
cat(txt[1]) # print first page
