# Load libraries
library(tidyverse)
library(tidytext)
library(RISmed)
library(igraph)
library(ggraph)
library(topicmodels)
library(remotes)
library(litsearchr)
library(lubridate)
library(medrxivr)

# Define Search terms
query <- '("alzheimer disease"[MeSH Terms] OR ("alzheimer"[All Fields] AND "disease"[All Fields]) OR "alzheimer disease"[All Fields] OR ("alzheimer s"[All Fields] AND "disease"[All Fields]) OR "alzheimer s disease"[All Fields]) AND ((fha[Filter]) AND (booksdocs[Filter] OR casereports[Filter] OR clinicalstudy[Filter] OR clinicaltrial[Filter] OR controlledclinicaltrial[Filter] OR meta-analysis[Filter] OR randomizedcontrolledtrial[Filter] OR review[Filter] OR systematicreview[Filter]) AND (2022/1/2:2024/01/01[pdat]) AND (english[Filter]))'
leca_query <- '("lecanemab"[All Fields] OR "mab2401"[All Fields] OR "leqembi"[All Fields] OR "ban2401"[All Fields])'
type <- 'esearch'
db <- 'pubmed'
mindate <- '2022/01/01'
maxdate <- '2024/01/01'
retmax <- 8000
datetype <- 'edat' # date of publication
leca_approv <- '2023-01-06'
my_stopwords <- tibble(word = c("alzheimer's", "ad", "95", "ci", "including"))
