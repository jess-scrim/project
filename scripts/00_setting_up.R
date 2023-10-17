# load libraries
library(tidyverse)
library(tidytext)
library(readr)
library(stringr)
library(pdftools)
library(tm)
library(RISmed)
library(ggplot2)
library(igraph)
library(ggraph)
library(topicmodels)
library(remotes)
library(litsearchr)

# Search terms
query <- '(alzheimer) OR (alzheimer\'s disease))'
type <- 'esearch'
db <- 'pubmed'
mindate <- '2022/01/01'
maxdate <- '2023/10/10'
retmax <- 10000