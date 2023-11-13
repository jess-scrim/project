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
query <- '("alzheimer disease"[MeSH Terms] OR ("alzheimer"[All Fields] AND "disease"[All Fields]) OR "alzheimer disease"[All Fields] OR ("alzheimer s"[All Fields] AND "disease"[All Fields]) OR "alzheimer s disease"[All Fields]) AND ((fha[Filter]) AND (booksdocs[Filter] OR casereports[Filter] OR clinicalstudy[Filter] OR clinicaltrial[Filter] OR controlledclinicaltrial[Filter] OR meta-analysis[Filter] OR randomizedcontrolledtrial[Filter] OR review[Filter] OR systematicreview[Filter]) AND (2022/1/2:2023/10/10[pdat]) AND (english[Filter]))'
type <- 'esearch'
db <- 'pubmed'
mindate <- '2022/01/01'
maxdate <- '2023/10/10'
retmax <- 100
datetype <- 'edat' # date of publication
