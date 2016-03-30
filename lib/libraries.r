library(knitr)
library(dplyr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(shiny)
library(methods)

# Uvozimo funkcije za delo z datotekami XML.
source("lib/xml.r", encoding = "UTF-8")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")

