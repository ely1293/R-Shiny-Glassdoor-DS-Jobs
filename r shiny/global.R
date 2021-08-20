library(shiny)
library(dplyr)
library(maps)
library(googleVis)
library(leaflet)
library(DT)
library(tidyr)
library(ggplot2)
library(shinydashboard)
library(spData)
library(DT)
library(readr)

df <- read_csv("glassdoor_ds.csv", 
                            col_types = cols(SalaryLow = col_number(), 
                                             SalaryHigh = col_number(), SalaryAvg = col_number()))
