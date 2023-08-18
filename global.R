library("shiny")
library("shinyWidgets")
library("elastic")
library("networkD3")
library("dplyr")
library("tidyr")


source("config.R")

conection <- connect(elastic_ip, port="9200", user=elastic_user, pwd = elastic_password)