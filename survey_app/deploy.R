library("rsconnect")
library(shiny)

rsconnect::setAccountInfo(name='krsafonov',
                          token='xxxxxxxx',
                          secret='xxxxxxxx')

# setwd("~/Documents/ma/conjoint/survey_app/")

runApp()

deployApp()

