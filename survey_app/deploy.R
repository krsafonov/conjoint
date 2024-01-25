library("rsconnect")
library(shiny)

rsconnect::setAccountInfo(name='krsafonov',
                          token='xxxxxxxx',
                          secret='xxxxxxxx')

setwd("~/survey_app/")
runApp()

deployApp()

