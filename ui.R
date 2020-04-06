

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(highcharter)
library(leaflet)
library(flexdashboard)
library(shinyWidgets)
library(readxl)
library(httr)
library(tidyr)

jscode <- "shinyjs.refresh = function() { history.go(0); }"






navbarPage(theme = shinytheme("cosmo"), "Coronavirus", navbarMenu("Menu", icon = icon("search"), 
                                                                  tabPanel("MAP", icon = icon("globe-asia"), 
                                                                           sidebarPanel(verticalLayout(
                                                                             p(h5(tags$b('How to use this dashboard:'))),
                                                                             p(h6('Start choosing the KPI you want to look at. You can also filter the minimum number of cases per country in order to avoid any bias. Finally, click on apply to display the map.'),
                                                                             h6('Comparison between countries is available in the Countries tabPanel in the Menu.')), 
                                                                             radioGroupButtons(inputId = "kpi", label = "Choose your KPI:", choices = c("Death rate", "Number of deaths", "Number of cases"), status = "primary", checkIcon = list(yes = icon("thumbs-up", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))), 
                                                                             tags$style(".irs-line {background: #6610f2} .irs-bar {background: #007bff}"), 
                                                                             sliderInput("variation_count", "Choose the minimum number of cases (in numbers):", min = 100, max = 5000 , value = 200), 
                                                                             splitLayout(actionButton("apply", "Apply", icon = icon("play"), class = "btn btn-primary")))), 
                                                                           highchartOutput("my_map", height = 1000)),
                                                                  tabPanel("Countries", icon = icon("chart-bar"), 
                                                                           fluidRow(
                                                                             sidebarPanel(verticalLayout(
                                                                               p(h5(tags$b('How to use this dashboard:'))),
                                                                               p(h6('Just choose two countries to be compared and click on apply to launch the comparison.')),
                                                                               selectizeInput("countries", label = h5(tags$b("Countries:")),
                                                                                              choices = NULL,
                                                                                              selected = NULL,
                                                                                              multiple = TRUE,
                                                                                              options = list(maxItems = 2, placeholder = 'Select countries ...')),
                                                                               h6('Note: Only countries with at least 100 cases and 10 deaths are available.'),
                                                                               br(),
                                                                               splitLayout(actionButton("apply2", "Apply", icon = icon("play"), class = "btn btn-primary"))))),
                                                                           br(),
                                                                           fluidRow(
                                                                             column(6,
                                                                                    highchartOutput("graph_cases_after_100")),
                                                                             column(6, highchartOutput("graph_deaths_after_10"))),
                                                                           br(),
                                                                           fluidRow(column(6, highchartOutput('graph_daily_after_100')),
                                                                                    column(6, highchartOutput('graph_daily_after_10'))))))