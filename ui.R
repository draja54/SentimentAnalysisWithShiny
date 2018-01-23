#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(DT)
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("cerulean"),
    
    navbarPage("Customer Feedback Analysis",
               tabPanel("Sentiment Analysis",
                        
                            fluidRow(
                                column(8,
                                fileInput("finput",label = h3("Upload a csv file"))),
                                
                                column(4,
                                dateRangeInput("dates", label = h3("Date range"))
                                )),
                            
                            # Show a plot of the generated distribution
                           
                                tabsetPanel(type = "tab", 
                                            tabPanel("Emotions Word Cloud", 
                                                     br(),
                                                     plotOutput("ewordcloud")
                                                     ),
                                            tabPanel("Polarity Word Cloud", 
                                                     br(),
                                                     plotOutput("pwordcloud")),
                                            tabPanel("Sentiment by Emotion",
                                                     br(),
                                                     plotOutput("eplot")),
                                            tabPanel("Sentiment by Polarity", 
                                                     br(),
                                                     plotOutput("pplot")),
                                            tabPanel("Data", 
                                                     br(),
                                                     DT::dataTableOutput("fData")),
                                            tabPanel("Emotion Data", 
                                                     br(),
                                                     DT::dataTableOutput("eData")),
                                            tabPanel("Polarity Data",
                                                     br(),
                                                     DT::dataTableOutput("pData"))
                                )
                            ),
               tabPanel("Sample Template",
                        DT::dataTableOutput("sampleTable"),
                        downloadButton("downloadCsv", "Download CSV", class = "button"),
                        tags$head(tags$style(".button{background-color: #1E90FF;}
                                                .button{color: white;}
                                                .button{background-image: none;}
                                             "))
                        ),
               navbarMenu("More",
                          tabPanel("About")))
    )
)