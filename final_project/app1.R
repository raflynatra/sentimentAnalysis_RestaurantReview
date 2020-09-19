library(shiny)
library(shinycssloaders)
library(wordcloud)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(tidytext)
library(DT)

setwd('D:/Document/Informatika/Tugas/Data Science/Project/final_project')
jakarta <- readRDS('data/jakarta.rds')
bandung <- readRDS('data/bandung.rds')
semarang <- readRDS('data/semarang.rds')
yogyakarta <- readRDS('data/yogyakarta.rds')
surabaya <- readRDS('data/surabaya.rds')

source("scraper/scrapReview.R")
source("model-dan-dataset/modelNB.R")

ui <- fluidPage(
  
  titlePanel("Sentiment Analysis of Restaurants from 5 Big Cities in Java Island"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "selectCity",
        label = "Select City",
        choices = c("Jakarta", "Bandung", "Semarang", "Yogyakarta", "Surabaya")
      ),
      uiOutput("selectResto")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About",
          helpText("Tihs app will display the sentiment classification of restaurant's user review from TripAdvisor
                   website. The sentiment will be split into Positive Sentiment and Negative Sentiment.")
          # conditionalPanel(condition = is.na("output$total"), 
          #                  h3(textOutput("Sorry, the restaurant you selected has no reviews"))
          #                  )
        ),
        tabPanel(
          "User Review + Sentiment Classification",
          fluidRow(
            box(
              title = "User Review",
              solidHeader = T,
              width = 12,
              collapsible = T,
              div(DT::dataTableOutput("table_review") %>% withSpinner(color="#1167b1"), style = "font-size: 70%;")
            ),
            box(title = "Sentiment Classification",
                solidHeader = T,
                width = 12,
                collapsible = T,
                plotOutput("plot") %>% withSpinner(color="#1167b1")
            )
          )
        ),
        tabPanel(
          "Wordcloud",
          fluidRow(
            box(title = "Wordcloud",
                solidHeader = T,
                width = 12,
                collapsible = T,
                plotOutput("wordcloud") %>% withSpinner(color="#1167b1")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  var <- reactive({
    switch (input$selectCity,
            "Jakarta" = setNames(jakarta$link, jakarta$name),
            "Bandung" = setNames(bandung$link, bandung$name),
            "Semarang" = setNames(semarang$link, semarang$name),
            "Yogyakarta" = setNames(yogyakarta$link, yogyakarta$name),
            "Surabaya" = setNames(surabaya$link, surabaya$name)
    )
  })
  
  output$selectResto <- renderUI({
    selectInput(inputId  =  "selectResto", 
                label = "Select Restaurant",
                choices = var())
  })
  
  dataScrap <- reactive({
    result <- get_resto_reviews(input$selectResto, incProgress)
    return(result)
  })
  
  data_prediction <- reactive({
    withProgress({
      setProgress(message = "Predicting", value = 0)
      
      reviews <- dataScrap()$review
      incProgress(1/2)
      prediction <- get_prediction(reviews)
      incProgress(1/2)
    })
    prediction$reviewer <- dataScrap()$reviewer
    
    return(prediction)
  })
  
  output$total_review <- renderText({
    paste0("This restaurant has ", nrow(dataScrap()), " review")
  })
  
  output$table_review <- renderDataTable(datatable({
    data_prediction()
  }))
  
  output$wordcloud <- renderPlot({
    data_corpus <- clean_data(dataScrap()$review)
    wordcloud(data_corpus, min.freq = 30, max.words = 50)
  })

  output$plot <- renderPlot({
    Classification <- data_prediction()$sentiment
    Total <- nrow(data_prediction())
    ggplot(data_prediction(), aes(x = Classification, fill = Classification)) + geom_histogram()
  })
  
}

shinyApp(ui, server)
