library(shiny)
library(shinythemes)


genres <- readRDS('.\genres.Rds')
titles <- readRDS('.\titles.Rds')


shinyUI(fluidPage(theme = shinytheme('flatly'),
          navbarPage(
            'Movie Recommender',
            tabPanel('Simple Recommender',
                     sidebarPanel(
                       selectInput('genre', 'movie genre', choices = genres),
                       numericInput('rec_num1', 'number of recommendations', value = 5, min = 1, max=20),
                       selectInput('by_vote_avg', 'by vote average', choices = c(T, F), selected = F),
                       selectInput('by_popularity', 'by popularity', choices = c(T,F), selected = T),
                       numericInput('year_after', 'year after', value = NULL, max = 2020),
                       actionButton('submit1', 'submit', class='btn btn-primary')
                     ),
                     mainPanel(
                       dataTableOutput('simple_recommends')
                       
                     )
            ), # tab1 -- simple recommender
            tabPanel('Content Recommender',
                     sidebarPanel(
                       selectInput('title2', 'movie', choices = titles),
                       numericInput('rec_num2', 'number of recommendations', value = 10, min = 1, max = 20, step = 1),
                       selectInput('description2', 'show descriptions', choices = c(T,F), selected = F),
                       actionButton('submit2', 'submit', class='btn btn-primary')
                     ),
                     mainPanel(
                       dataTableOutput('content_recommends')
                     )), # tab2 -- content recommender
            tabPanel('CollaborativeFiltering(User-Based)',
                     sidebarPanel(
                       numericInput('userId3', 'userId', value = 1, min = 1, max = 671, step = 1),
                       numericInput('rec_num3', 'number of recommendations', value = 10, min = 1, max = 20, step = 1),
                       selectInput('description3', 'show descriptions', choices = c(T,F), selected = F),
                       actionButton('submit3', 'submit', class='btn btn-primary')
                       
                     ),
                     mainPanel(
                       dataTableOutput('UBCF_recommends')
                     )), # tab3 -- UBCF recommender
            tabPanel('Hybrid Recommender',
                     sidebarPanel(
                       numericInput('userId4', 'userId', value = 1, min = 1, max = 671, step = 1),
                       selectInput('title4', 'movie', choices = titles),
                       numericInput('rec_num4', 'number of recommendations', value = 10, min = 1, max = 20, step = 1),
                       selectInput('description4', 'show descriptions', choices = c(T,F), selected = F),
                       actionButton('submit4', 'submit', class='btn btn-primary')
                     ),
                     mainPanel(
                       dataTableOutput('hybrid_recommends')
                     )) # tab4 -- hybrid recommender
          ) # navbar 
) # fluidpage
)
