library(tidyverse)
library(dplyr)
library(lubridate)
library(tibble)
library(stringr)
library(stringi)
library(tidytext)
# library(ggplot2)
library(superml)
library(reshape2)
library(caret)
library(recommenderlab)

library(shiny)
library(shinythemes)
library(DT)

m4 <- readRDS('m4.Rds')
movie1 <- readRDS('movie1.Rds')
# movie_rating <- readRDS('movie_rating.Rds')
link <- read.csv('links_small.csv')
rating <- read.csv('ratings_small.csv')
id_rating_temp <- merge(link, rating, by='movieId')
movie_rating <- merge(movie1, id_rating_temp, by='movieId')

tfv <- readRDS('tfv.Rds')
# sim_df <- readRDS('sim_df.Rds')
tf_mat <- readRDS('tf_mat.Rds')
cos_sim <- readRDS('cos_sim.Rds')
cos_sim_mat <- cos_sim(tf_mat, tf_mat)
sim_df <- as.data.frame(cos_sim_mat)


cf_mat <- readRDS('cf_mat.Rds')

genres <- readRDS('genres.Rds')
titles <- readRDS('titles.Rds')


ubcf_model <- readRDS('ubcf_model.Rds')
simple_recommender <- readRDS('simple_recommender.Rds')
content_recommender <- readRDS('content_recommender.Rds')
UBCF_Recommender <- readRDS('UBCF_Recommender.Rds')
hybrid_recommender <- readRDS('hybrid_recommender.Rds')


shinyServer(function(input, output){
  
  
  simple_recommends <- reactive({
    datatable(simple_recommender(data=m4, 
                                 genre = input$genre, rec_num = input$rec_num1, 
                                 by_vote_avg = input$by_vote_avg, by_popularity = input$by_popularity,
                                 year_after = input$year_after))
  })
  content_recommends <- reactive({
    datatable(content_recommender(title = input$title2, rec_num = input$rec_num2, description = input$description2))
  })
  UBCF_recommends <- reactive({
    datatable(UBCF_Recommender(userId = input$userId3, rec_num = input$rec_num3, description = input$description3))
  })
  hybrid_recommends <- reactive({
    datatable(hybrid_recommender(userId = input$userId4, title = input$title4, rec_num = input$rec_num4, description = input$description4))
  })
  
  
  output$simple_recommends <- renderDataTable({
    if (input$submit1>0) {isolate(simple_recommends())}
  })
  output$content_recommends <- renderDataTable({
    if (input$submit2>0) {isolate(content_recommends())}
  })
  output$UBCF_recommends <- renderDataTable({
    if (input$submit3>0) {isolate(UBCF_recommends())}
  })
  output$hybrid_recommends <- renderDataTable({
    if (input$submit4>0) {isolate(hybrid_recommends())}
  })
  
  
}
)
