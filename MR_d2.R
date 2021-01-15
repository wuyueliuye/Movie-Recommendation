#### Data Prepare ####

## Libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(tibble)
library(stringr)
library(stringi)
library(tidytext)
library(ggplot2)
library(superml)
library(reshape2)
library(caret)
library(recommenderlab)
#library(text2vec)

movie0 <- read.csv('movies_metadata.csv')
head(movie0)
m1 <-na.omit(movie0)
summary(m1$vote_count)
summary(m1$vote_average)
m1$release_year <- year(as.Date(m1$release_date))
summary(m1$release_year)
m2 <- m1[complete.cases(m1[,c('id', 'title', 'release_year', 'genres', 'vote_count','vote_average','popularity')]),]
# m2 <- m2[m2$genres != '[]',]
head(m2$genres)
m2$genres1 <- stringi::stri_extract_all(m2$genres, regex = "[A-Z][a-z]+")
#m2$companies1 <- stringi::stri_extract_all(m2$production_companies, regex = "[A-Z][a-z]*[ [A-Z][a-z]*]*")
#m3 <- m2[!is.na(m2$genres1) & !is.na(m2$companies1),]
m3 <- m2[!is.na(m2$genres1),]
genres <- levels(as.factor(unlist(m3$genres1)))
# companies <- levels(as.factor(unlist(m3$companies1)))


genre_df <- tibble::enframe(m3$genres1)%>%
  unnest(value)%>%
  mutate(temp=1)%>%
  pivot_wider(names_from=value, values_from=temp, values_fill=list(temp=0))

# company_df <- tibble::enframe(m3$companies1)%>%
#   unnest(value)%>%
#   mutate(temp=1)%>%
#   pivot_wider(names_from=value, values_from=temp, values_fill=list(temp=0))
# 
# movie1 <- cbind(m3, genre_df, company_df)
m4 <- cbind(m3,genre_df)
saveRDS(m4,'m4.Rds')
movie1 <- cbind(m3, genre_df)

# simple_recommend <- function(data, genre, rec_num, by_vote_avg=F, by_popularity=T, year_after=NULL){
#   if (is.null(company)) {
#     if (is.null(year_after)) {generic_df <- data[data[,genre]==1 & data$vote_count>=50, ]}
#     else {generic_df <- data[data[,genre]==1 & data$vote_count>=50 & data$release_year >= year_after, ]}}
# 
#   else {if (is.null(year_after)) {generic_df <- data[data[,genre]==1 & data$company==1 & data$vote_count>=50, ]}
#     else {generic_df <- data[data[,genre]==1 & data[,company]==1, data$vote_count>=50 & data$release_year >= year_after, ]}}
# 
#   if (by_vote_avg==T) {generic_df <-  generic_df[order(generic_df$vote_average,as.numeric(generic_df$popularity) , decreasing = T),]}
#   else  {generic_df <-  generic_df[order(as.numeric(generic_df$popularity), decreasing = T),]}
#   rec <- generic_df[1:rec_num, c('title', 'release_year', 'genres1','vote_average', 'popularity', 'companies1')]
#   print(rec)
# }
# simple_recommend(movie1, 'Action', 'Walt Disney Pictures',10, T, T, 2010)

#### Simple Recommender ####

simple_recommender <- function(data=m4, genre, rec_num, by_vote_avg=F, by_popularity=T, year_after=NULL, ...){
  
  if (is.null(year_after)) {generic_df <- data[data[,genre]==1 & data$vote_count>=50, ]}
  else {generic_df <- data[data[,genre]==1 & data$vote_count>=50 & data$release_year >= year_after, ]}
  
  
  if (by_vote_avg==T) {generic_df <-  generic_df[order(generic_df$vote_average,as.numeric(generic_df$popularity) , decreasing = T),]}
  else  {generic_df <-  generic_df[order(as.numeric(generic_df$popularity, generic_df$vote_average), decreasing = T),]}
  rec <- generic_df[1:rec_num, c('title', 'release_year', 'genres1','vote_average', 'popularity')]
  row.names(rec) <- NULL
  return(rec)
}

simple_recommender(genre = 'Action', rec_num = 10, year_after = 2015)

# simple_recommend2 <- function(data, genre, rec_num, by_vote_avg=F, by_popularity=T, year_after=NULL, company=NULL){
#   if (is.null(company)) {
#     if (is.null(year_after)) {generic_df <- data[data[,genre]==1 & data$vote_count>=50, ]}
#     else {generic_df <- data[data[,genre]==1 & data$vote_count>=50 & data$release_year >= year_after, ]}}
#   
#   else {if (is.null(year_after)) {generic_df <- data[data[,genre]==1 & data$company==1 & data$vote_count>=50, ]}
#     else {generic_df <- data[data[,genre]==1 & data[,company]==1, data$vote_count>=50 & data$release_year >= year_after, ]}}
#   
#   if (by_vote_avg==T) {generic_df <-  generic_df[order(generic_df$vote_average,as.numeric(generic_df$popularity) , decreasing = T),]}
#   else  {generic_df <-  generic_df[order(as.numeric(generic_df$popularity), decreasing = T),]}
#   rec <- generic_df[1:rec_num, c('title', 'release_year', 'genres1','vote_average', 'popularity', 'companies1')]
#   print(rec)
# }

#### Content-Based ####
# link0 <- read.csv('links_small.csv')
# credit0 <- read.csv('credits.csv')
# keyword0 <- read.csv('keywords.csv')
# 
# keyword0$key <- stringi::stri_extract_all(keyword0$keywords, regex = '[a-z]+[ \\-a-z]*')
# for (i in 1:nrow(keyword0)) {
#   keyword0$key[[i]] <- keyword0$key[[i]][!keyword0$key %in% c('id', 'name')]
# }
# 
# keyword
m_temp <- m3[,c('id', 'title', 'release_year','genres1', 'overview', 'tagline', 'vote_count', 'vote_average', 'popularity')]
link <- read.csv('links_small.csv')
movie1 <- merge(m_temp, link, by.x = c('id'), by.y=c('tmdbId'))
movie1$description <- paste(movie1$overview, movie1$tagline)
saveRDS(movie1, 'movie1.Rds')
m_temp <- movie1[,c('title','description')]

temp <- m_temp%>%tidytext::unnest_tokens(word, description)%>%anti_join(stop_words)
freq_temp <- temp%>%count(word, sort = T)
wordcloud2::wordcloud2(freq_temp)


tfv <- superml::TfIdfVectorizer$new(max_features = 1000, remove_stopwords = T)
tf_mat <- tfv$fit_transform(m_temp$description)

cos_sim <- function(m1,m2, ...){
  mat <- tcrossprod(m1,m2)
  t1 <- sqrt(apply(m1,1,crossprod))
  t2 <- sqrt(apply(m2, 1, crossprod))
  mat/outer(t1,t2)
}

cos_sim_mat <- cos_sim(tf_mat, tf_mat)

titles <- as.factor(movie1$title)
sim_df <- as.data.frame(cos_sim_mat)

content_recommender <- function(title, rec_num=10, description=F, ...){
  
  if (description==F) {description <- NULL}
  else description='descripton'
  
  sim_scores <- as.vector(t(sim_df[which(titles==title),]))
  sim_by_idx <- order(sim_scores,decreasing = T)
  rec_idx <- sim_by_idx[2:(rec_num+1)]
  rec_movies <- movie1[rec_idx, c('title', 'release_year', 'genres1',description)]
  row.names(rec_movies) <- NULL
  return(rec_movies)
}

content_recommender('Toy Story',5)


#### Collaborative-Filtering (User-Based CF) ####
rating <- read.csv('ratings_small.csv')
id_rating_temp <- merge(rating, link, by='movieId')
movie_rating <- merge(id_rating_temp, movie1, by='movieId')

cf_df <- reshape2::dcast(rating, userId~movieId, value.var='rating')
cf_mat <- as.matrix(cf_df[,-1])
# dim(ucf_mat)
cf_mat <- as(cf_mat, "realRatingMatrix")
# cf_mat@data@Dimnames[[1]] <- unique(rating$userId) mute the userIds from the rating matrix if needed

userIds <- unique(rating$userId)
ubcf_model <- recommenderlab::Recommender(cf_mat, method='UBCF', param='cosine')
UBCF_Recommender <- function(userId, rec_num=10, description=F, ...){
  
  if (description==F) {description <- NULL}
  else description='description'
  
  rec <- predict(ubcf_model, cf_mat[userId], n=rec_num)
  rec_list <- as(rec, 'list')
  rec_movieId <- rec_list[[1]]
  rec_movies <- movie_rating[movie_rating$movieId %in% rec_movieId, c('title','release_year', 'genres1', description)]
  rec_movies <- unique(rec_movies)
  # rec_movies <- rec_movies[,c('title','release_year', 'genres1', description)]
  row.names(rec_movies) <- NULL
  # rec_movies <- as.data.frame(rec_movies)
  return(rec_movies)
}

UBCF_Recommender(11,5,T)

#### Hybrid Recommender (ContentBased + CollaborativeFiltering)####

hybrid_recommender <- function(userId, title, rec_num, description=F, ...){
  
  if (description==F) {description <- NULL}
  else description='description'
  
  rec_step1 <- UBCF_Recommender(userId, 50, T)
  rec_step1.2 <- rbind(movie1[which(movie1$title==title), 
                              c('title', 'release_year', 'genres1', 'description')], rec_step1)
  
  
  tf_mat_temp <- tfv$fit_transform(rec_step1.2$description)
  cos_sim_mat_temp <- cos_sim(tf_mat_temp, tf_mat_temp)
  sim_df_temp <- as.data.frame(cos_sim_mat_temp)
  
  
  sim_scores <- as.vector(t(sim_df_temp[1,]))
  sim_by_idx <- order(sim_scores, decreasing = T)
  rec_idx <- sim_by_idx[2:(rec_num+1)]
  rec_step2 <- movie1[rec_idx, c('title', 'release_year', 'genres1', description)]
  row.names(rec_step2) <- NULL
  return(rec_step2)
  
}
hybrid_recommender(55, titles[100], 5, T)


# hybrid_recommender_draft <- function(userId, title, rec_num, description=F){
#   if (description==F) {description <- NULL}
#   else description='descripton'
#   
#   sim_scores <- as.vector(t(sim_df[which(titles==title),]))
#   sim_by_idx <- order(sim_scores, decreasing = T)
#   rec_idx <- sim_by_idx[2:51]
#   rec_step1 <- movie1[rec_idx, c('movieId', 'title', 'release_year', 'genres1','description')]
#   
#   rec_step1.2 <- merge(rec_step1, id_rating_temp, by='movieId')
#   
#   cf_df <- reshape2::dcast(rec_step1.2, userId~movieId, value.var='rating')
#   cf_mat <- as.matrix(cf_df[,-1])
#   cf_mat <- as(cf_mat, "realRatingMatrix")
#   # cf_mat_norm <- normalize(cf_mat)
#   ubcf_model <- recommenderlab::Recommender(cf_mat, method='UBCF', param='cosine')
#   
#   rec_step2 <- predict(ubcf_model, cf_mat[userId], n=rec_num)
#   
#   rec_list <- as(rec_step2, 'list')
#   rec_movieId <- rec_list[[1]]
#   rec_movies <- rec_step1.2[rec_step1.2$movieId %in% rec_movieId, c('title','release_year', 'genres1', description)]
#   rec_movies <- unique(rec_movies)
#   row.names(rec_movies) <- NULL
#   return(rec_movies)
#   
# }
# hybrid_recommender_draft(55, titles[100], 5)
rsconnect::deployApp()
