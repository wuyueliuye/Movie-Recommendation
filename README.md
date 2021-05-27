#### Movie Recommendation

data sources: https://www.kaggle.com/rounakbanik/the-movies-dataset

4 types of recommenders

- Popularity and rating based recommender
Movies be ordered and recommended by number of ratings or the average level of ratings 

- Content based recommender
TFIDF to compute the similarity between movie descriptions

- User based collaborative filtering recommender
Recommend with items that similar users interested
![](https://upload.wikimedia.org/wikipedia/commons/5/52/Collaborative_filtering.gif)

- hybrid recommender from content and collaborative filtering
Analyze similarity between movies based on their description, also recommend items that similar users liked under the loop of similar items
