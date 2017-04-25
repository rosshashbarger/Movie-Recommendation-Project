library(plyr)
library(dplyr)
library(magrittr)
library(readr)



movies = read.csv("imdb_data.csv", stringsAsFactors = F)



### Data cleaning
# Change genres, actors, and keywords to be lists instead of string
# Normalize the numeric data points
# Remove error character in movie title
movies$genres = lapply(movies$genres, function(x) unlist(strsplit(x, "[|]")))
movies$plot_keywords = lapply(movies$plot_keywords, function(x) unlist(strsplit(x, "[|]")))
movies$actors = with(movies, paste0(actor_1_name, ",", actor_2_name, ",", actor_3_name))
movies$actors = lapply(movies$actors, function(x) unlist(strsplit(x, "[,]")))
movies$movie_title = lapply(movies$movie_title, function(x) substr(x, 1, nchar(x)-2))
movies$duration = scale(movies$duration)
movies$gross = scale(movies$gross)
movies$budget = scale(movies$budget)
movies$imdb_score = scale(movies$imdb_score)



### Columns to use for similarity:  director_name, genres, actors, duration, gross, language, country, 
### content_rating, budget, imdb_score
columns.compare = c("movie_title", "director_name", "genres", "actors", "duration", "gross", "language", "country", 
                    "content_rating", "budget", "imdb_score")



### Movie for reference - will find recommendations for this movie
#movie.title = "Avatar"
#movie.title = "Tangled"
#movie.title = "Rent"
#movie.title = "Silver Linings Playbook"
#movie.title = "The Purge"
movie.title = "The Big Short"
movie.ref = movies[movies$movie_title==movie.title, columns.compare]


### Calculate the similarity of every movie to the reference movie
movies.compare = movies[which(movies$movie_title!=movie.title), columns.compare]
n=nrow(movies.compare)
movies.recc = data.frame(title = "", similarity = rep(0, n))
movies.recc$title = movies.compare$movie_title
for(i in 1:n){
  # Check if directors are the same
  sim.direct = sum(movie.ref$director_name==movies.compare[i, "director_name"])
  
  # Check how many of reference movie genres are in common and scale via number of genres
  # reference movie is listed under
  genres.ref = unlist(movie.ref$genres)
  sim.genre = 0
  for(j in genres.ref){
    sim.genre = sim.genre + sum(j %in% movies.compare[i, "genres"][[1]])
  }
  sim.genre = sim.genre/length(genres.ref)
  
  # Check how many of reference movie actors are in common and scale
  actors.ref = unlist(movie.ref$actors)
  sim.act = 0
  for(j in actors.ref){
    sim.act = sim.act + sum(j %in% movies.compare[i, "actors"][[1]])
  }
  sim.act = sim.act/length(actors.ref)
  
  # Check if language is the same
  sim.lang = sum(movie.ref$language==movies.compare[i, "language"])
  
  # Check if country is the same
  sim.country = sum(movie.ref$country==movies.compare[i, "country"])
  
  # Check if content-rating is the same
  sim.rating = sum(movie.ref$content_rating==movies.compare[i, "content_rating"])
  
  # Similarity of all numeric attributes using euclidean distance and scale similarity based on
  # distance of two movies being 6 standard deviations from each other in each attribute
  sim.num = sqrt((movie.ref$duration-movies.compare[i, "duration"])^2 + 
                   (movie.ref$gross-movies.compare[i, "gross"])^2 +
                   (movie.ref$budget-movies.compare[i, "budget"])^2 + 
                   (movie.ref$imdb_score-movies.compare[i, "imdb_score"])^2)
  sim.num = 1 - sim.num[1]/12
  
  # Calculate overall similarity based on the following criteria:
  # Director:  10%
  # Genres:  25%
  # Actors:  25%
  # Language:  5%
  # Country:  5%
  # Content-rating:  10%
  # Euclidean distance from duration, gross, budget, and IMDB score:  20%
  # 
  # One a scale of (-1, 1) with 1 being most similar
  tot.sim = .1*sim.direct + .25*sim.genre + .25*sim.act + .05*sim.lang + 
    .05*sim.country + .1*sim.rating + .2*sim.num
  
  movies.recc[i, "similarity"] = tot.sim
}

movies.recc = arrange(movies.recc, -similarity)
movies.recc[1:5,]
