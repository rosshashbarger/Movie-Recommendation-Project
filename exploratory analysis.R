movies = read.csv("imdb_data.csv", stringsAsFactors = F)

movies$genres = lapply(movies$genres, function(x) strsplit(x, "[|]"))
