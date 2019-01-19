library("jsonlite")
library("curl")
library("igraph")
library("stringi")
## library("ggplot2")
library("parallel")

corpora <- "rus"
## plays_to_sremove <- list("blok-balaganchik", "blok-korol-na-ploschadi", "blok-neznakomka", "gogol-teatralnyi-razezd")

## Downloading plays
list_of_names <- fromJSON(paste0("https://dracor.org/api/corpora/", corpora))
sorted_ids <- list_of_names$dramas$id[sort.list(list_of_names$dramas$id)]
## sorted_ids <- sorted_ids[sorted_ids != plays_to_remove]  ## removing of plays which do not represent social interactions
plays <- mclapply(sorted_ids, function(x) read.csv(paste0("https://dracor.org/api/corpora/", corpora, "/play/", x, "/networkdata/csv"), stringsAsFactors = F))
#p_chars <- mclapply(sorted_ids, function(x) fromJSON(paste0("https://dracor.org/api/corpora/", corpora, "/play/", x), flatten = T))
p_text <- mclapply(sorted_ids, function(x) fromJSON(paste0("https://dracor.org/api/corpora/", corpora, "/play/", x, "/spoken-text-by-character")))


#list_of_df <- function(play){
#  df <- data.frame(play$cast$id)
#  names(df) <- "cast"
#  df <- df[order(df$cast),]
#  ## names(df) <- make.names(play$id)
#  return (df)
#}

#plays_chars <- lapply(p_chars, list_of_df)

metadata <- read.csv(paste0("https://dracor.org/api/corpora/", corpora, "/metadata.csv"), stringsAsFactors = F)
metadata <- metadata[order(metadata$name),]
## metadata <- metadata[metadata$name != plays_to_remove,] ## removing of plays which do not represent social interactions

#names(plays_chars) <- metadata$name

## Remove 'Type' variable
del_vars <- function(play){
  play$Type <- NULL
  names(play) <- tolower(names(play))
  #play$Weight <- NULL
  return (play)
}

plays <- mclapply(plays, del_vars)

## Creating graphs of plays
graphs_of_plays <- mclapply(plays, function(x) graph_from_data_frame(x, directed = F))

net_calc <- function(x){
  V(x)$betweenness <- betweenness(x, v = V(x), directed = F, weights = NA)
  V(x)$closeness <- round(closeness(x, weights = NA, normalized = T), digits = 7)
  V(x)$w_degree <- strength(x)
  V(x)$degree <- degree(x)
  V(x)$eigenvector <- round(eigen_centrality(x, weights = NA)$vector, digits = 7)
  x
}

ranking <- function(x){
  x$b_rank <- rank(-x$betweenness, ties.method = "min")
  x$c_rank <- rank(-x$closeness, ties.method = "min")
  x$wd_rank <- rank(-x$w_degree, ties.method = "min")
  x$d_rank <- rank(-x$degree, ties.method = "min")
  x$e_rank <- rank(-x$eigenvector, ties.method = "min")
  x$network_m <- rank(rowMeans(subset(x, select=c("b_rank", "c_rank", "wd_rank", "d_rank", "e_rank"))), ties.method = "min")
## should it be as.numeric(as.factor())
  x
}

graphs_of_plays <- mclapply(graphs_of_plays, net_calc)
graphs_df <- mclapply(graphs_of_plays, function(x) as_data_frame(x, what="vertices"))
graphs_df <- mclapply(graphs_df, ranking)
names(graphs_df) <- metadata$name

list_of_df <- function(play){
  df <- data.frame(play$id)
  names(df) <- "tmp"
  df$text <- play$text
  df$cast <- play$id
  df <- df[order(df$cast),]
  df$tmp <- NULL
  df$num_words <- lapply(df$text, stri_count_words)
  df$num_words <- lapply(df$num_words, sum)
  return (df)
}

plays_text <- lapply(p_text, list_of_df)
names(plays_text) <- metadata$name

sum(stri_count_words(p_text[[1]][["text"]][[1]]))
chars1 <- p_text[[1]][["id"]]
count_calc1 <- data.frame(chars1)



graph1 <- graphs_of_plays[[1]]
graph1 <- net_calc(graph1)
graph1_df <- as_data_frame(graph1, what="vertices")
graph1_df <- ranking(graph1_df)
