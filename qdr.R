library("jsonlite")
library("curl")
library("igraph")
library("stringi")
## library("ggplot2")
library("parallel")
library("xml2")
library("magrittr")
library("data.table")

corpora <- "ger"

## Downloading plays
list_of_names <- fromJSON(paste0("https://dracor.org/api/corpora/", corpora))
sorted_ids <- list_of_names$dramas$id[sort.list(list_of_names$dramas$id)]

downloader <- function(playname){
  if (file.exists(paste0("csv/", playname, ".csv"))) {
    
  } else {
    download.file(paste0("https://dracor.org/api/corpora/", corpora, "/play/", playname, "/networkdata/csv"), paste0("csv/", playname, ".csv"))
  }
  read.csv(paste0("csv/", playname, ".csv"), stringsAsFactors = F)
}

plays <- lapply(sorted_ids, downloader)

#plays <- mclapply(sorted_ids, function(x) read.csv(paste0("https://dracor.org/api/corpora/", corpora, "/play/", x, "/networkdata/csv"), stringsAsFactors = F))
#p_chars <- mclapply(sorted_ids, function(x) fromJSON(paste0("https://dracor.org/api/corpora/", corpora, "/play/", x), flatten = T))
p_text <- mclapply(sorted_ids, function(x) fromJSON(paste0("https://dracor.org/api/corpora/", corpora, "/play/", x, "/spoken-text-by-character")))
p_segments <- lapply(sorted_ids, function(x) read_xml(paste0("https://dracor.org/api/corpora/", corpora, "/play/", x, "/segmentation"), encoding = "UTF-8"))

metadata <- read.csv(paste0("https://dracor.org/api/corpora/", corpora, "/metadata.csv"), stringsAsFactors = F)
metadata <- metadata[order(metadata$name),]
## metadata <- metadata[metadata$name != plays_to_remove,] ## removing of plays which do not represent social interactions

#names(plays_chars) <- metadata$name

list_of_df <- function(play){
  df <- data.frame(play$id, stringsAsFactors = F)
  names(df) <- "tmp"
  df$text <- play$text
  df$cast <- play$id
  df <- df[order(df$cast),]
  df$tmp <- NULL
  df$num_words <- lapply(df$text, stri_count_words)
  df$num_sp <- sapply(df$num_words, length)
  df$num_words <- sapply(df$num_words, sum)
  df$text <- NULL
  return (df)
}

plays_text <- lapply(p_text, list_of_df)
plays_text <- lapply(plays_text, function(x) x[order(x$cast),])
names(plays_text) <- metadata$name

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
names(graphs_df) <- metadata$name

cast_compare <- function(x){
  diff <- plays_text[[x]]$cast[!plays_text[[x]]$cast %in% graphs_df[[x]]$name]
  if (!is.null(diff)){
    for (line in diff){
      #graphs_df[[x]] <- rbind(graphs_df[[x]], c(line, as.numeric(c(0, 0, 0, 0, 0))))
      graphs_df[[x]] <- rbind(graphs_df[[x]], c(0, 0, 0, 0, 0, 0))
      graphs_df[[x]]$name[length(graphs_df[[x]]$name)] <- line
      }
    }
  #graphs_df[[x]][,2:6] <- as.numeric(as.vector(graphs_df[[x]][,2:6]))
  return (graphs_df[[x]])
}

#graphs_df38 <- graphs_df[["gogol-teatralnyi-razezd"]]
#text_df38 <- plays_text[["gogol-teatralnyi-razezd"]]
#diff38 <- as.vector(text_df38$cast[!text_df38$cast %in% graphs_df38$name])
#if (!is.null(diff38)){
#  for (line in diff38){
#    print("done")s
#    graphs_df38 <- rbind(graphs_df38, c(line, 0, 0, 0, 0, 0))
#  }
#}
#ifelse (text_df37$cast[!text_df37$cast %in% graphs_df37$name] != "0",
#        graphs_df37 <- rbind(graphs_df37, c(text_df37$cast[!text_df37$cast %in% graphs_df37$name], 0, 0, 0, 0, 0)))

graphs_df <- lapply(names(graphs_df), cast_compare)
graphs_df <- lapply(graphs_df, ranking) #???????????????????????????????????
graphs_df <- lapply(graphs_df, function(x) x[order(x$name),])
names(graphs_df) <- metadata$name


#sum(stri_count_words(p_text[[1]][["text"]][[1]]))
#chars1 <- p_text[[1]][["id"]]
#count_calc1 <- data.frame(chars1)

#graph1 <- graphs_of_plays[[1]]
#graph1 <- net_calc(graph1)
#graph1_df <- as_data_frame(graph1, what="vertices")
#graph1_df <- ranking(graph1_df)

#p_text1 <- plays_text[["andreyev-mysl"]]
#xml1 <- read_xml("https://dracor.org/api/corpora/rus/play/andreyev-mysl/segmentation", encoding = "UTF-8")
#unxml1 <- xml_find_all(p_segments[[1]], ".//sgm") %>% as_list
#unl1 <- sapply(unxml1, unlist)
#ununl1 <- unlist(unl1)
#stages1 <- sapply(p_text1$cast, function (x) stri_count_fixed(ununl1, x))
#stages1_sum <- colSums(stages1)
#stages1_df <- data.frame(keyName=names(stages1_sum), value=stages1_sum, row.names=NULL)

#count <- function(x){ length((which(x == unl))) }
#num_segm1 <- lapply(plays_text[["andreyev-mysl"]][["cast"]], function(x) length(which(x == ununl)))
                    
unxml <- lapply(p_segments, function(x) xml_find_all(x, ".//sgm") %>% as_list)
unl <- lapply(unxml, function(x) lapply(x, unlist))
ununl <- lapply(unl, unlist)
names(ununl) <- metadata$name

stages_func <- function(x, y){
  sapply(x$cast, function (x) stri_count_fixed(y, x))
}

stages <- lapply(names(plays_text), function(x) stages_func(plays_text[[x]], ununl[[x]]))
stages_sum <- lapply(stages, colSums)
stages_df <- lapply(stages_sum, function(x) data.frame(name=as.character(names(x)), num_stages=x, row.names=NULL))
stages_df <- lapply(stages_df, function(x) x[order(x$name),])
names(stages_df) <- metadata$name

text_metrics <- function(x){
  df <- data.frame(plays_text[[x]]$cast, stringsAsFactors = F)
  names(df) <- "cast"
  df$num_words <- plays_text[[x]]$num_words
  df$num_sp <- plays_text[[x]]$num_sp
  df$num_stages <- stages_df[[x]]$num_stages
  return (df)
}

text_df <- lapply(names(plays_text), text_metrics)
names(text_df) <- metadata$name
text_ranks <- lapply(names(text_df), function (x) lapply(-text_df[[x]][,2:4], rank, ties.method = "min"))
names(text_ranks) <- metadata$name

add_ranking <- function(x){
  text_df[[x]]$rank_words <- text_ranks[[x]]$num_words
  text_df[[x]]$rank_sp <- text_ranks[[x]]$num_sp
  text_df[[x]]$rank_stages <- text_ranks[[x]]$num_stages
  text_df[[x]]$text_m <- rank(rowMeans(subset(text_df[[x]], select=c("rank_words", "rank_sp", "rank_stages"))), ties.method = "min")
  return(text_df[[x]])
}

text_df <- lapply(names(text_df), add_ranking)
names(text_df) <- metadata$name

unite <- function(x){
  df <- data.frame(text_df[[x]]$cast, stringsAsFactors = F)
  names(df) <- "cast"
  df$count <- text_df[[x]]$text_m
  df$network <- graphs_df[[x]]$network_m
  df$words <- text_df[[x]]$rank_words
  df$speech_acts <- text_df[[x]]$rank_sp
  df$stages <- text_df[[x]]$rank_stages
  df$betweenness <- graphs_df[[x]]$b_rank
  df$closeness <- graphs_df[[x]]$c_rank
  df$weighted_degree <- graphs_df[[x]]$wd_rank
  df$degree <- graphs_df[[x]]$d_rank
  df$eigenvector <- graphs_df[[x]]$d_rank
  return(df)
}

ranks_df <- lapply(names(graphs_df), unite)
names(ranks_df) <- metadata$name