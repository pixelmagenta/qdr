library("jsonlite")
library("curl")
library("igraph")
library("stringi")
library("ggplot2")
library("parallel")
library("xml2")
library("magrittr")
#library("data.table")
library("dplyr")
library("rapportools")


corpora <- "ger"

## Download for new plays
list_of_names <- fromJSON(paste0("https://dracor.org/api/corpora/", corpora))
sorted_ids <- list_of_names$dramas$name[sort.list(list_of_names$dramas$name)]

#write.csv(sorted_ids, file = "rus_listofnames160.csv")

#Take saved 160 plays
#df_sorted_ids <- read.csv(file="rus_listofnames160.csv", stringsAsFactors = F)
#sorted_ids <- df_sorted_ids$x

download_plays <- function(playname){
  if (!file.exists(paste0("csv/", corpora, "/", playname, ".csv"))) {
    download.file(paste0("https://dracor.org/api/corpora/", corpora, "/play/", playname, "/networkdata/csv"), paste0("csv/", corpora, "/", playname, ".csv"))
  }
  read.csv(paste0("csv/", corpora, "/", playname, ".csv"), stringsAsFactors = F)
}

plays <- lapply(sorted_ids, download_plays)

p_text <- mclapply(sorted_ids, function(x) fromJSON(paste0("https://dracor.org/api/corpora/", corpora, "/play/", x, "/spoken-text-by-character")))
options(timeout = 40000)#probably doesn't help
p_segments <- lapply(sorted_ids, function(x) read_xml(paste0("https://dracor.org/api/corpora/", corpora, "/play/", x, "/segmentation"), encoding = "UTF-8"))


metadata <- read.csv(paste0("https://dracor.org/api/corpora/", corpora, "/metadata.csv"), stringsAsFactors = F) #to update

#write.csv(metadata, file = "rus_metadata160.csv")

#metadata <- read.csv(file="rus_metadata160.csv", stringsAsFactors = F)

metadata$X <- NULL
metadata[,7:16] <- NULL
metadata <- metadata[order(metadata$name),]
## metadata <- metadata[metadata$name != plays_to_remove,] ## removing of plays which do not represent social interactions

names(p_segments) <- metadata$name

#cashing of segments info
#for (name in names(p_segments)){
#  write_xml(p_segments[[name]], file = paste0("rus_segments/", name, ".xml"))
#}


list_of_df <- function(play){
  df <- data.frame(play$id, stringsAsFactors = F)
  names(df) <- "tmp"
  df$text <- play$text
  df$cast <- play$id
  df <- df[order(df$cast),]
  df$tmp <- NULL
  df$num_words <- lapply(df$text, stri_count_words)
  df$num_sp <- as.numeric(sapply(df$num_words, length))
  df$num_words <- as.numeric(sapply(df$num_words, sum))
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
names(graphs_of_plays) <- metadata$name

net_calc <- function(x){
  V(x)$betweenness <- betweenness(x, v = V(x), directed = F, weights = NA)
  #V(x)$betweenness <- betweenness(x, v = V(x), directed = F, weights = x$Weight)
  V(x)$closeness <- round(closeness(x, weights = NA, normalized = T), digits = 7)
  V(x)$w_degree <- strength(x)
  V(x)$degree <- degree(x)
  V(x)$eigenvector <- round(eigen_centrality(x, weights = NA)$vector, digits = 7)
  #V(x)$eigenvector <- round(eigen_centrality(x)$vector, digits = 7) nooo
  #x <- x[order(x$name), ]
  x
}

graphs_of_plays <- mclapply(graphs_of_plays, net_calc)
graphs_df <- mclapply(graphs_of_plays, function(x) igraph::as_data_frame(x, what="vertices"))
graphs_df <- lapply(graphs_df, function(x) x[order(x$name),])
names(graphs_df) <- metadata$name

cast_compare <- function(x){
  diff <- plays_text[[x]]$cast[!plays_text[[x]]$cast %in% graphs_df[[x]]$name]
  if (!is.null(diff)){
    for (line in diff){
      graphs_df[[x]] <- rbind(graphs_df[[x]], c(0, 0, 0, 0, 0, 0))
      graphs_df[[x]]$name[length(graphs_df[[x]]$name)] <- line
    }
  }
  return (graphs_df[[x]])
}
graphs_df <- lapply(names(graphs_df), cast_compare)
names(graphs_df) <- metadata$name

unxml <- lapply(p_segments, function(x) xml_find_all(x, ".//sgm") %>% as_list)
unl <- lapply(unxml, function(x) lapply(x, unlist))
ununl <- lapply(unl, unlist)
ununl <- lapply(ununl, function(x) paste0("{", x, "}"))
names(ununl) <- metadata$name

stages_func <- function(x, y){
  sapply(paste0("{", x$cast, "}"), function (x) stri_count_fixed(y, x))
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

metrics_df <- lapply(names(graphs_df), function (x) data.frame(text_df[[x]], graphs_df[[x]]))
names(metrics_df) <- metadata$name

del_name <- function(df){
  df$name <- NULL
  df
}

metrics_df <- lapply(metrics_df, del_name)

#!!!!!!!!!!!!!!!!!!!! ties.method "min" or "average"???
ranking <- function(x){
  df <- data.frame(x$cast, stringsAsFactors = F) #!!!!!!!!!!!cast
  for (col in names(x)[-1]){
    df$t <- rank(-x[col], ties.method = "min")
    names(df)[names(df) == 't'] <- paste0(col, "_rank")
  }
  df$text <- rank(rowMeans(subset(df, select=c("num_words_rank", "num_sp_rank", "num_stages_rank"))), ties.method = "min")
  df$network <- rank(rowMeans(subset(df, select=c("betweenness_rank", "closeness_rank", "w_degree_rank", "degree_rank", "eigenvector_rank"))), ties.method = "min")
  df$overall <- rank(rowMeans(subset(df, select=c("network", "text"))), ties.method = "min")
  df$sum <- rowSums(df[2:9])
  names(df$x.cast) <- "cast"
  return(df)
}

ranks_df <- lapply(metrics_df, ranking)
names(ranks_df) <- metadata$name

num_one <- function(x){
  if (rapportools::is.empty(dplyr::filter(ranks_df[[x]], num_words_rank==1 & num_sp_rank==1 & num_stages_rank==1 & betweenness_rank==1 & closeness_rank==1 & w_degree_rank==1 & degree_rank==1 & eigenvector_rank==1))) {
    num <- 0
  } else {
    num <- ranks_df[[x]][ranks_df[[x]]$sum == 8,]$x.cast
    #num <- 1
    #protagonists <- rbind(protagonists, c(x, ranks_df[[x]][ranks_df[[x]]$num_words_rank == 1,]$x.cast))
  }
  return (num) 
}

num_one2 <- function(x){
  if (rapportools::is.empty(dplyr::filter(ranks_df[[x]], sum==9))) {
    num <- 0
  } else {
    num <- ranks_df[[x]][ranks_df[[x]]$sum == 9,]$x.cast
    #num <- 1
    #protagonists <- rbind(protagonists, c(x, ranks_df[[x]][ranks_df[[x]]$num_words_rank == 1,]$x.cast))
  }
  return (num) 
}

search_for_examples <- function(x){
  if (rapportools::is.empty(dplyr::filter(ranks_df[[x]], network==1 & text!=1))) {
    num <- 0
  } else {
    num <- ranks_df[[x]][ranks_df[[x]]$num_words_rank == 1,]$x.cast
    #protagonists <- rbind(protagonists, c(x, ranks_df[[x]][ranks_df[[x]]$num_words_rank == 1,]$x.cast))
  }
  return (num) 
}

max_of_metric <- function(x){
  if (rapportools::is.empty(dplyr::filter(ranks_df[[x]], num_words_rank != 1 && num_sp_rank == 1))) {
    num <- 0
  } else {
    num <- ranks_df[[x]][ranks_df[[x]]$num_words_rank == 1,]$x.cast
    #protagonists <- rbind(protagonists, c(x, ranks_df[[x]][ranks_df[[x]]$num_words_rank == 1,]$x.cast))
  }
  return (num) 
}

metadata$cor_coeff <- sapply(names(ranks_df), function(x) cor.test(ranks_df[[x]]$text, ranks_df[[x]]$network, method = "spearman", exact = FALSE)$estimate[["rho"]])
metadata$stages_vs_sp <- sapply(names(ranks_df), function(x) cor.test(ranks_df[[x]]$num_sp_rank, ranks_df[[x]]$num_stages_rank, method = "spearman")$estimate[["rho"]])
#Correlation table for all eight metrics
#cor(ranks_df[["pushkin-rusalka"]][2:9], method = "spearman")

metadata$num_one <- sapply(names(graphs_df), num_one)
metadata$num_one2 <- sapply(names(graphs_df), num_one2)
metadata$example <- sapply(names(graphs_df), search_for_examples)
metadata$num_of_components <- sapply(graphs_of_plays, count_components)



get_cluster_sizes_3max <- function(arr){
  diffs <- diff(sort(arr))
  # compute 3 max diffs between subsequent pairs
  maxs <-  c(-1, -1, -1)
  idx <-  c(-1, -1, -1)
  i <- 1
  for (d in rev(diffs)){
    if (d > maxs[1]) {
      maxs = c(d, maxs[1:2])
      idx = c(i, idx[1:2])
    } else if (d > maxs[2]) {
      maxs = c(maxs[1], d, maxs[2])
      idx = c(idx[1], i, idx[2])
    } else if (d > maxs[3]) {
      maxs[3] <- d
      idx[3] <- i
    } 
    i <- i + 1
  }
  # return number of elements in each group
  return (diff(sort(c(0, idx, length(arr)))))
}


get_cluster_sizes <- function(arr){
  if (length(unique(arr)) > 3){
    return (get_cluster_sizes_3max(arr))
  } else {
    diffs <- diff(sort(arr))
    # compute 2 max diffs between subsequent pairs
    maxs <-  c(-1, -1)
    idx <-  c(-1, -1)
    i <- 1
    for (d in rev(diffs)){
      if (d > maxs[1]) {
        maxs = c(d, maxs[1])
        idx = c(i, idx[1])
      } else if (d > maxs[2]) {
        maxs[2] <- d
        idx[2] <- i
      } 
      i <- i + 1
    }
    # return number of elements in each group
    # hack: append max index twice (in idx and idx[1]) to create an empty group
    return (diff(sort(c(0, idx, idx[1], length(arr)))))
  }
}

quantify_importance <- function(x){
    df <- data.frame(ordered(c("Major", "Minor Major", "Major Minor", "Minor"), levels = c("Minor", "Major Minor", "Minor Major", "Major")))
    names(df) <- "group"
    if (length(x$cast) != 2) { #there are 7 Rus and 15 Ger plays where length(x$cast)<=3
      for (col in names(x)[2:9]){
        df[col] <- get_cluster_sizes(unlist(c(x[col])))
        df[col]<- prop.table(df[col])
      }
  } else {
      for (col in names(x)[2:9]){
        df[col]<- c(2,0,0,0)
        df[col]<- prop.table(df[col])
        }
    }
  return (df)
}

quartiles_df <- lapply(metrics_df, quantify_importance)
names(quartiles_df) <- metadata$name

quartiles_bind <- bind_rows(quartiles_df)
percentages_df <- quartiles_bind %>% group_by(group) %>% summarise_all(list(~sum))
percentages_df <- cbind(percentages_df[1], percentages_df[2:9]*100/length(metadata$name))

#CUT APPROACH

cut_quartiles <- function(x){
  df <- data.frame(ordered(c("I", "II", "III", "IV"), levels = c("IV", "III", "II", "I")))
  #df <- data.frame(ordered(c("Major", "Minor Major", "Major Minor", "Minor"), levels = c("Minor", "Major Minor", "Minor Major", "Major")))
  names(df) <- "group"
  for (col in names(x)[2:9]){
    adf <- as.data.frame(table(cut(unlist(c(x[col])), 4, labels = F)), stringsAsFactors = F)
    adf$Var1 <- as.numeric(adf$Var1)
    diff <- setdiff(c(1, 2, 3, 4), adf$Var1)
    if (!is.null(diff)){
      for (val in diff){
        adf <- rbind(adf, c(val, 0))
      }
    }
    adf <- adf[order(-adf$Var1), ]
    df[col] <- adf$Freq
    df[col]<- prop.table(df[col])
  }
  return (df)
}

cut_quartiles_df <- lapply(metrics_df, cut_quartiles)
names(cut_quartiles_df) <- metadata$name

cut_quartiles_bind <- bind_rows(cut_quartiles_df)
cut_quartiles_groups <- cut_quartiles_bind %>% group_by(group)
attr(cut_quartiles_groups$num_words, "groups") <- NULL
#cut_percentages_df <- cut_quartiles_groups %>% summarise_all(list(~sum)) Похоже что надо как-то убрать аттрибуты
cut_percentages_df <- cut_quartiles_bind %>% group_by(group) %>% summarise_all(list(~sum))
cut_percentages_df <- cbind(cut_percentages_df[1], cut_percentages_df[2:9]*100/length(metadata$name))

cut_quartiles_sumz <- summarize(cut_quartiles_bind, llist(cut_quartiles_bind$group), sum)
cut_quartiles_groups2 <- group_by(cut_quartiles_bind, group)
summarise(cut_quartiles_groups2, )

cut_percentages_df <- aggregate(. ~ group, cut_quartiles_bind, sum)
cut_percentages_df <- cbind(cut_percentages_df[1], cut_percentages_df[2:9]*100/length(metadata$name))

major_group_df <- data.frame(metadata$name, metadata$year, metadata$numOfSpeakers, stringsAsFactors = F)
names(major_group_df) <- c("name", "year", "numOfSpeakers")
major_group_df$year <- paste0(substr(major_group_df$year, 1,3), "0")
major_group_df[major_group_df$year == 1740, ]$year <- 1750
major_group_df[major_group_df$year == 1940, ]$year <- 1930
#major_group_df[major_group_df$year == 1930, ]$year <- 1920
major_group_df$degree <- sapply(major_group_df$name, function (x) quartiles_df[[x]][["degree"]][1]*100)
major_group_df$num_words <- sapply(major_group_df$name, function (x) quartiles_df[[x]][["num_words"]][1]*100)

#major_group_df$cut_degree2 <- sapply(major_group_df$name, function (x) cut_quartiles_df[[x]][["degree"]][3]*100)
major_group_df$cut_degree <- sapply(major_group_df$name, function (x) cut_quartiles_df[[x]][["degree"]][1]*100)
major_group_df$cut_num_words <- sapply(major_group_df$name, function (x) cut_quartiles_df[[x]][["num_words"]][1]*100)
major_group_df$cut_num_sp <- sapply(major_group_df$name, function (x) cut_quartiles_df[[x]][["num_sp"]][1]*100)
major_group_df$cut_num_stages <- sapply(major_group_df$name, function (x) cut_quartiles_df[[x]][["num_stages"]][1]*100)
major_group_df$cut_closeness <- sapply(major_group_df$name, function (x) cut_quartiles_df[[x]][["closeness"]][1]*100)
major_group_df$cut_w_degree <- sapply(major_group_df$name, function (x) cut_quartiles_df[[x]][["w_degree"]][1]*100)
major_group_df$cut_eigenvector <- sapply(major_group_df$name, function (x) cut_quartiles_df[[x]][["eigenvector"]][1]*100)
major_group_df$cut_betweenness <- sapply(major_group_df$name, function (x) cut_quartiles_df[[x]][["betweenness"]][1]*100)


tsar_boris <- decompose.graph(graphs_of_plays[["tolstoy-tsar-boris"]])


delo <- data.frame(metrics_df[["sukhovo-kobylin-delo"]]$cast, metrics_df[["sukhovo-kobylin-delo"]]$degree)

