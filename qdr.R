library("jsonlite")
#library("curl")
library("igraph")
library("stringi")
library("ggplot2")
library("parallel")
library("xml2")
library("magrittr")
library("data.table")
library("dplyr")
#library("rapportools")

corpora <- "rus"

## Downloading plays
list_of_names <- fromJSON(paste0("https://dracor.org/api/corpora/", corpora))
sorted_ids <- list_of_names$dramas$id[sort.list(list_of_names$dramas$id)]

downloader <- function(playname){
  if (!file.exists(paste0("csv/", playname, ".csv"))) {
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
metadata$nodes <- NULL
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
  #V(x)$eigenvector <- round(eigen_centrality(x)$vector, digits = 7) nooo
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

metrics_df <- lapply(names(graphs_df), function (x) data.frame(text_df[[x]], graphs_df[[x]]))
names(metrics_df) <- metadata$name

del_name <- function(df){
  df$name <- NULL
  df
}

metrics_df <- lapply(metrics_df, del_name)

ranking <- function(x){
  df <- data.frame(x$cast, stringsAsFactors = F)
  for (col in names(x)[-1]){
    df$t <- rank(-x[col], ties.method = "min")
    names(df)[names(df) == 't'] <- paste0(col, "_rank")
  }
  df$text <- rank(rowMeans(subset(df, select=c("num_words_rank", "num_sp_rank", "num_stages_rank"))), ties.method = "min")
  df$network <- rank(rowMeans(subset(df, select=c("betweenness_rank", "closeness_rank", "w_degree_rank", "degree_rank", "eigenvector_rank"))), ties.method = "min")
  df$overall <- rank(rowMeans(subset(df, select=c("network", "text"))), ties.method = "min")
  names(df$x.cast) <- "cast"
  return(df)
}

ranks_df <- lapply(metrics_df, ranking)
names(ranks_df) <- metadata$name

num_one <- function(x){
  if (rapportools::is.empty(dplyr::filter(ranks_df[[x]], num_words_rank==1 & num_sp_rank==1 & num_stages_rank==1 & betweenness_rank==1 & closeness_rank==1 & w_degree_rank==1 & degree_rank==1 & eigenvector_rank==1))) {
    num <- 0
  } else {
    num <- 1
  }
  return (num) 
}

metadata$cor_coeff <- sapply(names(ranks_df), function(x) cor.test(ranks_df[[x]]$text, ranks_df[[x]]$network, method = "spearman")$estimate[["rho"]])
#Correlation table for all eight metrics
#cor(ranks_df[["pushkin-rusalka"]][2:9], method = "spearman")

metadata[,7:19] <- NULL
metadata$num_one <- sapply(names(graphs_df), num_one)

#ggplot(metadata, aes(y=metadata$cor_coeff))+geom_boxplot(na.rm = TRUE)

theme_set(theme_gray(base_size = 24))
ggplot(metadata, aes(x=metadata$numOfSpeakers, y=metadata$cor_coeff))+
  geom_boxplot(na.rm = TRUE, color="darkblue", fill="lightblue", size = 1.5, outlier.shape = NA)+
  geom_jitter(color="darkblue", fill="lightblue", size = 3)+
  labs(x="Number of characters", y = "Correlation coefficient")+
  #ggtitle("Russian")+
  theme(plot.title = element_text(hjust = 0.5))

theme_set(theme_gray(base_size = 18))
ggplot(metadata, aes(x=metadata$numOfSpeakers, y=metadata$cor_coeff))+
  geom_boxplot(na.rm = TRUE, color="seagreen4", fill="seagreen1", size = 0.9)+
  geom_jitter(color="seagreen4", fill="seagreen1", size = 2.5)+
  labs(x="Number of characters", y = "Correlation coefficient")+
  ggtitle("German")+
  theme(plot.title = element_text(hjust = 0.5))

quartiles <- function(x){
  if (length(x$cast)>3) {
    df_q <- data.frame(x$cast, stringsAsFactors = F)
    #df_q$betweeness <- x$betweenness
    
    df <- data.frame(c("first","second","third", "fourth"), stringsAsFactors = F)
    names(df) <- "group"
    
    for (col in names(x)[2:9]){
      df_q[col] <- ntile(x[col], 4)
      df_t <- df_q %>% group_by_(col) %>% count_(col)
      df[col] <- df_t$n
      df[col]<- prop.table(df[col])
    }
  }
  
  #df_q$betweeness <- ntile(df_q$betweeness, 4)
  #df <- df_q %>% group_by(betweeness) %>% count(betweeness)
  #df$betweeness <- NULL
  #names(df)[1] <- "betweenness"
  
  return (df)
}

quartiles_df <- lapply(metrics_df, quartiles)
names(quartiles_df) <- metadata$name


df_q2 <- data.frame(graphs_df[["tolstoy-tsar-boris"]]$name, stringsAsFactors = F)
df_q2$eigenvector <- graphs_df[["tolstoy-tsar-boris"]]$eigenvector
df_q2$eigenvector <- dplyr::ntile(df_q2$eigenvector, 4)
df2 <- df_q2 %>% dplyr::group_by(eigenvector) %>% dplyr::count(eigenvector)

tmp <- graphs_df[["andreyev-mysl"]]
tmp1 <- data.frame(graphs_df[["andreyev-mysl"]]$name, stringsAsFactors = F)
tmp1$betweenness <- dplyr::ntile(graphs_df[["andreyev-mysl"]]$betweenness, 4)
tmp2 <- tmp1 %>% dplyr::group_by(quartile) %>% dplyr::count(quartile)
tmp_sum <- tmp2 %>% count(quartile)
