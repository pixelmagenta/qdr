library("ggplot2")
#library("wesanderson")
#library("Redmonder")
library("rcartocolor")
library("gridExtra")
library("reshape")
library("ggsci")
library("pals")
library("SDMTools")
library("plotfunctions")



theme_set(theme_gray(base_size = 24))
ggplot(metadata, aes(x=metadata$numOfSpeakers, y=metadata$cor_coeff))+
  geom_boxplot(na.rm = TRUE, color="#2a5674", fill="#96d0d1", size = 1.0, outlier.shape = NA)+
  geom_jitter(color="#2a5674", fill="#96d0d1", size = 2.8)+
  labs(x="Number of characters", y = "Correlation coefficient")+
  #ggtitle("Russian")+
  theme(plot.title = element_text(hjust = 0.5))

theme_set(theme_gray(base_size = 24))
ggplot(metadata, aes(x=metadata$numOfSpeakers, y=metadata$cor_coeff))+
  geom_boxplot(na.rm = TRUE, color="royalblue4", fill="skyblue1", size = 1.0, outlier.shape = NA)+
  geom_jitter(color="royalblue4", fill="skyblue1", size = 2.8)+
  labs(x="Number of characters", y = "Correlation coefficient")+
  #ggtitle("Russian")+
  theme(plot.title = element_text(hjust = 0.5))

theme_set(theme_gray(base_size = 24))
ggplot(metadata, aes(x=metadata$numOfSpeakers, y=metadata$cor_coeff))+
  geom_boxplot(na.rm = TRUE, color="aquamarine4", fill="aquamarine1", size = 1.0, outlier.shape = NA)+
  geom_jitter(color="aquamarine4", fill="aquamarine1", size = 2.4)+
  labs(x="Number of characters", y = "Correlation coefficient")+
  ggtitle("German")+
  theme(plot.title = element_text(hjust = 0.5))

theme_set(theme_gray(base_size = 18))
ggplot(metadata, aes(x=metadata$numOfSpeakers, y=metadata$cor_coeff))+
  geom_boxplot(na.rm = TRUE, color="seagreen4", fill="seagreen1", size = 0.9, outlier.shape = NA)+
  geom_jitter(color="seagreen4", fill="seagreen1", size = 2.5)+
  labs(x="Number of characters", y = "Correlation coefficient")+
  ggtitle("German")+
  theme(plot.title = element_text(hjust = 0.5))


percentages_df <- data.frame(percentages_df$group,
                             percentages_df$degree,
                             percentages_df$w_degree,
                             percentages_df$closeness,
                             percentages_df$betweenness,
                             percentages_df$eigenvector,
                             percentages_df$num_words,
                             percentages_df$num_sp,
                             percentages_df$num_stages)

names(percentages_df) <- c("group",
                           "Degree",
                           "Weighted degree",
                           "Closeness centrality",
                           "Betweenness centrality",
                           "Eigenvector centrality",
                           "Number of words", 
                           "Number of speech acts", 
                           "Number of stages")

percentages_df2 <- melt(percentages_df, id.vars = c("group"))

theme_set(theme_minimal(base_size = 14))
percentages_plot <- ggplot(percentages_df2, aes(x=group, y=value, fill = group)) + 
  geom_bar(stat = "identity") +
  #scale_fill_manual(values=c("#e4c7f1", "#d1afe8", "#b998dd", "#9f82ce"))+
  scale_fill_manual(values=c("#96d0d1", "#68abb8", "#45829b", "#2a5674"))+
  labs(x = NULL, y = NULL)+
  #theme_minimal(baze_size = 14)+
  theme(legend.position="none")+
  geom_text(aes(label = paste0(round(percentages_df2$value, digits = 0),"%")), hjust=-0.1, size=3)+
  coord_flip()

percentages_plot + facet_wrap(percentages_df2$variable, nrow = 8)
#+ ggtitle("RusDraCor")



percentages_plot <- ggplot(percentages_df2, aes(x=group, y=value, fill = group)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=carto_pal(7, "Emrld"))+
  labs(x = NULL, y = NULL)+
  theme_minimal()+
  theme(legend.position="none")+
  geom_text(aes(label = paste0(round(percentages_df2$value, digits = 0),"%")), hjust=-0.1, size=3)+
  coord_flip()

percentages_plot + facet_wrap(percentages_df2$variable, nrow = 8) + ggtitle("GerDraCor")

theme_set(theme_gray(base_size = 18))
ggplot(major_group_df, aes(x=year, y=degree)) + 
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=median, geom="smooth", aes(group=0),lwd=1) +
  geom_jitter()






ranking_for_plots <- function(x){
  df <- data.frame(x$cast, stringsAsFactors = F)
  for (col in names(x)[-1]){
    df$t <- rank(x[col], ties.method = "last")
    names(df)[names(df) == 't'] <- paste0(col, "_rank")
  }
  df$text <- rank(rowMeans(subset(df, select=c("num_words_rank", "num_sp_rank", "num_stages_rank"))), ties.method = "max")
  df$network <- rank(rowMeans(subset(df, select=c("betweenness_rank", "closeness_rank", "w_degree_rank", "degree_rank", "eigenvector_rank"))), ties.method = "max")
  df$overall <- rank(rowMeans(subset(df, select=c("network", "text"))), ties.method = "max")
  names(df$x.cast) <- "cast"
  return(df)
}

plot_ranks_df <- lapply(metrics_df, ranking_for_plots)
names(plot_ranks_df) <- metadata$name


s <- "babel-marija"
g <- graphs_of_plays[[s]]
l <- layout_with_dh(g)
View(metrics_df[[s]])
View(ranks_df[[s]])
View(plot_ranks_df[[s]])

V(g)$color <- kovesi.linear_bmy_10_95_c71(max(metrics_df[[s]]$eigenvector*10))[V(g)$eigenvector]
plot_colors <- kovesi.linear_bmy_10_95_c71(max(metrics_df[[s]]$eigenvector*10))[V(g)$eigenvector*10]

plot_colors <- kovesi.linear_bmy_10_95_c71(max(metrics_df[[s]]$betweenness))[V(g)$betweenness/10]

plot_colors <- kovesi.linear_bmy_10_95_c71(vcount(g))[V(g)$w_degree]

V(g)$color <- plot_colors
V(g)$frame.color <- plot_colors

V(g)$label.color <- "black"
V(g)$label.cex <- 1.6
V(g)$label.degree <- -pi/2
plot(g, vertex.label.dist = 1.3, vertex.size = 11, vertex.label.family = "Times",layout = l)


write.csv(l, file = paste0("layout_best_dh_", s, ".csv"))
