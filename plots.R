library("ggplot2")
#library("wesanderson")
#library("Redmonder")
library("rcartocolor")
library("gridExtra")
library("reshape")
library("ggsci")
library("pals")
library("ggpubr")
#library("SDMTools")
#library("plotfunctions")
library("svglite")


theme_set(theme_minimal(base_size = 28))

cor_coeff_plot <- ggplot(metadata, aes(x=metadata$numOfSpeakers, y=metadata$cor_coeff))+
  geom_boxplot(na.rm = TRUE, color="#2a5674", fill="#96d0d1", size = 1.0, outlier.shape = NA)+
  geom_jitter(color="#2a5674", fill="#96d0d1", size = 2.8)+
  labs(x="Number of characters", y = "Correlation coefficient")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal(base_size = 28)
  #theme(panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
   #            plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"))
#ggtitle("Russian")+

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



percentages <- cut_percentages_df

percentages <- data.frame(percentages$group,
                             percentages$degree,
                             percentages$w_degree,
                             percentages$closeness,
                             percentages$betweenness,
                             percentages$eigenvector,
                             percentages$num_words,
                             percentages$num_sp,
                             percentages$num_stages)

names(percentages) <- c("group",
                           "Degree",
                           "Weighted degree",
                           "Closeness centrality",
                           "Betweenness centrality",
                           "Eigenvector centrality",
                           "Number of words", 
                           "Number of speech acts", 
                           "Number of stages")

first <- percentages
first$`Weighted degree` <- NULL
first$`Closeness centrality` <- NULL
first$`Eigenvector centrality` <- NULL

percentages_df2 <- melt(first, id.vars = c("group"))

#theme_set(theme_minimal(base_size = 14))
percentages_plot <- ggplot(percentages_df2, aes(x=group, y=value, fill = group)) + 
  geom_bar(stat = "identity", width = 1) +
  #scale_fill_manual(values=c("#e4c7f1", "#d1afe8", "#b998dd", "#9f82ce"))+
  scale_fill_manual(values=c("#96d0d1", "#68abb8", "#45829b", "#2a5674"))+
  labs(x = NULL, y = NULL)+
  #theme_minimal(baze_size = 14)+
  #theme(legend.position="none")+
  geom_text(aes(label = paste0(round(percentages_df2$value, digits = 0),"%")), hjust=-0.1, size=7)+
  theme_minimal(base_size = 24) +
  theme(legend.position="none")+
  coord_flip()

#theme(legend.position="none", panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
#plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"))+

percentages_five <- percentages_plot + facet_wrap(percentages_df2$variable, nrow = 8)
#+ ggtitle("RusDraCor")
percentages_five

theme_set(theme_minimal(base_size = 14))
percentages_plot <- ggplot(percentages_df2, aes(x=group, y=value, fill = group)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=carto_pal(7, "Emrld"))+
  labs(x = NULL, y = NULL)+
  theme_minimal()+
  theme(legend.position="none")+
  geom_text(aes(label = paste0(round(percentages_df2$value, digits = 0),"%")), hjust=-0.1, size=3)+
  coord_flip()

percentages_plot + facet_wrap(percentages_df2$variable, nrow = 8) + ggtitle("GerDraCor")


#theme_set(theme_minimal(base_size = 18))
first_group_degree_plot <- ggplot(major_group_df, aes(x=year, y=cut_degree)) + 
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=median, geom="smooth", aes(group=0),lwd=1.7, color="#CE4C73") +
  labs(x="Decades", y = "Percentage of characters in the first group")+
  geom_jitter(size=2.5)+
  theme_minimal(base_size = 26)
  #theme(panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
  #      plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"))
first_group_degree_plot


ggsave(file="percentages_five.svg", plot=percentages_five, width=13.0208, height=8.75)
ggsave(file="cor_coeff.svg", plot=cor_coeff_plot, width=9.1875, height=9.375)
ggsave(file="first_group_degree.svg", plot=first_group_degree_plot, width=15.625, height=8.2708)

s <- "gogol-revizor"
g <- graphs_of_plays[[s]]
l <- layout_with_dh(g)
View(metrics_df[[s]])
View(ranks_df[[s]])
View(plot_ranks_df[[s]])

plot_colors <- kovesi.linear_bmy_10_95_c71(max(metrics_df[[s]]$eigenvector*10))[V(g)$eigenvector]

plot_colors <- kovesi.linear_bmy_10_95_c71(max(V(g)$w_degree))[V(g)$w_degree]

plot_colors <- kovesi.linear_bmy_10_95_c71(max(V(g)$betweenness))[V(g)$betweenness]

plot_colors <- kovesi.linear_bmy_10_95_c71(max(V(g)$closeness*10))[V(g)$closeness*10]

plot_colors <- kovesi.linear_bmy_10_95_c71(vcount(g))[V(g)$degree]

V(g)$color <- plot_colors
V(g)$frame.color <- plot_colors

V(g)$label.color <- "black"
V(g)$label.cex <- 1.6
V(g)$label.degree <- -pi/2
plot(g, vertex.label.dist = 1.3, vertex.size = 11, vertex.label.family = "Times",layout = l)


write.csv(l, file = paste0("layout_best_dh_", s, ".csv"))

l33 <- read.csv(file="layout_best_dh_babel-marija.csv")
l33_m <- data.matrix(l33[2:3])
lles <- l
write.csv(lles, "layout_les.csv")
