library("ggplot2")
#library("wesanderson")
#library("Redmonder")
library("rcartocolor")
library("gridExtra")
library("reshape")
library("ggsci")
library("pals")

theme_set(theme_gray(base_size = 24))
ggplot(metadata, aes(x=metadata$numOfSpeakers, y=metadata$cor_coeff))+
  geom_boxplot(na.rm = TRUE, color="darkblue", fill="lightblue", size = 1.5, outlier.shape = NA)+
  geom_jitter(color="darkblue", fill="lightblue", size = 3)+
  labs(x="Number of characters", y = "Correlation coefficient")+
  ggtitle("Russian")+
  theme(plot.title = element_text(hjust = 0.5))

theme_set(theme_gray(base_size = 18))
ggplot(metadata, aes(x=metadata$numOfSpeakers, y=metadata$cor_coeff))+
  geom_boxplot(na.rm = TRUE, color="seagreen4", fill="seagreen1", size = 0.9)+
  geom_jitter(color="seagreen4", fill="seagreen1", size = 2.5)+
  labs(x="Number of characters", y = "Correlation coefficient")+
  ggtitle("German")+
  theme(plot.title = element_text(hjust = 0.5))



percentages_df2 <- melt(cut_percentages_df, id.vars = c("group"))

percentages_plot <- ggplot(percentages_df2, aes(x=group, y=value, fill = group)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=carto_pal(7, "SunsetDark"))+
  theme_minimal()+
  theme(legend.position="none")+
  geom_text(aes(label = paste0(round(percentages_df2$value, digits = 0),"%")), hjust=1.5, size=3)+
  coord_flip()

percentages_plot + facet_wrap(percentages_df2$variable, nrow = 8) + ggtitle("RusDraCor")



percentages_plot <- ggplot(percentages_df2, aes(x=group, y=value, fill = group)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=carto_pal(7, "Emrld"))+
  theme_minimal()+
  theme(legend.position="none")+
  geom_text(aes(label = paste0(round(percentages_df2$value, digits = 0),"%")), hjust=1.5, size=3)+
  coord_flip()

percentages_plot + facet_wrap(percentages_df2$variable, nrow = 8) + ggtitle("GerDraCor")

max(metrics_df[["chekhov-svadba"]]$eigenvector)

g <- graphs_of_plays[["pushkin-boris-godunov"]]
V(g)$color <- kovesi.linear_bmy_10_95_c71(vcount(g))[V(g)$closeness*100]
V(g)$frame.color = kovesi.linear_bmy_10_95_c71(vcount(g))[V(g)$closeness*100]
V(g)$label.color <- "black"
V(g)$label.cex <- 1.1
V(g)$label.degree <- -pi/2
l <- layout_nicely(g)
plot(g, vertex.label.dist = 1.3, vertex.size = 10, layout = l)

