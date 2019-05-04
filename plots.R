library("ggplot2")
#library("wesanderson")
#library("Redmonder")
library("rcartocolor")
library("gridExtra")
library("reshape")
library("ggsci")

percentages_df2 <- melt(percentages_df, id.vars = c("group"))

percentages_plot <- ggplot(percentages_df2, aes(x=group, y=value, fill = group)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=carto_pal(7, "Emrld"))+
  theme_minimal()+
  theme(legend.position="none")+
  geom_text(aes(label = paste0(round(percentages_df2$value, digits = 0),"%")), hjust=1.5, size=3)+
  coord_flip()

percentages_plot + facet_wrap(percentages_df2$variable, nrow = 8) + ggtitle("GerDraCor")
