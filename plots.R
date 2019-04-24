library("wesanderson")
library("Redmonder")
library("rcartocolor")
library("gridExtra")

bar_chart1 <- ggplot(percentages_df, aes(x=group, y=betweenness, fill = group)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=redmonder.pal(4,"dPBIPuGn"))+
  coord_flip()
bar_chart1

bar_chart2 <- ggplot(percentages_df, aes(x=group, y=betweenness, fill = group)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=wes_anderson(4,"GrandBudapest2"))+
  coord_flip()
bar_chart2

p1 <- ggplot(percentages_df, aes(x=group, y=degree, fill = group)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=carto_pal(7, "Emrld"))+
  #theme(legend.position="none")+
  theme_minimal()+
  coord_flip()

p2 <- ggplot(percentages_df, aes(x=group, y=closeness, fill = group)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=carto_pal(7, "Emrld"))+
  #theme(legend.position="none")+
  theme_minimal()+
  coord_flip()

p3 <- ggplot(percentages_df, aes(x=group, y=betweenness, fill = group)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=carto_pal(7, "Emrld"))+
  theme_minimal()+
  coord_flip()

grid.arrange(p1, p2, p3, nrow = 3)
