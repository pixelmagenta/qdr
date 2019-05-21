
df <- subset(metadata, select = c(name, year, numOfSpeakers) )

## Comparison of R-squared for power law ##

## preprocessing

degree_dist <- lapply(graphs_of_plays, function(x) degree(x, v = V(x), loops = FALSE, normalized = FALSE))
degree_dist_v <- lapply(degree_dist, as.vector)
number_of_nodes <- lapply(degree_dist_v, table)
distribution <- lapply(number_of_nodes, as.data.frame)

num_type <- function(x){
  x$Var1 <- as.numeric(as.character(x$Var1))
  x$Freq <- as.numeric(x$Freq)
  names(x) <- c("Node_degree", "Num_of_nodes")
  x
}

distribution <- lapply(distribution, num_type)

## regressions
fit_log <- lapply(distribution, function(x) lm(log(x$Num_of_nodes) ~ log(x$Node_degree)))
df$Rsqrd_log <- sapply (fit_log, function(x) summary(x)$r.squared)

df$num_of_degrees <- sapply(metrics_df, function(x) length(unique(x$degree)))

df_quadratic <- df[df$num_of_degrees > 2, ]
fit_quadratic <- lapply(df[df$num_of_degrees > 2, ]$name, function(x) lm(distribution[[x]]$Num_of_nodes ~ poly(distribution[[x]]$Node_degree, 2)))
df_quadratic$Rsqrd_quadratic <- sapply (fit_quadratic, function(x) summary(x)$r.squared)



## PLOTS ##

theme_set(theme_gray(base_size = 15)) ## size of axis font
plot1 <- ggplot(na.omit(df), aes(x = year, y = CC_dev, 
                                 fill = crit_1, shape = crit_2, color = crit_1))+
  geom_point(size = 6)+
  scale_shape_manual(values=c(21, 22), name = "Criterion 2")+
  scale_color_manual(values=c("slateblue3", "indianred3"), name = "Criterion 1")+
  scale_fill_manual(values=c("slateblue1", "indianred1"), name = "Criterion 1")+
  labs(x="Year of creation", y = "Clustering coefficient deviation")
plot1


theme_set(theme_gray(base_size = 15)) ## size of axis font
plot2 <- ggplot(na.omit(df), aes(x = numOfSpeakers, y = CC_dev, 
                                 fill = crit_1, shape = crit_2, color = crit_1))+
  geom_point(size = 6)+
  scale_shape_manual(values=c(21, 22), name = "Criterion 2")+
  scale_color_manual(values=c("slateblue3", "indianred3"), name = "Criterion 1")+
  scale_fill_manual(values=c("slateblue1", "indianred1"), name = "Criterion 1")+
  labs(x="Number of characters", y = "Clustering coefficient deviation")+
  coord_cartesian(xlim = c(0, max(df$numOfSpeakers)+12))+
  geom_text(aes(label=ifelse(numOfSpeakers>75,as.character(name),'')),hjust=0.5,vjust=1.5, color="black", size=4)
plot2


power_law_plot <- function(name_of_play){
  id <- which(name_of_play == df$name)[[1]]
  loglog <- lm(log(distribution[[id]]$Num_of_nodes) ~ log(distribution[[id]]$Node_degree))
  loglog_df <- data.frame(x = distribution[[id]]$Node_degree, y = exp(fitted(loglog)))
  
  ggplot(data = distribution[[id]], aes(x = Node_degree, y = Num_of_nodes))+ 
    geom_point(size = 3) + 
    geom_line(data = loglog_df, aes(x = x, y = y), linetype = 2, color="blue", size = 1)+
    labs(x="Node degree", y = "Number of nodes")
}

power_law_plot("ostrovsky-snegurochka")


some_law_plot <- function(name_of_play){
  id <- which(name_of_play == df$name)[[1]]
  loglog <- lm((distribution[[id]]$Num_of_nodes) ~ poly(distribution[[id]]$Node_degree, 2))
  loglog_df <- data.frame(x = distribution[[id]]$Node_degree, y = fitted(loglog))
  
  ggplot(data = distribution[[id]], aes(x = Node_degree, y = Num_of_nodes))+ 
    geom_point(size = 3) + 
    geom_line(data = loglog_df, aes(x = x, y = y), linetype = 2, color="blue", size = 1)+
    labs(x="Node degree", y = "Number of nodes")
}

some_law_plot("fonvizin-nedorosl")



