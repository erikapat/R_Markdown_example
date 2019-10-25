
# Calculate_information_gain


Graph_information_gain <- function(dt.values){
  
  
  dt.values <- melt(as.matrix(dt.values))
  dt.values <- as.data.table(dt.values)
  dt.values <- dt.values[order(-value)]
  
  d <- ggplot(dt.values, aes(y = value, x = reorder(Var1, value))) +  geom_bar(stat = "identity", color = 'blue') + coord_flip()
  d <- d + ylab('Entropy') + xlab('Features') 
  d <- d + theme(axis.title.y = element_text(size = rel(1.5)))
  d <- d + theme(axis.text.y = element_text(size = rel(1.8)))
  d <- d + theme(axis.text.x = element_text(size = rel(1.8)))
  d <- d + theme(axis.title.x = element_text(size = rel(1.5)))
  d <- d + theme(legend.text = element_text(size = 15))
  d <- d + theme(legend.title = element_text(size = 15))
  d <- d +  theme(
    panel.background = element_rect(fill="white") ,
    panel.grid.minor.y = element_line(size=3),
    panel.grid.major = element_line(colour = "lightgray"),
    plot.background = element_rect(fill="white")
  )
  return(d)
}
