
#--------------------------------------------------------------------------------------------------------
# CORRELATION GRAPH

CorrelationGraph_1 <- function(train){
  
  # Write to the log:
  cat("\n")
  cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
  
  # Generate output files with write_csv(), plot() or ggplot()
  # Any files you write to the current directory get shown as outputs
  trans.mat <- train
  #sparse.mat <- as(trans.mat, "dgCMatrix")
  M <- cor(trans.mat)
  
  #out.folder <- paste0(dir.figures, 'correlation/')
  #if (!file.exists(out.folder)) {
  #  cat("[INFO] Creating folder: ", out.folder, "\n")
  #  dir.create(out.folder, recursive = T)
  #}  
  
  #ofile <-  paste0(out.folder, "CORRELATIONS", '.png')
  #cat("[SAVE] ", ofile, "\n")
  #png(paste0(out.folder, "CORRELATIONS", '.png'), width = 480, height = 480, units = "px")
  corrplot(M, method = "ellipse",order = "hclust")
  #dev.off()

  
  return(0)
}


CorrelationGraph_2 <- function(dt.train){
  corr <- cor(dt.train)
  library(corrplot)
  M <- cor(dt.train)
  return(corrplot(M, method="circle"))
}

CorrelationGraph_3 <- function(dt.train, tam= 1.5){
  
  library(dplyr)
  library(reshape2)
  d_cor <- as.matrix(cor(dt.train))
  d_cor_melt         <- arrange(melt(d_cor), -abs(value))
  d_cor_melt         <- as.data.table(d_cor_melt)
  
  d <- ggplot(data = d_cor_melt, aes(x=reorder(Var1, abs(value)), y=reorder(Var2, abs(value)), fill=value)) 
  d <- d + geom_tile() + xlab('') + ylab('')
  d <- d + scale_fill_gradient2(low = "orange", high = "blue", mid = "white", 
                                midpoint = 0, limit = c(-1,1), space = "Lab", 
                                name="Pearson\nCorrelation") 
  d <- d + geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) 
  #d <- d + geom_tile(aes(fill = value), color='white') 
  d <- d + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  d <- d + theme(axis.title.y = element_text(size = rel(tam)))
  d <- d + theme(axis.text.y = element_text(size = rel(tam)))
  d <- d + theme(axis.text.x = element_text(size = rel(tam)))
  d <- d + theme(axis.title.x = element_text(size = rel(tam)))
  d <- d + theme(legend.text = element_text(size = tam*10))
  d <- d + theme(legend.title = element_text(size = tam*10))
  
  return(d)
}


