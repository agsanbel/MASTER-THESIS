pca_analysis <- function(samp){
  
  require(factoextra)
  
  pca.met <- prcomp(samp)
  #plot the explained variance
  p <- fviz_screeplot(pca.met, addlabels = TRUE, main = "Percentage of variance explained for each PC")
  plot(p)
  #plot contribution for each PC
  p <- fviz_contrib(pca.met, choice = "var", axes = 1, top = 1000)
  plot(p)
  p <- fviz_contrib(pca.met, choice = "var", axes = 2, top = 1000)
  plot(p)
  p <- fviz_contrib(pca.met, choice = "var", axes = 3, top = 1000)
  plot(p)
  p <- fviz_contrib(pca.met, choice = "var", axes = 4, top = 1000)
  plot(p)
  #plot PC1 vs PC2
  plot(pca.met$x[,1], pca.met$x[,2], main = 'PC1 vs PC2', xlab = 'PC1', ylab = 'PC2')
  
  PVE <- 100*pca.met$sdev^2/sum(pca.met$sdev^2)
  par(mfrow = c(1,2))
  
  #plot variance explained
  plot(PVE, type = "o", 
       ylab = "PVE", 
       xlab = "Principal component", 
       col = "blue", 
       xlim = c(0,dim(pca.met$x)[2]))
  #plot the acumulative explained variance
  plot(cumsum(PVE), type = "o", 
       ylab = "PVE accumulated", 
       xlab = "Principal component", 
       col = "brown3",
       xlim = c(0,dim(pca.met$x)[2]))
  #eigenvectors and eigenvalues
  p <- fviz_pca(pca.met)
  plot(p)
  p <- fviz_pca_var(pca.met,
               col.var = "contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE,
               label = 'all',
               title = "Variables"
  )
  plot(p)
}