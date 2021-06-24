
corr_analysis <- function(sam, n){
  
  require(factoextra)
  require(corrplot)
  cor_matrix_pea <- cor(sam, method = 'pearson')
  cor_matrix_sp <- cor(sam, method = 'spearman')
  heatmap(cor_matrix_pea)
  heatmap(cor_matrix_sp)
  
  #Corrplot pearson and spearman
  corrplot(cor_matrix_pea, order = 'hclust', addrect = n)
  corrplot(cor_matrix_sp, order = 'hclust', addrect = n)
  
  # Scale variables
  datos <- scale(t(sam), center = T, scale = T) #SCALE TRUE AND CENTER TRUE
  
  # Pearson distance
  mat_dist <- get_dist(x = datos, method = "pearson")
  #round(as.matrix(mat_dist), 2)
  p <- fviz_dist(dist.obj = mat_dist, lab_size = 8, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
    theme(legend.position = "none")
  plot(p)
  
  # Euclidean distance
  mat_dist <- dist(x = datos, method = "euclidean")
  #round(as.matrix(mat_dist), 2)
  p <- fviz_dist(dist.obj = mat_dist, lab_size = 8, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
    theme(legend.position = "none")
  plot(p)
}
