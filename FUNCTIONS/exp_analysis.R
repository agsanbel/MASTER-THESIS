exp_analysis <- function(data, h, b, v, sam){
  
  require(vioplot)
  if (sam == 'sample'){label = 'Samples'}
  if (sam == 'sig'){label = 'Signatures'}
  #load data
  if (data == '*.csv'){
  dat <- read.csv(data, header = T, row.names = 1, check.names = F)}
  else{dat <- data}
  #dimensions
  cat('The dimensions of the data are',dim(dat), '\n')
  #statistics
  cat('The statistics of the first 5 rows are', '\n',summary(dat[,1:5]), '\n')
  #na values
  a <- anyNA(dat)
  if (a == TRUE){
    cat('There are NA values')
    dat <- na.omit(dat)} else {cat('There are not NA values')}
  #new dimensions
  dim(dat)
  #histogram
  if (h %% 2 == 0){c <- h/2} else { c <- (h+1)/2}
  par(mfrow=c(c,2))
  for (i in 1:h){
    hist(dat[,i], xlab = colnames(dat)[i], col = '#00AFBB', main = 'Histogram')
  }
  #boxplot 
  par(mfrow=c(2,1))
  boxplot(dat[,1:b], col = "#FC4E07", main = 'Boxplot',xlab = label, ylab = 'Value', las = 2)
  #vioplot
  vioplot(dat[,1:v],names = colnames(dat[,1:v]), col="gold", las = 2, xlab = label, ylab = 'Value') 
  title("Violin Plots ")
}