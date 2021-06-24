final <- c()
for (i in 1:5){
  a <- cbind(samples_cns[,i],rep(paste0('CX',i), length(samples_cns[,i])))
  final <- rbind(final,a)

}         
colnames(final) <- c('Value','Signature')
final <- as.data.frame(final)
final$Value <- round(as.numeric(final$Value),4)

library(ggplot2)
library(hrbrthemes)
ggplot(as.data.frame(final), aes(x=Value, group = Signature, fill = Signature), xlim=c(0,1)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

ggplot(final, aes(x=Value,group = Signature, fill = Signature)) +
  geom_density() 
