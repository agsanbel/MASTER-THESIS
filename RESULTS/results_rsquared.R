###function design plot
box_design <- function(data,name,tittle,metric){
  data$Set <- factor(data$Set, levels = c('train','test'))
  data$Model <- factor(data$Model, levels = c('Ridge','kNN','SVR'))
  require(ggplot2)
  pdf(file = name)
  if (metric == 'R-squared'){lim = c(min(Values)-0.1, 1)} else {lim = c(min(Values)-0.001, max(Values)+0.001)}
  p <- ggplot(data = data, aes(x=Model, y=Values)) + geom_boxplot(aes(fill=Set)) +
    scale_fill_manual(values=c("#1E90FF","#228B22", "#56B4E9")) +
    scale_y_continuous(name = metric,limits=lim) +
    scale_x_discrete(name = "Model") +
    ggtitle(tittle) +
    theme(axis.line.x = element_line(size = 0.5, colour = "black"),
          axis.line.y = element_line(size = 0.5, colour = "black"),
          axis.line = element_line(size=1, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title=element_text(size = 20, hjust = 0.5),
          text=element_text(size = 16),
          axis.text.x=element_text(colour="black", size = 12),
          axis.text.y=element_text(colour="black", size = 12))
  plot(p)
  dev.off()
}
#####################################################
############        METHYLATION        ##############
#####################################################

### R2 SCORE ###

## SIGNATURE 1

#train 
r_tr <- c(-0.261,-0.166,-0.272,-0.455,-0.332)
k_tr <- c(0.017,0.001,0.049,-0.007,0.021)
s_tr <- c(-0.012,-0.010,-0.011,-0.015,-0.041)

#test
r_te <- c(-0.186,-0.577,-0.249,-0.158,-0.417)
k_te <- c(-0.045,-0.518,-0.060,-0.116,-0.100)
s_te <- c(-0.001,-0.493,-0.050,-0.064,-0.059)

Set <- c('train','train','train','train','train','train','train','train','train','train','train','train','train','train','train','test','test','test','test','test','test','test','test','test','test','test','test','test','test','test')
Values <- c(r_tr,k_tr,s_tr,r_te,k_te,s_te)
Model <- c('Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR')

cx1 <- data.frame(Set,Model,Values)


box_design(cx1,'CX1_Rsquared.pdf','CX1','R-squared')

## SIGNATURE 3

#train 
r_tr <- c(-0.266,-0.216,-0.253,-0.463,-0.223)
k_tr <- c(-0.033,-0.068,0.018,-0.112,-0.072)
s_tr <- c(-0.003,-0.054,-0.015,-0.048,-0.010)

#test
r_te <- c(-0.347,-0.444,-0.053,-0.087,-0.445)
k_te <- c(-0.026,-0.175,-0.037,-0.218,-0.235)
s_te <- c(-0.003,-0.054,-0.001,-0.042,-0.116)

Set <- c('train','train','train','train','train','train','train','train','train','train','train','train','train','train','train','test','test','test','test','test','test','test','test','test','test','test','test','test','test','test')
Values <- c(r_tr,k_tr,s_tr,r_te,k_te,s_te)
Model <- c('Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR')

cx3 <- data.frame(Set,Model,Values)


box_design(cx3,'CX3_Rsquared.pdf','CX3','R-squared')

#####################################################
############        CNProfiles        ##############
#####################################################

### R2 SCORE ###

## SIGNATURE 1

#train 
r_tr <- c(-1.321,-0.619,-0.629,-1.365,-1.039)
k_tr <- c(0.034,0.057,0.072,0.026,0.176)
s_tr <- c(0.193,0.070,0.118,0.146,0.211)

#test
r_te <- c(-1.674,-0.115,-0.965,-1.564,-1.322)
k_te <- c(-0.044,0.075,-0.632,-0.043,-0.849)
s_te <- c(0.098,0.246,0.238,0.262,-0.333)

Set <- c('train','train','train','train','train','train','train','train','train','train','train','train','train','train','train','test','test','test','test','test','test','test','test','test','test','test','test','test','test','test')
Values <- c(r_tr,k_tr,s_tr,r_te,k_te,s_te)
Model <- c('Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR')

cx1 <- data.frame(Set,Model,Values)


box_design(cx1,'CX1_Rsquared_CNP.pdf','CX1','R-squared')

## SIGNATURE 3

#train 
r_tr <- c(-2.192,-0.841,-1.529,-1.631,-1.989)
k_tr <- c(0.024,0.034,-0.026,0.014,0.024)
s_tr <- c(-0.032,0.058,-0.085,-0.005,-0.015)

#test
r_te <- c(-0.654,-1.389,-3.221,-1.517,-1.733)
k_te <- c(-0.156,0.043,-0.215,-0.092,-0.021)
s_te <- c(-0.057,0.077,-0.033,-0.007,-0.075)

Set <- c('train','train','train','train','train','train','train','train','train','train','train','train','train','train','train','test','test','test','test','test','test','test','test','test','test','test','test','test','test','test')
Values <- c(r_tr,k_tr,s_tr,r_te,k_te,s_te)
Model <- c('Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR')

cx3 <- data.frame(Set,Model,Values)


box_design(cx3,'CX3_Rsquared_CNP.pdf','CX3','R-squared')


