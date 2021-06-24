###function design plot
box_mse <- function(data,name,tittle){
  data$Set <- factor(data$Set, levels = c('rd_train','rd_test','train','test'))
  data$Model <- factor(data$Model, levels = c('Ridge','kNN','SVR'))
  require(ggplot2)
  pdf(file = name)
  p <- ggplot(data = data, aes(x=Model, y=Values)) + geom_boxplot(aes(fill=Set)) +
    scale_fill_manual(values=c('indianred4','indianred2',"#1E90FF","#228B22", "#56B4E9")) +
    scale_y_continuous(name = 'MSE') +
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

####################################################
###############  METHILATION ######################
####################################################

### MSE SCORE ###

## SIGNATURE 1

# rd train 
r_rd_tr <- c(-0.040,-0.036,-0.040,-0.048,-0.039)
k_rd_tr <- c(-0.031,-0.031,-0.030,-0.034,-0.031)
s_rd_tr <- c(-0.032,-0.031,-0.032,-0.034,-0.031)
# rd test
r_rd_te <- c(0.037,0.057,0.052,0.039,0.033)
k_rd_te <- c(0.031,0.048,0.036,0.030,0.036)
s_rd_te <- c(0.031,0.048,0.034,0.024,0.039)
#train 
r_tr <- c(-0.040,-0.036,-0.040,-0.048,-0.039)
k_tr <- c(-0.031,-0.031,-0.030,-0.034,-0.031)
s_tr <- c(-0.032,-0.031,-0.032,-0.034,-0.031)
#test
r_te <- c(0.037,0.047,0.040,0.026,0.053)
k_te <- c(0.033,0.045,0.034,0.025,0.041)
s_te <- c(0.031,0.044,0.034,0.024,0.039)

Set <- c('rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','train','train','train','train','train','train','train','train','train','train','train','train','train','train','train','test','test','test','test','test','test','test','test','test','test','test','test','test','test','test')
Values <- abs(c(r_rd_tr,k_rd_tr,s_rd_tr,r_rd_te,k_rd_te,s_rd_te,r_tr,k_tr,s_tr,r_te,k_te,s_te))
Model <- c('Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR')

cx1 <- data.frame(Set,Model,Values)

box_mse(cx1,'CX1_MSE.pdf','CX1')

## SIGNATURE 3

# rd train 
r_rd_tr <- c(-0.018,-0.015,-0.016,-0.018,-0.015)
k_rd_tr <- c(-0.015,-0.013,-0.013,-0.014,-0.013)
s_rd_tr <- c(-0.014,-0.013,-0.013,-0.013,-0.012)
# rd test
r_rd_te <- c(0.015,0.016,0.014,0.018,0.021)
k_rd_te <- c(0.010,0.016,0.013,0.015,0.016)
s_rd_te <- c(0.009,0.014,0.013,0.014,0.016)
#train 
r_tr <- c(-0.018,-0.015,-0.016,-0.018,-0.015)
k_tr <- c(-0.015,-0.013,-0.013,-0.014,-0.013)
s_tr <- c(-0.014,-0.013,-0.013,-0.013,-0.012)
#test
r_te <- c(0.012,0.019,0.013,0.015,0.021)
k_te <- c(0.009,0.015,0.014,0.016,0.019)
s_te <- c(0.009,0.014,0.013,0.014,0.016)

Set <- c('rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','train','train','train','train','train','train','train','train','train','train','train','train','train','train','train','test','test','test','test','test','test','test','test','test','test','test','test','test','test','test')
Values <- abs(c(r_rd_tr,k_rd_tr,s_rd_tr,r_rd_te,k_rd_te,s_rd_te,r_tr,k_tr,s_tr,r_te,k_te,s_te))
Model <- c('Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR')

cx3 <- data.frame(Set,Model,Values)


box_mse(cx3,'CX3_MSE.pdf','CX3')

####################################################
###############  CNP    ############################
####################################################

### MSE SCORE ###

## SIGNATURE 1

# rd train 
r_rd_tr <- c(-0.080,-0.044,-0.054,-0.070,-0.066)
k_rd_tr <- c(-0.033,-0.026,-0.031,-0.029,-0.028)
s_rd_tr <- c(-0.027,-0.026,-0.030,-0.026,-0.026)
# rd test
r_rd_te <- c(0.056,0.068,0.150,0.101,0.063)
k_rd_te <- c(0.034,0.051,0.047,0.028,0.065)
s_rd_te <- c(0.030,0.056,0.040,0.044,0.042)
#train 
r_tr <- c(-0.080,-0.044,-0.054,-0.070,-0.066)
k_tr <- c(-0.033,-0.026,-0.031,-0.029,-0.028)
s_tr <- c(-0.027,-0.026,-0.030,-0.026,-0.026)
#test
r_te <- c(0.056,0.049,0.084,0.070,0.065)
k_te <- c(0.022,0.040,0.042,0.037,0.047)
s_te <- c(0.019,0.033,0.020,0.026,0.034)

Set <- c('rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','train','train','train','train','train','train','train','train','train','train','train','train','train','train','train','test','test','test','test','test','test','test','test','test','test','test','test','test','test','test')
Values <- abs(c(r_rd_tr,k_rd_tr,s_rd_tr,r_rd_te,k_rd_te,s_rd_te,r_tr,k_tr,s_tr,r_te,k_te,s_te))
Model <- c('Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR')

cx1 <- data.frame(Set,Model,Values)


box_mse(cx1,'CX1_MSE_CNP.pdf','CX1')

## SIGNATURE 3

# rd train 
r_rd_tr <- c(-0.080,-0.044,-0.054,-0.070,-0.066)
k_rd_tr <- c(-0.012,-0.012,-0.014,-0.012,-0.013)
s_rd_tr <- c(-0.013,-0.012,-0.015,-0.012,-0.013)
# rd test
r_rd_te <- c(0.056,0.068,0.150,0.101,0.063)
k_rd_te <- c(0.022,0.018,0.024,0.025,0.029)
s_rd_te <- c(0.016,0.018,0.008,0.018,0.012)
#train 
r_tr <- c(-0.039,-0.022,-0.033,-0.029,-0.038)
k_tr <- c(-0.012,-0.012,-0.014,-0.012,-0.013)
s_tr <- c(-0.013,-0.012,-0.015,-0.012,-0.013)
#test
r_te <- c(0.025,0.034,0.033,0.037,0.031)
k_te <- c(0.017,0.014,0.010,0.017,0.012)
s_te <- c(0.016,0.013,0.008,0.015,0.012)



Set <- c('rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_train','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','rd_test','train','train','train','train','train','train','train','train','train','train','train','train','train','train','train','test','test','test','test','test','test','test','test','test','test','test','test','test','test','test')
Values <- abs(c(r_rd_tr,k_rd_tr,s_rd_tr,r_rd_te,k_rd_te,s_rd_te,r_tr,k_tr,s_tr,r_te,k_te,s_te))
Model <- c('Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR','Ridge','Ridge','Ridge','Ridge','Ridge','kNN','kNN','kNN','kNN','kNN','SVR','SVR','SVR','SVR','SVR')

cx3 <- data.frame(Set,Model,Values)


box_mse(cx3,'CX3_MSE_CNP.pdf','CX3')
