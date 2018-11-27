class.names<- c("Shortjaw Kokopu", "Giant Kokopu", "Banded Kokopu", "Koaro", "Inanga")
data.matrix<- wb.lda1.pred.cv
plot.title<- "Linear Discriminant Analysis \nMorphological Predictors"

c("Banded Kokopu", "koaro", "inanga")

confus.plot(data.matrix = data.matrix
            , class.names = class.names
            , plot.title = "Linear Discriminant Analysis \nMorph, Spatial and Date Predictors")
#################################################################
###begin function###----
confus.plot<- function(data.matrix = NULL
                       , class.names = NULL
                       , plot.title = NULL
                       , leg.pos = "topright"
                       , 
                       # , rev.class = FALSE
                       ){
  class.num<- length(class.names)
  predcounts<- array(data = 0, dim = c(class.num, class.num))
  colnames(predcounts) = class.names
  rownames(predcounts) = class.names
  
  rm(i)
  rm(j)
  rm(k)
  for(i in 1:class.num){
    for(j in 1:class.num){
      for(k in 1:K){
        predcounts[i,j]<- data.matrix[i,j,k] + predcounts[i,j]
      }
    }
  }
  
  predprop<- array(data = 0, dim = c(class.num, class.num))
  colnames(predprop) = class.names
  rownames(predprop) = c(paste0("prop_pred", class.names))
  
  rm(i)
  rm(j)
  rm(k)
  for(i in 1:class.num){
    for(j in 1:class.num){
      predprop[i,j]<- predcounts[i,j]/sum(predcounts[,j])
    }
  }
  
  
  #############################################
  # make the total confusion matrix----
  confus.mat<- predprop * 100
  # make the colours for the bars
  cat.col<- terrain.colors(class.num, alpha = .9)
  # miscat.col<- terrain.colors(class.num, alpha = 0.5)
  
  # create the plot frame to hang the bars on----
  # create more space to accommodate the legend
  par(mar=c(5, 7, 4, 10), xpd=TRUE)
  plot(seq(-100:100), seq(0:200), type = "n", xlim = c(-100, 100)
       , xlab = ""
       , yaxt = "n"
       , ylab = ""
       , xaxt = "n"
       , bty = "n"
       , main = ""
  )
  mtext("Sensitivity", side = 1, at = -50, line = 3, cex = 2)
  mtext("Error", side = 1, at = 50, line = 3, col = "red", cex = 2)
  
  # create the spacing for the labels----
  rm(i)
  label.space<- 200 - (200/class.num/2) 
  for(i in 1:class.num-1){
    label.space<- c(label.space, label.space[i] - 200/class.num )
  }
  bar.space<- (200/class.num)-10
  
  # create vector of bar tops----
  rm(i)
  bartop<- label.space + (bar.space/2)
  for(i in 1:class.num-1){
    bartop<- c(bartop, bartop[i] - 200/class.num )
  }
  
  # create the spacing for the bar ends----
  rm(i)
  barend<- bartop[1] - bar.space
  for(i in 1:class.num){
    print(i)
    barend<- c(barend, bartop[i+1] - bar.space )
  }
  barend<- barend[1:class.num]
  
  # add labels to the bars for each category----
  axis(2, at = label.space
       , labels = class.names
       , las = 2
       , lwd = 0 
       # , cex.axis = 0.75
       , line = -2)
  # add the number of observations for each category----
  num.obs<- as.numeric(NA)
  rm(i)
  for(i in 1:class.num){
    num.obs<- c(num.obs, sum(predcounts[, i]))
  }
  num.obs<- num.obs[-1]
  
  axis(2, at = label.space - 7
       , labels = paste("n = ", num.obs)
       , las = 2
       , lwd = 0 
       , cex.axis = .75
       , line = -2)
 
  # add the labels to the x axis 
  axis(1, at = c(0, 50, 100)
       , labels = c(0, 50, 100)
       , col.axis = "red"
  )
  axis(1, at = c(-100, -50, 0)
       , labels = c(100, 50, 0)
  )

  segments(x0=0, y0=0, y1=200, col = "lightgrey", lty = 1)
  segments(x0  = -25, y0=0, y1=200, col = "lightgrey", lty = 2)
  segments(x0  = 25, y0=0, y1=200, col = "lightgrey", lty = 2)
  segments(x0  = -50, y0=0, y1=200, col = "lightgrey", lty = 1)
  segments(x0  = 50, y0=0, y1=200, col = "lightgrey", lty = 1)
  segments(x0  = -75, y0=0, y1=200, col = "lightgrey", lty = 2)
  segments(x0  = 75, y0=0, y1=200, col = "lightgrey", lty = 2)
  segments(x0  = -100, y0=0, y1=200, col = "lightgrey", lty = 1)
  segments(x0  = 100, y0=0, y1=200, col = "lightgrey", lty = 1)
  
  # create the bars for the correctly classified species----
  rm(i)
  for (i in 1:class.num){
    polygon(x = c(-confus.mat[i,i], 0, 0, -confus.mat[i,i])
            , y = c(bartop[i], bartop[i], barend[i], barend[i]), col = cat.col[i])
    confus.mat[i,i]<- 0
  }
  
  # bars sections for true inanga with incorrect classifications
  start.x<- 0
  rm(i)
  rm(j)
  for(i in 1:class.num){
    # print(i)
    start.x<- 0
    for(j in 1:class.num){
      # print(j)
      polygon(x = c(confus.mat[j,i]+ start.x, start.x, start.x, confus.mat[j,i] + start.x)
              , y = c(bartop[i], bartop[i], barend[i], barend[i])
              , col = cat.col[j])
      start.x<- confus.mat[j,i] + start.x
      # print(start.x)
    }
  }
# put on the title----
  title(plot.title)
  
# add a legend----
  legend(leg.pos
         , title = "Species"
         , inset = c(-0.2, 0)
         , legend = class.names
         , pch = 22, pt.bg = cat.col, pt.cex = 2
         , bty = "n"
         , bg = "white")
}
