## >>>>>>>>>>>>> R version 5.1

###################################################################################
# Required packages
###################################################################################
library(ggplot2)
library(grid)
library(fBasics)
library(tidyr)
library(reshape2)
library(agricolae)
library(Rmisc)
library(gdata)
library(rlist)
library(stringr)


###################################################################################
# Tukey plot significance function
###################################################################################
tukey_plot_significance<-function(data)
  {
  
  ##Convert 0 values to NA values
  data[data == 0] <- NA
  data_melted <- reshape2::melt(data)   
  colnames(data_melted)[1] = 'X'
  stats <- summarySE(data_melted, measurevar="value", groupvars=c("X","variable"), na.rm=TRUE)
  names(stats)[1]<-"group"
  
  # ANOVA + Tukey test + statistics (mean,sd,SEM...)
  variable_list <- split(data_melted, f= data_melted$X)
  aov <- lapply(variable_list, function(x) aov(value~variable, data = x ))
  stats_data <- lapply(variable_list, function(x) summarySE(x, measurevar="value", groupvars=c("variable"),na.rm=TRUE) )
  tukey <- lapply(aov, function(x) HSD.test(x,"variable",group=FALSE))
  tukey_groups <- lapply(aov, function (x) HSD.test(x,"variable"))
  
  ##Tukey test with letter groups
  ##Loop to extract data groups, names, and x & y positions for plotting
  stats_data <- lapply(stats_data, function(x) {x$ymax <-x$value + x$se;
                                                row.names(x) <- x$variable;
                                                return(x)})

  tukey_letters <- lapply(tukey_groups, function(x) x$groups)
  tukey_letters <- mapply(function(x, y) merge(x, y, by=0) , tukey_letters, stats_data, SIMPLIFY = FALSE)
  tukey_letters <- mapply(function(x, y) {y$xpos <- match( y$variable, x[,1]); return(y)}, stats_data, tukey_letters , SIMPLIFY = FALSE)

  
  ### Another way to compare our data statistically with tukey test is with asterisks. 
  ## We are going to create a data frame of each experimental condition desired where 
  ## p-value and asterisks will appear and their relation with x position and y position between the
  ## two groups compared. On this way, we are going to be able to draw a segment or path connecting both bars in our bar plot 
  ## or as many as we want/have.
  

  
  ast <- lapply(tukey, function(x) subset(x$comparison, signif. != " "))
  asterisks <- lapply(ast, function(x) cbind(x, 
                                             setNames(object= data.frame(str_split_fixed(rownames(x)," - ",2)), c("g1", "g2"))) )
   asterisks1<-asterisks
  path <- mapply( function(x, y) {x$x1 <- y[match(x$g1, y$variable), "xpos"]; return(x)},
                  asterisks,
                  tukey_letters,
                  SIMPLIFY = FALSE)
  path <- mapply( function(x, y) {x$x2 <- y[match(x$g2, y$variable), "xpos"]; return(x)},
                  path,
                  tukey_letters,
                  SIMPLIFY = FALSE)
  
  path <- mapply( function(x, y) {x$y1 <- y[match(x$g1, y$variable), "value.x"]; return(x)},
                  path,
                  tukey_letters,
                  SIMPLIFY = FALSE)
  path <- mapply( function(x, y) {x$y2 <- y[match(x$g2, y$variable), "value.x"]; return(x)},
                  path,
                  tukey_letters,
                  SIMPLIFY = FALSE)
  path <- lapply( path, function(x) {x$xmed <- as.numeric(rowMeans(x[,c("x1", "x2")])) ; return(x)})
  
  path <- mapply( function(x, y) {x$ymax <- rowMaxs(x[,c("y1","y2")])+ max(y$se); return(x)},
                  path,
                  tukey_letters,
                  SIMPLIFY = FALSE)
  df_path = melt(path, id.vars= names(path[[1]]))
  df_tukey_letters = melt(tukey_letters, id.vars= names(tukey_letters[[1]]))

  ## To manipulate the resultant list (asterisks) we're going to change the name to "path". This data will be 
  ## used for geom_segment() in ggplot 

    
    #Resultant list has different data frames depending on the number of conditions. 
    # Each data frame holds the following information: 
    #    - sig.=asterisks 
    #    - xmed=place to set the asterirsks, in the halfway between two compared groups
    #    - ymax= max position of bar + position of erro bar, x1=x,x2=xend,y1=y,y2=yend are coordenates for geom_segment())
    #

  # 
  result <- list(data_melt = data_melted,
                 aov = aov,
                 summary.aov = summary(aov),
                 tukey_letters = df_tukey_letters,
                 stats = stats,
                 stats_groups = stats_data,
                 asterisks = asterisks,
                 geom.path = df_path)
  return(result) 
} 


 
 ##################################################################################################
 
 save("tukey_plot_significance", file="tukeySignificanceLettersFunction.Rdata")
 
 
 ##################################################################################################

