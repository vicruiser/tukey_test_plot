  
library(ggplot2)
library(grid)
library(fBasics)
library(tidyr)
library(reshape)
library(agricolae)
library(Rmisc)
library(gdata)
library(rlist)



## Data example

##         X   X400    X80     X8 CONTROL
## 1      CA  87743 157938 184261  114066
## 2      CA  78969 166712 166712   61420
## 3      CA  52646     NA 122841   87743
## 4      CA  61420 114066 149164      NA
## 5      CA  70195     NA 140389   61420
## 6      CA     NA 140389     NA   78969
## 7  Etanol  78969 228133 166712  114066
## 8  Etanol 122841 245682 122841   61420
## 9  Etanol     NA     NA 228133   87743
## 10 Etanol  61420 166712 105292      NA
## 11 Etanol     NA 175487 157938   61420
## 12 Etanol  78969 201810     NA   78969
## 13     SA 122841 105292     NA  114066
## 14     SA 140389  78969 254456   61420
## 15     SA  96518  61420 122841   87743
## 16     SA  87743  52646     NA      NA
## 17     SA 105292 122841 184261   61420
## 18     SA     NA  78969 210584   78969


 

 tukey.plot.significance<-function(data)
  {
   ##Convert 0 values to NA values
   data[data == 0] <- NA
   data_melt<- melt(data)   
   
   stats <- summarySE(data_melt, measurevar="value", groupvars=c("X","variable"),na.rm=TRUE)
   names(stats)[1]<-"group"
          
   # ANOVA + Tukey test + statistics (mean,sd,SEM...)
   
   names<- numeric()
   names<-levels(data_melt$X)
   
   variable_list<-list()
   aov<-list()
   tukey<-list()
   tukey_groups<-list()
      
   for(i in 1:length(names))
       { 
        ##data separation acording to our experiment treatments (X)
          variable_list[[i]] <-data_melt[data_melt$X==names[i],] 
                   
          for(i in 1:length(variable_list))  
           {
            aov[[i]]<-(aov(value~variable,data=variable_list[[i]]))      
            for(i in 1:length(aov))
                {
                  tukey[[i]]<- HSD.test(aov[[i]],"variable",group=FALSE)
                  tukey_groups[[i]] <-HSD.test(aov[[i]],"variable")
                }
           } 
       }
       
   stats_data<-list()
   for(i in 1:length(variable_list))
        {
          stats_data[[i]] <- summarySE(variable_list[[i]], measurevar="value", groupvars=c("variable"),na.rm=TRUE)
        }
    
   ## Naming each level    
   names(variable_list)<-names 
   names(tukey)<-names
   names(tukey_groups)<-names
   names(aov)<-names      
   names(stats_data)<-names
  
   ##Tukey test with letter groups
   ##Loop to extract data groups, names, and x & y positions for plotting
  
   tukey_letters<-list() 
   i<-1
   j<-1
   for( i in 1:length(tukey_groups))
   {
     k<-1
     levels(tukey_groups[[i]]$groups$trt)<-rownames(tukey_groups[[i]]$means)
     tukey_letters[[i]]<-tukey_groups[[i]]$groups
     stats_data[[i]]$ymax<-stats_data[[i]]$value+stats_data[[i]]$se
     xpos.t<-numeric()
     for(j in 1:nrow(tukey_letters[[i]]))
     {
       xpos.t[k]<-as.integer(rownames(stats_data[[i]][stats_data[[i]]$variable==(as.vector(tukey_letters[[i]]$trt[j])),]))
       k<-k+1
     }
     ymax<-numeric()  
     for(m in 1:length(xpos.t))
     {
      ymax[m]<-stats_data[[i]][xpos.t[m],"ymax"]           
     }
    tukey_letters[[i]]<-cbind(tukey_letters[[i]],data.frame(xpos.t,ymax))
   }
    
    ##Naming list levels
    names(tukey_letters)<-names
   
  ##We add new column in  tukey_letters that correspond with the name of the level list. This is very important for "facets"  
  ## function in ggplot.
  
  for (i in 1:length(tukey_letters))
  {  
    group<-numeric()
    group<-rep(names(tukey_letters[i]),length.out=nrow(tukey_letters[[i]]))
    tukey_letters[[i]]<-cbind(tukey_letters[[i]],group)
  }
  
  #We eliminate the levels of tukey_letters so we can set the postion and labels of each 
  # tukey letters group automatically in our ggplot.   
 
  df_tukey_letters<-data.frame()
  for(i in 1:length(tukey_letters))
  { 
   df_tukey_letters<-rbind(df_tukey_letters,tukey_letters[[i]])
  }
 

  ### Another way to compare our data statistically with tukey test is with asterisks. 
  ## We are going to create a data frame of each experimental condition desired where 
  ## p-value and asterisks will appear and their relation with x position and y position between the
  ## two groups compared. On this way, we are going to be able to draw a segment or path connecting both bars in our bar plot 
  ## or as many as we want/have.
   
   ## First of all, we are going to display all posible combinations of data conditions
   ## in a data frame for extracting significance info in tukey test 
   v1<-as.vector(unique(data_melt$variable))
   df<-data.frame(v1)
   comb<-combn(df$v1,2)
   df.comb<-data.frame(comb[1,])
   df.comb$v2<-data.frame(comb[2,])
   df.comb$v3<-paste(comb[2,],comb[1,],sep=" - ")
  
   ## Next: extract which tukey results are significative and how many asterisks [("sig.") + p-value ("pvalue")]
   ## For this operation, row 68 function is needed. 

   i<-1
   j<-1
   k<-1
   l<-1
   m<-1
   asterisks<-list(0) 
   for(i in 1:length(tukey))
     {
       n1<-numeric()
       n2<-numeric()
       ast<-data.frame()
       group<-numeric()  
       k<-1
       m<-1
       for (j in 1:nrow(tukey[[i]]$comparison))
           {
            if(tukey[[i]]$comparison[j,]$pvalue<0.05)
             {
               strip<-numeric()
               ast<-rbind(ast,data.frame(tukey[[i]]$comparison[j,]))
               strip<-strsplit(rownames(ast[m,])," - ")
               n1[m]<-strip[[1]][[1]]
               n2[m]<-strip[[1]][[2]]  
               m<-m+1
               group[k]<-names(tukey[i])
               k<-k+1
             }
           } 
       ast<-cbind(ast,group)
       ast<-cbind(ast,n1)
       ast<-cbind(ast,n2)
       asterisks[[i]] <-ast
       names(asterisks)[l]<-names(tukey[i])
       l<-l+1
     }
     
   i<-1
   for(i in 1:length(asterisks))
   {
     if(nrow(asterisks[[i]])==0)
     {
       asterisks[[i]]<-NULL
     }
   }
  
  
  ## To manipulate the resultant list (asterisks) we're going to change the name to "path". This data will be 
  ## used for geom_segment() in ggplot 
   path<-asterisks  
   
   for(i in 1:length(path))
    {
       k<-1
       x1<-numeric()
       x2<-numeric()
       y1<-numeric()
       y2<-numeric()
       se<-numeric()
       df<-path[[i]]
       df2<-stats_data[[i]]
       for(j in 1:nrow(df))
         {
           index1<-as.vector(df[["n1"]][[j]])
           index2<-as.vector(df[["n2"]][[j]])
           x1[k]<- as.integer(rownames(df2[df2$variable==index1,]))
           x2[k]<- as.integer(rownames(df2[df2$variable==index2,]))
           y1[k]<- df2[df2$variable==index1,]$value
           y2[k]<- df2[df2$variable==index2,]$value
           se[k]<- df2[df2$variable==index1,]$se
           k<-k+1
         }
       df3<-data.frame(x1,x2,y1,y2)
       path[[i]]<-cbind(path[[i]],df3)
       xmed<-rowMeans(subset(path[[i]],select=c(x1,x2)))
       subs<-subset(path[[i]],select=c(y1,y2))
       ymax<-apply(subs[1:nrow(subs),], 1, max)
       ymax<-ymax+max(se)
       df4<-data.frame(xmed,ymax)
       path[[i]]<-cbind(path[[i]],df4)
    }
  
   #Resultant list has different data frames depending on the number of conditions. 
   # Each data frame holds the following in formation: 
   #    - sig.=asterisks 
   #    - xmed=place to set the asterirsks, in the halfway between two compared groups
   #    - ymax= max position of bar + position of erro bar, x1=x,x2=xend,y1=y,y2=yend are coordenates for geom_segment())
   #
   
   
   
   
   #Again, we eliminate the levels of tukey_letters so we can set the postion and labels of each 
   # tukey letters group automatically in our ggplot. 
    
     df_path<-data.frame()
     for(i in 1:length(path))
     { 
      df_path<-rbind(df_path,path[[i]])
     }
    
   result <- list(data_melt=data_melt,names=names,aov=aov,summary.aov=summary(aov),tukey_groups=tukey_groups,
   tukey_letters=df_tukey_letters,stats=stats,stats_groups=stats_data,combinations=df.comb,asterisks=asterisks,geom.path=df_path)
   return(result) 
  } 
     
 
 
 ##################################################################################################
 data<- read.csv(file.choose()  , header=T, sep=";") 
  significance<-tukey.plot.significance(data) 
