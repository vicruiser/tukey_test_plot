
  library(ggplot2)
   library(grid)
   library(fBasics)
   library(tidyr)
   library(reshape)
   library(agricolae)
   library(Rmisc)
   library(gdata)
   library(rlist)

 load("myFunction.Rdata")
 
 data<- read.csv(file.choose()  , header=T, sep=";")
 significance<-tukey.plot.significance(data)
 tukey_letters<-significance$tukey_letters
 stats<-significance$stats

 

offset<-7
offset_asterisk<-10 
  
plot<- ggplot( stats, aes(x = variable, y = value, fill = group)) + 
         geom_bar(stat = "identity", position =position_dodge(),colour="black",width=.7,size=.5)+ 
         geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1,size=.5,position=position_dodge(.7))+
         geom_line(size=.8)+
         theme(
               plot.title = element_text(lineheight=.8, face="bold",size=10),
               axis.title = element_text(size =8, face="bold"),
               axis.text = element_text(angle=45, vjust=1,size=8,face="bold"),
               axis.ticks = element_line(size = rel(2.5)),
               axis.ticks.length = unit(0.5, "cm")
               )+
         labs(
              title="your title",
              x="x axis title",
              y="y axis title"
              )+
         scale_x_discrete(
                          limit = c("names of x variables"),
                          labels = c("new names of x variables")
                          )+
         geom_text(data =tukey_letters, 
                     aes(x=xpos.t, y=ymax+offset_asterisk,label=M), 
                     size = 5,position=position_dodge(.5)
                    )+
         facet_wrap(~group) ## You can use face_wrap function only if you need it
