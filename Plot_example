
 significance<-tukey.plot.significance(data) 
 
 tukey_letters<-significance$tukey_letters
 stats<-significance$stats
 geom.path<-significance$geom.path

 
   offset<-0.02
   offset_segment<-10
   
   p<- ggplot(stats, aes(x = variable, y = value, fill = group)) + 
   geom_bar(stat = "identity", position = "dodge", colour="black",width=.7,size=.5)+ 
   facet_wrap(~ group)+
   geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1,size=.5)+
   geom_line(size=.8)+
   theme(plot.title = element_text(lineheight=.8, face="bold"),
   strip.text.x = element_text(size = 20, face="bold"),axis.text.x = element_text(angle=45, vjust=1))+
   geom_text(data =tukey_letters, aes(x=xpos.t, y=ymax+offset, 
              label= M), size = 5)
              
## If you are interested in setting segments and asteriks just use geom_segmet() + geom_text() with information
## from significance$geom.path 
    geom_text(data = geom.path, aes(x=xmed, y=ymax+offset_segment, 
              label=sig., size = 8)+
    geom_segment(data = geom.path, size = .8, 
                 aes(x=x1, y=ymax, xend=x2, yend=ymax+offset_segment)+   ##horizontal line
    geom_segment(data = geom.path, size = .8, 
                 aes(x=x1, y=ymax, xend=x1, yend=ymax+offset_segment))+   ## vertical line
    geom_segment(data = geom.path, size = .8, 
                 aes(x=x2, y=ymax, xend=x2, yend=ymax+offset_segment))   ##vertical line          
              
  
