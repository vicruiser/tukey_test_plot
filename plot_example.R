###################################################################################
# Load tukey significance function
###################################################################################

load("tukeySignificanceLettersFunction.Rdata")

###################################################################################
# Load data example and apply function
###################################################################################
data_example <- read.table("./data_example.csv", sep =";", header=T)

significance<-tukey_plot_significance(data_example) 

# Extract letters positions
tukey_letters<-significance$tukey_letters
stats<-significance$stats

# Set asteriks position (so it won't fall on top of the error bars)
offset<-7
offset_asterisk<-10 
  
plot<- ggplot( tukey_letters, aes(x = variable, y = value.x, fill = L1)) + 
         geom_bar(stat = "identity", position =position_dodge(),colour="black",width=.7,size=.5)+ 
         geom_errorbar(aes(ymin=value.x-se, ymax=value.x+se), width=.1,size=.5,position=position_dodge(.7))+
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
        facet_wrap(~L1)+ ## You can use face_wrap function only if you need it+
        geom_text(data =tukey_letters, 
                     aes(x=xpos, y=ymax+offset_asterisk,label=groups), 
                     size = 5,position=position_dodge(.5)
                    )
plot
