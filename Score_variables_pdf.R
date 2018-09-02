  library(ggplot2)
  library(gridExtra)
  library(ggpubr)
  library(cowplot)
  
  #recall column with "target",take universall name(target can be 'target_for_calc','target_for_calc_30' and so on) 
  names(data)[grep("target", names(data))]<-"target"
  
  #generate "data" with first column "target" and another columns are sorted alphabetically
  data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))
  
  #recall first column,cauth automatically programe calls it "data$target"
  names(data)[1]<-"target"
  
  Total<-length(data$target)
  Good<-sum(data$target)
  Bad<-Total-Good
    pdf("var_plot115.pdf",width = 12,height=15,paper='special')
    for (j in 2:ncol(data)) {
      plot1_hist<-ggplot(data, aes(data[,j])) + 
        geom_bar(aes(y = (..count..)/sum(..count..))) +
        scale_y_continuous(labels=scales::percent)+ 
        geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
        theme(axis.text.x = element_text(angle=10, vjust=0.9),
              plot.margin = unit(c(1,1,1,1), "cm") ) + 
        labs( y = "", x = "")
      
      aggregate_table<-aggregate(. ~ data[,j], data = data[c(names(data)[1],names(data)[j])],
                                 FUN = function(x) c(good = sum(x),
                                                     bad=length(x)-sum(x),
                                                     total = length(x),
                                                     good2=  round((sum(x)*100)/Good,2),
                                                     bad2=round((length(x)-sum(x))*100/Bad,2),
                                                     total2=round((length(x)*100)/Total,2),
                                                     BR=round((length(x)-sum(x))*100/length(x),2),
                                                     WOE=round(log((sum(x)/Good)/((length(x)-sum(x))/Bad)),4)))[,c(1,2)]
      
      #log(x) will produce NaN any time x is less than zero(calculating 'length(x)-sum(x)' we have '-' func 'log' see that and returns error
      aggregate_table<-cbind(aggregate_table[,1],data.frame(aggregate_table[,2]))
      names(aggregate_table)<-c(names(data)[j],"good, #","bad, #","total, #","good, %","bad, %","total, %","BR, %","WOE")
      
      plot2_line<-ggplot(aggregate_table, aes(x=aggregate_table[,1],y=aggregate_table[,8], group = 1))+
        geom_line(color="dodgerblue4",size=1)+
        geom_point(colour = "black", size = 2) +
        theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
        geom_text(aes(label=aggregate_table[,8]),hjust=0, vjust=0)+
        labs( y = "", x = "") +
        ylim(c(0, 30))
      
      IV=round(sum((aggregate_table[,5]-aggregate_table[,6])/100*aggregate_table[,9]),4)
      
      table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme("mBlue"))
      
      
      
      text <- paste("
                    ",names(data)[j],"     ","IV =",IV, sep = " ")
      title <- ggparagraph(text = text, face = "italic", size = 25, color = "black")
      print(ggarrange(title,plot1_hist, plot2_line, table , 
                      ncol = 1, nrow = 4,heights = c(0.055,0.3, 0.2, 0.2)))
      
    }
    
    dev.off()
