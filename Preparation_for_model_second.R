library(woe)
library(smbinning) 
library(classInt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(lazyeval)


data <- read.csv2("cc_no_csf_else_bin.csv")

begin_ncol <- ncol(data)
for (i in c(2:begin_ncol)) 
{ 
  options("scipen"=100, "digits"=4)
  
  #equal
  eq<-classIntervals(data[,i], 5, style = 'quantile')
  colname <- paste(names(data)[i], "eq", sep="_")
  data[[colname]] <- with(data, cut(data[,i], 
                                    c(min(data[,i])-1,unique(eq$brks)),include.lowest = TRUE, right = FALSE, ordered = TRUE))
  levels(data[[colname]])<-gsub(",", ";", levels(data[[colname]]))
  
  #kmeans
  interval_count<-6
  repeat{
    km<-classIntervals(data[,i], interval_count, style = 'kmeans')
    colname <- paste(names(data)[i], "km",interval_count, sep="_")
    data[[colname]] <- with(data, cut(data[,i], km$brks,include.lowest = TRUE, right = FALSE, ordered = TRUE))
    levels(data[[colname]])<-gsub(",", ";",levels(data[[colname]]))
    interval_count<-interval_count-1
    if (min(table(data[[colname]])/nrow(data))>3) {break}
    if (interval_count<2) {break}
  }
  
  #smbinning
  sb=smbinning(data,y=names(data)[1],x=names(data)[i])
  colname <- paste(names(data)[i], "sb", sep="_")
  data[[colname]] <- with(data, cut(data[,i], c(min(data[,i])-1,unique(sb$bands)), right = TRUE, left = FALSE, ordered = TRUE))
  levels(data[[colname]])<-gsub(",", ";",levels(data[[colname]]))
  
  #save(eq, km, sb, file = paste(names(data[i]),".rda", sep = ""))
  #rm(eq)
  #rm(km)
  #rm(sb)
}

data <- data[,c(1,(begin_ncol+1):ncol(data))]

file<-data
## create loop for BR
begin_ncol<-ncol(file)
for (i in 2:begin_ncol){
  
  var_for_group <- names(file)[i]
  column_br <- paste("BR", names(file)[i] , sep="_")
  br_table <- file %>%
    select(c(i,1)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(!!column_br := (n() - sum(.))/n()))
  
  file <- left_join(file,br_table,by=names(file)[i])
}


data <- file

Total<-length(data$target_for_calc)
Good<-sum(data$target_for_calc)
Bad<-Total-Good
pdf("var_plonew9.pdf",width = 9,height=10,paper='special')
for (j in 2:begin_ncol) {
  plot1_hist<-ggplot(data, aes(data[,j])) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_y_continuous(labels=scales::percent)+ 
    geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    labs( y = "Class", x = "")
  
  plot2_line<-ggplot(data, aes(x=data[,j],y=data[,j+begin_ncol-1],group=1)) + 
    geom_line(color="dodgerblue4",size=1)+
    geom_point() +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    scale_y_continuous(limits=c(0, 0.3),breaks=c(0.05,0.1,0.15, 0.2, 0.25, 0.3), labels = function(x) paste0(x*100, "%"))+
    labs( y = "BR", x = "")
  
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(plot1_hist))
  g2 <- ggplot_gtable(ggplot_build(plot2_line))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  
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
  
  IV=round(sum((aggregate_table[,5]-aggregate_table[,6])/100*aggregate_table[,9]),4)
  
  table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme("mBlue"))
  
  
  
  text <- paste("
              ",names(data)[j],"     ","IV =",IV, sep = " ")
  title <- ggparagraph(text = text, face = "italic", size = 25, color = "black")
  print(ggarrange(title,g, table , 
                  ncol = 1, nrow = 3,heights = c(0.055,0.3, 0.3, 0.2)))
  
}
dev.off()










