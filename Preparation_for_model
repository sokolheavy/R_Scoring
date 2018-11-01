library(woe)
library(smbinning) 
library(classInt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(lazyeval)
library(corrplot)
library(gtable)
library(grid)

# set data for binning
data <- read.csv2("cc_no_csf_else_bin.csv")

## Create binning data 

# initial number of columns
begin_ncol <- ncol(data)

# binning every variable with different type of binning('equal', 'kmeans', 'smbinning')
for (i in c(2:begin_ncol)) 
{ 
  
  # equal 
  # Idea: equal number of observation in every class
  
  # create intervals
  eq<-classIntervals(data[,i], 5, style = 'quantile')
  
  # column name for new binning column, that bins with 'equal' method
  colname <- paste(names(data)[i], "eq", sep="_")
  
  # set column, that bins with 'equal' method
  data[[colname]] <- with(data, cut(data[,i], 
                                    c(min(data[,i])-1,unique(eq$brks)),include.lowest = TRUE, 
                                    right = FALSE, ordered = TRUE))
  
  # for saving ordered factors all repleacements must be done on factor levels, not on data(!!!)
  levels(data[[colname]])<-gsub(",", ";", levels(data[[colname]]))
  
  # kmeans 
  # Idea: search k 'center' points, that have biggest spreading of points around this 'center'(every interval have his own 'center' point)
  
  # k in {6,5,4,3,2}
  interval_count<-6
  repeat{
    km<-classIntervals(data[,i], interval_count, style = 'kmeans')
    colname <- paste(names(data)[i], "km",interval_count, sep="_")
    data[[colname]] <- with(data, cut(data[,i], km$brks,include.lowest = TRUE, 
                                      right = FALSE, ordered = TRUE))
    levels(data[[colname]])<-gsub(",", ";",levels(data[[colname]]))
    interval_count<-interval_count-1
    
    # any class have to be bigger than 3% of all data
    if (min(table(data[[colname]])/(nrow(data)/100)>3)) {break}
    
    # interval_count - {6,5,4,3,2}, if interval_count<2 - end
    if (interval_count<2) {break}
  }
  
  # smbinning
  # Idea: 'optamal binning' (maximization IV)
  sb=smbinning(data,y=names(data)[1],x=names(data)[i])
  colname <- paste(names(data)[i], "sb", sep="_")
  data[[colname]] <- with(data, cut(data[,i], c(min(data[,i])-1,unique(sb$bands)),
                                    right = TRUE, left = FALSE, ordered = TRUE))
  levels(data[[colname]])<-gsub(",", ";",levels(data[[colname]]))
  
  # save binnings intervals into 'rda' files
  #save(eq, km, sb, file = paste(names(data[i]),".rda", sep = ""))
  #rm(eq)
  #rm(km)
  #rm(sb)
}

## Create variable "data" only with bining data (order columns by IV(in descending order))

# select only bining data (remove column without binning)
data <- data[,c(1,(begin_ncol+1):ncol(data))]

# IV - statistic table
iv_table <- iv.mult(data,"target_for_calc",TRUE)

# Sorting columns by IV
data <- data[c("target_for_calc",iv_table[,1])]

## Create loop for BR (add columns with BR for every binning variable)

file<-data
begin_ncol<-ncol(file)
for (i in 2:begin_ncol){
  
  #create 'br_table'. It consists of 2 column("BR" + name_of_variables, BR_value)
  var_for_group <- names(file)[i]
  column_br <- paste("BR", names(file)[i] , sep="_")
  br_table <- file %>%
    select(c(i,1)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(!!column_br := (n() - sum(.))/n()))
  
  # join 'br_table' to the table with bining variables
  file <- left_join(file,br_table,by=names(file)[i])
}

## Create PDF-file with statistics for all bining variables

data <- file
Total<-length(data$target_for_calc)
Good<-sum(data$target_for_calc)
Bad<-Total-Good

pdf("Statistic_of_variables.pdf",width = 12, height=9, paper='special')

# table with IV values(descending order)
ggtexttable(iv_table, rows = NULL, theme = ttheme("lGreenWhite"))

# loop, that creates statistics for every column 
for (j in 2:begin_ncol) {
  plot1_hist <- ggplot(data, aes(data[,j])) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_y_continuous(labels=scales::percent)+ 
    geom_text(aes( y = ((..count..)/sum(..count..)),
                   label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    labs( y = "Class", x = "")
  
  plot2_BR_line <- ggplot(data, aes(x=data[,j],y=data[,j+begin_ncol-1],group=1)) + 
    geom_line(color="dodgerblue4",size=1)+
    geom_point() +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    scale_y_continuous(limits=c(0, 0.3),breaks=c(0.05,0.1,0.15, 0.2, 0.25, 0.3), 
                       labels = function(x) paste0(x*100, "%"))+
    labs( y = "BR", x = "")
  
  # union 2 graphics(plot1_hist, plot2_BR_line) in 1 
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(plot1_hist))
  g2 <- ggplot_gtable(ggplot_build(plot2_BR_line))
  
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
  
  
  # calc statistic values for every column
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
  
  # value from 'aggregate_table' sets in the ggplot object
  table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme("lGreenWhite"))
  
  # set name of variable and her 'Strength'(dependense of IV: 'Strong', Weak, 'Very weak' and etc)
  text1 <- paste0("
              ",names(data)[j],": ", iv_table$Strength[iv_table$Variable == names(data)[i]])
  
  # set style of 'text1'
  title1 <- ggparagraph(text = text1, face = "italic", size = 25, color = "black")
  
  
  text2 <- paste("                  ","IV =",IV, sep = " ")
  title2 <- ggparagraph(text = text2, face = "italic", size = 25, color = "black")
  
  # union 4 object in one file: 
  print(ggarrange(title1, title2, g, table , 
                  ncol = 1, nrow = 4,heights = c(0.055, 0.055, 0.3, 0.2)))
  
}

dev.off()

## Remove unused columns

data$Age_of_passport_month_km_4 <- NULL

## Calc_WOE

# select only bining data (remove column with 'BR')
data_woe <- select(data, -grep("BR", names(data)))

# create 'woe_table'. It consists of 2 column("WOE" + name_of_variables, WOE_value)
for (i in 2:ncol(data_woe)){
  var_for_group <- names(data_woe)[i]
  column_woe <- paste("WOE", names(data_woe)[i] , sep="_")
  woe_table <- data_woe %>%
    select(c(i,1)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(!!column_woe := log((sum(.)/Good)/((length(.)-sum(.))/Bad))))
   
  # join 'woe_table' to the table with bining variables                      
  data_woe <- left_join(data_woe,woe_table,by=names(data_woe)[i])
} 


## Create file for log_regresion: 

data_log_regr<- select(data_woe, c(1, grep("WOE", names(data_woe))))

## Create file for Corelation:

data_cor <- select(data_woe, grep("WOE", names(data_woe))) 

cor_table <- cor( data_cor[!is.infinite(rowSums(data_cor)),])

#build Correlation plot in pdf
pdf("CorrPlot.pdf",width = 25,height=25,paper='special')
corrplot(cor_table, method="number",type = "upper",tl.col = "black",diag = FALSE)
dev.off()
