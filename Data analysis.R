#######################################################################################Assign a value to the stability of production and consumption in each country
####################################################################
#########################################################################################Harmonize the scores calculated by the median so that consumption and production can be compared
#######################################################################################Assign a value to the stability of production and consumption in each country
###Loading data
setwd("XX")
data<- read.csv("All stability indices-two periods.csv")
head(data)

# The median of the slope
positive_slope_median <- 0.013948
negative_slope_median <- -0.01054
# The median stability
stability_median <- 2.879738

####################################################################Consumption stability
# Add a scoring column and assign a score
data$Consumption.Score <- ifelse(data$Con.slope < 0,
                                 ifelse(data$Con.slope < negative_slope_median & data$Con.stability > stability_median, -4,
                                        ifelse(data$Con.slope < negative_slope_median & data$Con.stability < stability_median, -3,
                                               ifelse(data$Con.slope > negative_slope_median & data$Con.stability > stability_median, -2, -1))),
                                 ifelse(data$Con.slope < positive_slope_median & data$Con.stability < stability_median, 1,
                                        ifelse(data$Con.slope < positive_slope_median & data$Con.stability > stability_median, 2,
                                               ifelse(data$Con.slope > positive_slope_median & data$Con.stability < stability_median, 3, 4))))
# Output result
print(data)

####################################################################Production stability
# Add a scoring column and assign a score
data$Production.Score <- ifelse(data$Pro.slope < 0,
                                ifelse(data$Pro.slope < negative_slope_median & data$Pro.stability > stability_median, -4,
                                       ifelse(data$Pro.slope < negative_slope_median & data$Pro.stability < stability_median, -3,
                                              ifelse(data$Pro.slope > negative_slope_median & data$Pro.stability > stability_median, -2, -1))),
                                ifelse(data$Pro.slope < positive_slope_median & data$Pro.stability < stability_median, 1,
                                       ifelse(data$Pro.slope < positive_slope_median & data$Pro.stability > stability_median, 2,
                                              ifelse(data$Pro.slope > positive_slope_median & data$Pro.stability < stability_median, 3, 4))))
# Output result
print(data)
write.csv(data, "All stability indices-two periods-Score.csv", row.names = FALSE)   




#######################################################################################Assign a value to the stability of farming and fishing in each country

###Loading data
setwd("XX")
data<- read.csv("Aqua and cap stability indices-1991-2019.csv")
head(data)

# The median of the slope
positive_slope_median <- 0.013948
negative_slope_median <- -0.01054
# The median stability
Stability_median <- 2.879738

# Add a scoring column and assign a score
data$Score <- ifelse(data$Slope < 0,
                                 ifelse(data$Slope < negative_slope_median & data$Stability > Stability_median, -4,
                                        ifelse(data$Slope < negative_slope_median & data$Stability < Stability_median, -3,
                                               ifelse(data$Slope > negative_slope_median & data$Stability > Stability_median, -2, -1))),
                                 ifelse(data$Slope < positive_slope_median & data$Stability < Stability_median, 1,
                                        ifelse(data$Slope < positive_slope_median & data$Stability > Stability_median, 2,
                                               ifelse(data$Slope > positive_slope_median & data$Stability < Stability_median, 3, 4))))
# Output result
print(data)
write.csv(data, "All stability indices-Score-auqa+cap.csv", row.names = FALSE)         







#############################################################################################Production and consumption trends of the two periods are plotted globally
###Loading data
setwd("XX")
data<- read.csv("All stability indices-two periods-Score-map.csv")
head(data)

##Select data#################################################################################1961-1990
library(ggplot2)
library(dplyr)
df1<-data%>%filter(Period==unique(Period)[1])
head(df1)
## merge the two datasets by Country
world <- map_data("world")
worldSubset <- left_join(world, df1, by = "region")
head(worldSubset)
write.csv(worldSubset, "global country data.csv", row.names = FALSE)   
## Let's ditch many of the unnecessary elements
plain <- theme(axis.text = element_blank(),
               axis.line = element_blank(),
               axis.ticks = element_blank(),
               panel.border = element_blank(),
               panel.grid = element_blank(),
               axis.title = element_blank(),
               panel.background = element_rect(fill = "white"),
               plot.title = element_text(hjust = 0.5),
               legend.position = c(0.12,0.34),
               legend.background = element_blank(),
               legend.key = element_blank(),
               legend.title = element_text(size=18))
##Per capita consumption trend index
library(viridis)
library(ggpubr)
library(cowplot)
library(ggrepel)
color_palette <- c("#387E46","#6FB568","#88CA7C","#B0DEA5","#DEF1D8","#FFE5E0","#FFC3B8","#FCA192","#F77F70","#C83342")
legend_labels <- c("4", "3", "2", "1", "0.5", "-0.5","-1", "-2", "-3", "-4")
#Per capita consumption trend
worldSubset$Consumption.Score <- as.factor(worldSubset$Consumption.Score)
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group, fill = Consumption.Score)) + 
  scale_fill_manual(values = color_palette, breaks = legend_labels, labels = legend_labels,na.value = "grey80",
                    guide = guide_legend(label.theme = element_text(size = 18)))+
  geom_polygon(color = "black", linewidth = 0.25) +
  coord_fixed(1.1) +
  theme(plot.title = element_text(size = 22)) +
  labs(fill = "(1961-1990)") +
  plain+ggtitle("AFCS")
worldTL

# Count the number of countries scored for each color
score_counts <- table(df1$Count.Consumption.Score)
score_counts
# Create a data box with scores and quantities
pie_data <- data.frame(
  Score = as.numeric(names(score_counts)),
  Count = as.numeric(score_counts)
)
# Create a data box that contains the score and the corresponding color. specify the color vector as needed
score_color <- data.frame(
  Score = c(4, 3, 2, 1, 0.5, -0.5,-1, -2, -3, -4),
  Color = c("#387E46","#6FB568","#88CA7C","#B0DEA5","#DEF1D8","#FFE5E0","#FFC3B8","#FCA192","#F77F70","#C83342"))
# Merge pie chart data boxes and color data boxes
pie_data <- merge(pie_data, score_color, by = "Score", all.x = TRUE)
# Calculated percentage
pie_data$Percentage <- round(pie_data$Count / sum(pie_data$Count) * 100, 1)

# Pie chart
pie_chart <- ggplot(data = pie_data, aes(x = "", y = Percentage, fill = factor(Score))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y",start=180) +
  guides(fill = "none") +
  theme_void()+ 
  geom_text_repel(aes(x=1.4,label = paste0(Percentage, "%")), size = 4.3, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = pie_data$Color)
pie_chart

# Insert the pie chart into the map graph
combined_plot <- ggdraw() +
  draw_plot(worldTL, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(pie_chart, x = 0.07, y = 0.13, width = 0.3, height = 0.3)
# Display composition chart
combined_plot
ggsave("Per capita consumption trend map 1961-1990.png",width =30, height =16, units = "cm")

#Per capita production trend
worldSubset$Production.Score <- as.factor(worldSubset$Production.Score)
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group,fill=Production.Score)) + 
  scale_fill_manual(values = color_palette, breaks = legend_labels, labels = legend_labels,na.value = "grey80",
                    guide = guide_legend(label.theme = element_text(size = 18)))+
  geom_polygon(color="black",linewidth=0.25)+
  coord_fixed(1.1) +
  theme(plot.title = element_text(size=22))+
  labs(fill="(1961-1990)")+
  plain+ggtitle("AFPS")
worldTL

# Count the number of countries scored for each color
score_counts <- table(df1$Count.Production.Score)
score_counts
# Create a data box with scores and quantities
pie_data <- data.frame(
  Score = as.numeric(names(score_counts)),
  Count = as.numeric(score_counts)
)
# Create a data box that contains the score and the corresponding color. specify the color vector as needed
score_color <- data.frame(
  Score = c(4, 3, 2, 1, 0.5, -0.5,-1, -2, -3, -4),
  Color = c("#387E46","#6FB568","#88CA7C","#B0DEA5","#DEF1D8","#FFE5E0","#FFC3B8","#FCA192","#F77F70","#C83342"))
# Merge pie chart data boxes and color data boxes
pie_data <- merge(pie_data, score_color, by = "Score", all.x = TRUE)
# Calculated percentage
pie_data$Percentage <- round(pie_data$Count / sum(pie_data$Count) * 100, 1)
# Pie chart
pie_chart <- ggplot(data = pie_data, aes(x = "", y = Percentage, fill = factor(Score))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y",start=180) +
  guides(fill = "none") +
  theme_void()+ 
  geom_text_repel(aes(x=1.4,label = paste0(Percentage, "%")), size = 4.3, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = pie_data$Color)
pie_chart

# Insert the pie chart into the map graph
combined_plot <- ggdraw() +
  draw_plot(worldTL, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(pie_chart, x = 0.07, y = 0.13, width = 0.3, height = 0.3)
# Display composition chart
combined_plot
ggsave("Per capita production trend map 1961-1990.png",width =30, height =16, units = "cm")

##Total score
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group,fill=Total.score)) + 
  scale_fill_distiller(limits=c(-8,8), palette ="RdYlBu", direction =-1,
                       na.value = "grey80",guide = guide_colourbar(direction = "vertical",barwidth =2, barheight =8,frame.colour = "black",frame.linewidth=0.6,ticks.colour = "black",
                                                                   ticks.linewidth=0.6,label.theme = element_text(size=13))) +# or direction=1
  geom_polygon(color="black",linewidth=0.25)+
  coord_fixed(1.1) +
  theme(plot.title = element_text(size=22))+
  labs(fill="(1961-1990)")+
  plain+ggtitle("Sum of AFPS and AFCS")
worldTL
ggsave("Integrated trend of production and consumption map 1961-1990.png",width =30, height =16, units = "cm")

##Difference between consumption and production
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group,fill=Difference.con.pro)) + 
  scale_fill_distiller(limits=c(-8,8), palette ="RdBu", direction =-1,
                       na.value = "grey80",guide = guide_colourbar(direction = "vertical",barwidth =2, barheight =8,frame.colour = "black",frame.linewidth=0.6,ticks.colour = "black",
                                                                   ticks.linewidth=0.6,label.theme = element_text(size=18))) +# or direction=1
  geom_polygon(color="black",linewidth=0.25)+
  coord_fixed(1.1) +
  theme(plot.title = element_text(size=22))+
  labs(fill="(1961-1990)")+
  plain+ggtitle("Difference between AFCS and AFPS")
worldTL
ggsave("Difference between consumption and production map 1961-1990.png",width =30, height =16, units = "cm")





##Select data#####################################################################################1991-2019
df2<-data%>%filter(Period==unique(Period)[2])
head(df2)
worldSubset <- left_join(world, df2, by = "region")
head(worldSubset)
##Per capita consumption trend index
library(viridis)
color_palette <- c("#387E46","#6FB568","#88CA7C","#B0DEA5","#DEF1D8","#FFE5E0","#FFC3B8","#FCA192","#F77F70","#C83342")
legend_labels <- c("4", "3", "2", "1", "0.5", "-0.5","-1", "-2", "-3", "-4")
#Per capita consumption trend
worldSubset$Consumption.Score <- as.factor(worldSubset$Consumption.Score)
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group, fill = Consumption.Score)) + 
  scale_fill_manual(values = color_palette, breaks = legend_labels, labels = legend_labels,na.value = "grey80",
                    guide = guide_legend(label.theme = element_text(size = 18)))+
  geom_polygon(color = "black", linewidth = 0.25) +
  coord_fixed(1.1) +
  theme(plot.title = element_text(size = 22)) +
  labs(fill = "(1991-2019)") +
  plain+ggtitle("AFCS")
worldTL

# Count the number of countries scored for each color
score_counts <- table(df2$Count.Consumption.Score)
score_counts
# Create a data box with scores and quantities
pie_data <- data.frame(
  Score = as.numeric(names(score_counts)),
  Count = as.numeric(score_counts)
)
#Create a data box that contains the score and the corresponding color. specify the color vector as needed
score_color <- data.frame(
  Score = c(4, 3, 2, 1, 0.5, -0.5,-1, -2, -3, -4),
  Color = c("#387E46","#6FB568","#88CA7C","#B0DEA5","#DEF1D8","#FFE5E0","#FFC3B8","#FCA192","#F77F70","#C83342"))
# Merge pie chart data boxes and color data boxes
pie_data <- merge(pie_data, score_color, by = "Score", all.x = TRUE)
# Calculated percentage
pie_data$Percentage <- round(pie_data$Count / sum(pie_data$Count) * 100, 1)
# Pie chart
pie_chart <- ggplot(data = pie_data, aes(x = "", y = Percentage, fill = factor(Score))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y",start=180) +
  guides(fill = "none") +
  theme_void()+ 
  geom_text_repel(aes(x=1.4,label = paste0(Percentage, "%")), size = 4.3, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = pie_data$Color)
pie_chart

# Insert the pie chart into the map graph
combined_plot <- ggdraw() +
  draw_plot(worldTL, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(pie_chart, x = 0.07, y = 0.13, width = 0.3, height = 0.3)
# Display composition chart
combined_plot
ggsave("Per capita consumption trend map 1991-2019.png",width =30, height =16, units = "cm")

#Per capita production trend
worldSubset$Production.Score <- as.factor(worldSubset$Production.Score)
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group,fill=Production.Score)) + 
  scale_fill_manual(values = color_palette, breaks = legend_labels, labels = legend_labels,na.value = "grey80",
                    guide = guide_legend(label.theme = element_text(size = 18)))+
  geom_polygon(color="black",linewidth=0.25)+
  coord_fixed(1.1) +
  theme(plot.title = element_text(size=22))+
  labs(fill="(1991-2019)")+
  plain+ggtitle("AFPS")
worldTL

# Count the number of countries scored for each color
score_counts <- table(df2$Count.Production.Score)
score_counts
# Create a data box with scores and quantities
pie_data <- data.frame(
  Score = as.numeric(names(score_counts)),
  Count = as.numeric(score_counts)
)
# Create a data box that contains the score and the corresponding color. specify the color vector as needed
score_color <- data.frame(
  Score = c(4, 3, 2, 1, 0.5, -0.5,-1, -2, -3, -4),
  Color = c("#387E46","#6FB568","#88CA7C","#B0DEA5","#DEF1D8","#FFE5E0","#FFC3B8","#FCA192","#F77F70","#C83342"))
# Merge pie chart data boxes and color data boxes
pie_data <- merge(pie_data, score_color, by = "Score", all.x = TRUE)
# Calculated percentage
pie_data$Percentage <- round(pie_data$Count / sum(pie_data$Count) * 100, 1)
# Pie chart
pie_chart <- ggplot(data = pie_data, aes(x = "", y = Percentage, fill = factor(Score))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y",start=180) +
  guides(fill = "none") +
  theme_void()+ 
  geom_text_repel(aes(x=1.4,label = paste0(Percentage, "%")), size = 4.3, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = pie_data$Color)
pie_chart

# Insert the pie chart into the map graph
combined_plot <- ggdraw() +
  draw_plot(worldTL, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(pie_chart, x = 0.07, y = 0.13, width = 0.3, height = 0.3)
# Display composition chart
combined_plot
ggsave("Per capita production trend map 1991-2019.png",width =30, height =16, units = "cm")


##Total score
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group,fill=Total.score)) + 
  scale_fill_distiller(limits=c(-8,8), palette ="RdYlBu", direction =-1,
                       na.value = "grey80",guide = guide_colourbar(direction = "vertical",barwidth =2, barheight =8,frame.colour = "black",frame.linewidth=0.6,ticks.colour = "black",
                                                                   ticks.linewidth=0.6,label.theme = element_text(size=13))) +# or direction=1
  geom_polygon(color="black",linewidth=0.25)+
  coord_fixed(1.1) +
  theme(plot.title = element_text(size=22))+
  labs(fill="(1991-2019)")+
  plain+ggtitle("Sum of AFPS and AFCS")
worldTL
ggsave("Integrated trend of production and consumption map 1991-2019.png",width =30, height =16, units = "cm")

##Difference between consumption and production
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group,fill=Difference.con.pro)) + 
  scale_fill_distiller(limits=c(-8,8), palette ="RdBu", direction =-1,
                       na.value = "grey80",guide = guide_colourbar(direction = "vertical",barwidth =2, barheight =8,frame.colour = "black",frame.linewidth=0.6,ticks.colour = "black",
                                                                   ticks.linewidth=0.6,label.theme = element_text(size=18))) +# or direction=1
  geom_polygon(color="black",linewidth=0.25)+
  coord_fixed(1.1) +
  theme(plot.title = element_text(size=22))+
  labs(fill="(1991-2019)")+
  plain+ggtitle("Difference between AFCS and AFPS")
worldTL
ggsave("Difference between consumption and production map 1991-2019.png",width =30, height =16, units = "cm")





#############################################################################################The second period of aquaculture and fishing trends score global mapping
###Loading data
setwd("XX")
df1<- read.csv("All stability indices-Score-auqa+cap-map.csv")
head(df1)
##Select data###################################################################Aquaculture
library(ggplot2)
library(ggrepel)
library(dplyr)
library(viridis)
library(ggpubr)
library(cowplot)
## merge the two datasets by region
world <- map_data("world")
worldSubset <- left_join(world, df1, by = "region")
## Let's ditch many of the unnecessary elements
plain <- theme(axis.text = element_blank(),
               axis.line = element_blank(),
               axis.ticks = element_blank(),
               panel.border = element_blank(),
               panel.grid = element_blank(),
               axis.title = element_blank(),
               panel.background = element_rect(fill = "white"),
               plot.title = element_text(hjust = 0.5),
               legend.position = c(0.12,0.34),
               legend.background = element_blank(),
               legend.key = element_blank(),
               legend.title = element_text(size=14))
##Per capita Aquaculture trend index
color_palette <- c("#387E46","#6FB568","#88CA7C","#B0DEA5","#DEF1D8","#FFE5E0","#FFC3B8","#FCA192","#F77F70","#C83342")
legend_labels <- c("4", "3", "2", "1", "0.5", "-0.5","-1", "-2", "-3", "-4")
#Per capita Aquaculture production trend
worldSubset$Aqua.score <- as.factor(worldSubset$Aqua.score)
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group, fill = Aqua.score)) + 
  scale_fill_manual(values = color_palette, breaks = legend_labels, labels = legend_labels,na.value = "grey80",
                    guide = guide_legend(label.theme = element_text(size = 15)))+
  geom_polygon(color = "black", linewidth = 0.25) +
  coord_fixed(1.1) +
  theme(plot.title = element_text(size = 22)) +
  labs(fill = "(1991-2019)") +
  plain+ggtitle("Per capita aquaculture production score")
worldTL

# Count the number of countries scored for each color
score_counts <- table(df1$Count.Aqua.score)
score_counts
# Create a data box with scores and quantities
pie_data <- data.frame(
  Aqua.score = as.numeric(names(score_counts)),
  Count = as.numeric(score_counts)
)
# Create a data box that contains the score and the corresponding color. specify the color vector as needed
score_color <- data.frame(
  Aqua.score = c(4, 3, 2, 1, 0.5, -0.5,-1, -2, -3, -4),
  Color = c("#387E46","#6FB568","#88CA7C","#B0DEA5","#DEF1D8","#FFE5E0","#FFC3B8","#FCA192","#F77F70","#C83342"))
# Merge pie chart data boxes and color data boxes
pie_data <- merge(pie_data, score_color, by = "Aqua.score", all.x = TRUE)
# Calculated percentage
pie_data$Percentage <- round(pie_data$Count / sum(pie_data$Count) * 100, 1)
# Pie chart
pie_chart <- ggplot(data = pie_data, aes(x = "", y = Percentage, fill = factor(Aqua.score))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y",start=180) +
  guides(fill = "none") +
  theme_void()+ 
  geom_text_repel(aes(x=1.4,label = paste0(Percentage, "%")), size = 4.3, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = pie_data$Color)
pie_chart

# Insert the pie chart into the map graph
combined_plot <- ggdraw() +
  draw_plot(worldTL, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(pie_chart, x = 0.07, y = 0.13, width = 0.3, height = 0.3)
# Display composition chart
combined_plot
ggsave("Per capita aquaculture production trend map 1991-2019.png",width =30, height =16, units = "cm")

#Select data###################################################################Capture
#Per capita captur production trend
worldSubset$Cap.score <- as.factor(worldSubset$Cap.score)
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group, fill = Cap.score)) + 
  scale_fill_manual(values = color_palette, breaks = legend_labels, labels = legend_labels,na.value = "grey80",
                    guide = guide_legend(label.theme = element_text(size = 15)))+
  geom_polygon(color = "black", linewidth = 0.25) +
  coord_fixed(1.1) +
  theme(plot.title = element_text(size = 22)) +
  labs(fill = "(1991-2019)") +
  plain+ggtitle("Per capita capture production score")
worldTL

# Count the number of countries scored for each color
score_counts <- table(df1$Count.Cap.score)
score_counts
# Create a data box with scores and quantities
pie_data <- data.frame(
  Cap.score = as.numeric(names(score_counts)),
  Count = as.numeric(score_counts)
)
# Create a data box that contains the score and the corresponding color. specify the color vector as needed
score_color <- data.frame(
  Cap.score = c(4, 3, 2, 1, 0.5, -0.5,-1, -2, -3, -4),
  Color = c("#387E46","#6FB568","#88CA7C","#B0DEA5","#DEF1D8","#FFE5E0","#FFC3B8","#FCA192","#F77F70","#C83342"))
# Merge pie chart data boxes and color data boxes
pie_data <- merge(pie_data, score_color, by = "Cap.score", all.x = TRUE)
# Calculated percentage
pie_data$Percentage <- round(pie_data$Count / sum(pie_data$Count) * 100, 1)
# Pie chart
pie_chart <- ggplot(data = pie_data, aes(x = "", y = Percentage, fill = factor(Cap.score))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y",start=180) +
  guides(fill = "none") +
  theme_void()+ 
  geom_text_repel(aes(x=1.3,label = paste0(Percentage, "%")), size = 4, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = pie_data$Color)
pie_chart

# Insert the pie chart into the map graph
combined_plot <- ggdraw() +
  draw_plot(worldTL, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(pie_chart, x = 0.07, y = 0.13, width = 0.3, height = 0.3)
# Display composition chart
combined_plot
ggsave("Per capita capture production trend map 1991-2019.png",width =30, height =16, units = "cm")






############################################################################The difference of production trend score and consumption trend score in different periods
#Consumption and production trend difference plot
###Loading data
setwd("XX")
data<- read.csv("All stability indices-two periods-Score-map.csv")
head(data)
#plot
p1<-ggplot(data, aes(x = Continent, y = Difference.con.pro, fill = Time)) +ylim(-8,8)+
  geom_boxplot(aes(x = Continent, y = Difference.con.pro, fill = Time),alpha = 1, width = .3, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("")+theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(size =18,colour="black",angle = 30, hjust = 1))+
  theme(axis.text.y = element_text(size =18,colour="black"))+
  theme(legend.position = "")+
  theme(legend.text = element_text(size =18,colour="black"))+
  labs(x="",y="")+theme(axis.title = element_text(size = 22))+
  guides(fill=guide_legend(title=NULL))
p1

#加一个total
p2<-ggplot(data, aes(x = Time,y = Difference.con.pro, fill = Time)) +ylim(-8,8)+
  geom_boxplot(alpha = 1, width = .3)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("")+theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(size =18,colour="black",angle = 30, hjust = 1))+
  theme(axis.text.y = element_text(size =18,colour="black"))+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size =18,colour="black"))+
  labs(x="",y="")+theme(axis.title = element_text(size = 22))+
  guides(fill=guide_legend(title=NULL))
p2

#combine plots
library(patchwork)
A1 <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(2, 1))
A1
ggsave("Differnece between consumption and production in two periods.png",A1,width =28, height =15, units = "cm")

######total difference between consumption and production difference test
hist(subset(data,Time == "1961-1990")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990")$Difference.con.pro)
shapiro.test(subset(data,Time == "1991-2019")$Difference.con.pro)
test <- wilcox.test(Difference.con.pro ~ Time,data=data,paired = TRUE)
test
###Africa
hist(subset(data,Time == "1961-1990"&Continent=="Africa")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Africa")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Africa")$Difference.con.pro)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Africa")$Difference.con.pro)
a1<-subset(data,Continent=="Africa")
test <- wilcox.test(Difference.con.pro ~ Time,data=a1,paired = TRUE)
test
###Asia
hist(subset(data,Time == "1961-1990"&Continent=="Asia")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Asia")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Asia")$Difference.con.pro)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Asia")$Difference.con.pro)
a1<-subset(data,Continent=="Asia")
test <- wilcox.test(Difference.con.pro ~ Time,data=a1,paired = TRUE)
test
###Europe
hist(subset(data,Time == "1961-1990"&Continent=="Europe")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Europe")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Europe")$Difference.con.pro)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Europe")$Difference.con.pro)
a1<-subset(data,Continent=="Europe")
test <- wilcox.test(Difference.con.pro ~ Time,data=a1,paired = TRUE)
test
###North America
hist(subset(data,Time == "1961-1990"&Continent=="North America")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="North America")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="North America")$Difference.con.pro)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="North America")$Difference.con.pro)
a1<-subset(data,Continent=="North America")
test <- wilcox.test(Difference.con.pro ~ Time,data=a1,paired = TRUE)
test
###Oceania
hist(subset(data,Time == "1961-1990"&Continent=="Oceania")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Oceania")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Oceania")$Difference.con.pro)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Oceania")$Difference.con.pro)
a1<-subset(data,Continent=="Oceania")
test <- t.test(Difference.con.pro ~ Time,data=a1,paired = TRUE)
test
###South America
hist(subset(data,Time == "1961-1990"&Continent=="South America")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="South America")$Difference.con.pro,
     main = "Difference.con.pro",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="South America")$Difference.con.pro)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="South America")$Difference.con.pro)
a1<-subset(data,Continent=="South America")
test <- t.test(Difference.con.pro ~ Time,data=a1,paired = TRUE)
test







############################################################################The significance test of the change of production trend score and consumption trend score in different periods
#######################################################################Trend difference:Period2-period1
###Loading data
setwd("XX")
data<- read.csv("All stability indices-two periods-Score-map.csv")
head(data)
library(ggplot2)
##Select data##############################1961-1990
library(dplyr)
df1<-data%>%filter(Period==unique(Period)[1])
head(df1)
## merge the two datasets by region
world <- map_data("world")
worldSubset <- left_join(world, df1, by = "region")

library(viridis)
#Production trend difference
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group,fill=Difference.pro2.pro1)) + 
  scale_fill_distiller(limits=c(-8,8),palette ="RdBu", direction =-1,
                       na.value = "grey80",guide = guide_colourbar(direction = "vertical",barwidth =2, barheight =8,frame.colour = "black",frame.linewidth=0.6,ticks.colour = "black",
                                                                   ticks.linewidth=0.6,label.theme = element_text(size=18))) +# or direction=1
  geom_polygon(color="black",linewidth=0.25)+
  coord_fixed(1.1) +
  theme(plot.title = element_text(size=22))+
  labs(fill="1991-2019\n       vs\n1961-1990")+
  plain+ggtitle("Changes in AFPS")
worldTL
ggsave("Change in per capita production trend map.png",width =30, height =16, units = "cm")

#Consumption trend difference
worldTL <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group,fill=Difference.con2.con1)) + 
  scale_fill_distiller(limits=c(-8,8),palette ="RdBu", direction =-1,
                       na.value = "grey80",guide = guide_colourbar(direction = "vertical",barwidth =2, barheight =8,frame.colour = "black",frame.linewidth=0.6,ticks.colour = "black",
                                                                   ticks.linewidth=0.6,label.theme = element_text(size=18))) +# or direction=1
  geom_polygon(color="black",linewidth=0.25)+
  coord_fixed(1.1) +
  theme(plot.title = element_text(size=22))+
  labs(fill="1991-2019\n      vs\n1961-1990")+
  plain+ggtitle("Changes in AFCS")
worldTL
ggsave("Change in per capita consumption trend map.png",width =30, height =16, units = "cm")



######total production trend difference test
hist(subset(data,Time == "1961-1990")$Production.Score,
     main = "Production.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019")$Production.Score,
     main = "Production.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990")$Production.Score)
shapiro.test(subset(data,Time == "1991-2019")$Production.Score)
test <- wilcox.test(Production.Score ~ Time,data=data,paired = TRUE)
test
###Africa
hist(subset(data,Time == "1961-1990"&Continent=="Africa")$Production.Score,
     main = "Production.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Africa")$Production.Score,
     main = "Production.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Africa")$Production.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Africa")$Production.Score)
a1<-subset(data,Continent=="Africa")
test <- wilcox.test(Production.Score ~ Time,data=a1,paired = TRUE)
test
###Asia
hist(subset(data,Time == "1961-1990"&Continent=="Asia")$Production.Score,
     main = "Production.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Asia")$Production.Score,
     main = "Production.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Asia")$Production.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Asia")$Production.Score)
a1<-subset(data,Continent=="Asia")
test <- wilcox.test(Production.Score ~ Time,data=a1,paired = TRUE)
test

###Europe
hist(subset(data,Time == "1961-1990"&Continent=="Europe")$Production.Score,
     main = "Production.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Europe")$Production.Score,
     main = "Production.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Europe")$Production.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Europe")$Production.Score)
a1<-subset(data,Continent=="Europe")
test <- wilcox.test(Production.Score ~ Time,data=a1,paired = TRUE)
test
###North America
hist(subset(data,Time == "1961-1990"&Continent=="North America")$Production.Score,
     main = "Production.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="North America")$Production.Score,
     main = "Production.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="North America")$Production.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="North America")$Production.Score)
a1<-subset(data,Continent=="North America")
test <- wilcox.test(Production.Score ~ Time,data=a1,paired = TRUE)
test
###Oceania
hist(subset(data,Time == "1961-1990"&Continent=="Oceania")$Production.Score,
     main = "Production.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Oceania")$Production.Score,
     main = "Production.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Oceania")$Production.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Oceania")$Production.Score)
a1<-subset(data,Continent=="Oceania")
test <- wilcox.test(Production.Score ~ Time,data=a1,paired = TRUE)
test
###South America
hist(subset(data,Time == "1961-1990"&Continent=="South America")$Production.Score,
     main = "Production.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="South America")$Production.Score,
     main = "Production.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="South America")$Production.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="South America")$Production.Score)
a1<-subset(data,Continent=="South America")
test <- wilcox.test(Production.Score ~ Time,data=a1,paired = TRUE)
test


######total consumption trend difference test
hist(subset(data,Time == "1961-1990")$Consumption.Score,
     main = "Consumption.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019")$Consumption.Score,
     main = "Consumption.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990")$Consumption.Score)
shapiro.test(subset(data,Time == "1991-2019")$Consumption.Score)
test <- wilcox.test(Consumption.Score ~ Time,data=data,paired = TRUE)
test
###Africa
hist(subset(data,Time == "1961-1990"&Continent=="Africa")$Consumption.Score,
     main = "Consumption.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Africa")$Consumption.Score,
     main = "Consumption.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Africa")$Consumption.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Africa")$Consumption.Score)
a1<-subset(data,Continent=="Africa")
test <- wilcox.test(Consumption.Score ~ Time,data=a1,paired = TRUE)
test
###Asia
hist(subset(data,Time == "1961-1990"&Continent=="Asia")$Consumption.Score,
     main = "Consumption.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Asia")$Consumption.Score,
     main = "Consumption.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Asia")$Consumption.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Asia")$Consumption.Score)
a1<-subset(data,Continent=="Asia")
test <- wilcox.test(Consumption.Score ~ Time,data=a1,paired = TRUE)
test
###Europe
hist(subset(data,Time == "1961-1990"&Continent=="Europe")$Consumption.Score,
     main = "Consumption.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Europe")$Consumption.Score,
     main = "Consumption.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Europe")$Consumption.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Europe")$Consumption.Score)
a1<-subset(data,Continent=="Europe")
test <- wilcox.test(Consumption.Score ~ Time,data=a1,paired = TRUE)
test
###North America
hist(subset(data,Time == "1961-1990"&Continent=="North America")$Consumption.Score,
     main = "Consumption.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="North America")$Consumption.Score,
     main = "Consumption.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="North America")$Consumption.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="North America")$Consumption.Score)
a1<-subset(data,Continent=="North America")
test <- wilcox.test(Consumption.Score ~ Time,data=a1,paired = TRUE)
test
###Oceania
hist(subset(data,Time == "1961-1990"&Continent=="Oceania")$Consumption.Score,
     main = "Consumption.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="Oceania")$Consumption.Score,
     main = "Consumption.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="Oceania")$Consumption.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="Oceania")$Consumption.Score)
a1<-subset(data,Continent=="Oceania")
test <- wilcox.test(Consumption.Score ~ Time,data=a1,paired = TRUE)
test
###South America
hist(subset(data,Time == "1961-1990"&Continent=="South America")$Consumption.Score,
     main = "Consumption.Score",xlab = "1961-1990")
hist(subset(data,Time == "1991-2019"&Continent=="South America")$Consumption.Score,
     main = "Consumption.Score",xlab = "1991-2019")
shapiro.test(subset(data,Time == "1961-1990"&Continent=="South America")$Consumption.Score)
shapiro.test(subset(data,Time == "1991-2019"&Continent=="South America")$Consumption.Score)
a1<-subset(data,Continent=="South America")
test <- wilcox.test(Consumption.Score ~ Time,data=a1,paired = TRUE)
test


#production trend difference plot
p1<-ggplot(data, aes(x = Continent, y = Production.Score, fill = Time)) +ylim(-4,4)+
  geom_boxplot(aes(x = Continent, y = Production.Score, fill = Time),alpha = 1, width = .3, colour = "black")+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  ggtitle("")+theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(size =18,colour="black",angle = 30, hjust = 1))+
  theme(axis.text.y = element_text(size =18,colour="black"))+
  theme(legend.position = "")+
  theme(legend.text = element_text(size =18,colour="black"))+
  labs(x="",y="")+theme(axis.title = element_text(size = 22))+
  guides(fill=guide_legend(title=NULL))
p1
#加一个total
p2<-ggplot(data, aes(x = Time,y = Production.Score, fill = Time)) +ylim(-4,4)+
  geom_boxplot(alpha = 1, width = .3)+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  ggtitle("")+theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(size =18,colour="black",angle = 30, hjust = 1))+
  theme(axis.text.y = element_text(size =18,colour="black"))+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size =18,colour="black"))+
  labs(x="",y="")+theme(axis.title = element_text(size = 22))+
  guides(fill=guide_legend(title=NULL))
p2
#consumption trend difference plot
p3<-ggplot(data, aes(x = Continent, y = Consumption.Score, fill = Time)) +ylim(-4,4)+
  geom_boxplot(aes(x = Continent, y = Consumption.Score, fill = Time),alpha = 1, width = .3, colour = "black")+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  ggtitle("")+theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(size =18,colour="black",angle = 30, hjust = 1))+
  theme(axis.text.y = element_text(size =18,colour="black"))+
  theme(legend.position = "")+
  theme(legend.text = element_text(size =18,colour="black"))+
  labs(x="",y="")+theme(axis.title = element_text(size = 22))+
  guides(fill=guide_legend(title=NULL))
p3
#加一个total
p4<-ggplot(data, aes(x = Time,y = Consumption.Score, fill = Time)) +ylim(-4,4)+
  geom_boxplot(alpha = 1, width = .3)+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  ggtitle("")+theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(size =18,colour="black",angle = 30, hjust = 1))+
  theme(axis.text.y = element_text(size =18,colour="black"))+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size =18,colour="black"))+
  labs(x="",y="")+theme(axis.title = element_text(size = 22))+
  guides(fill=guide_legend(title=NULL))
p4
#combine plots
library(patchwork)
A1 <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(2, 1))
A2 <- p3 + p4 +
  plot_layout(ncol = 2, widths = c(2, 1))

ggsave("Production trend difference comparision.png",A1,width =28, height =15, units = "cm")
ggsave("Comsumption trend difference comparision.png",A2,width =28, height =15, units = "cm")






