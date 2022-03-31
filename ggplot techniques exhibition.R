library(ggplot2)
library(dplyr)
attach(mpg)
library(ggpubr)
theme_set(theme_pubr())
names(mpg)

#Scatter plots and trend lines. 
ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(size=3)
#Scatter plot showing engine displacement (displ) vs highway miles per gallon (hwy).

ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(size=3, position = "jitter")
#jittering the points so that all data entries can be plotted and minimizing overlap.


ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(size=3)+
  geom_smooth(se=F)
#Scatter plot accompanied by a smooth trend line. 

ggplot(data = mpg)+
  geom_point(size=3, mapping = aes(x = displ, y = hwy, colour = drv))+
  geom_smooth(mapping = aes(x = displ, y= hwy), se=F)
#The same thing, but with the scatter plots colour coded by the drive train (drv).

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv))+
  geom_point(size=3)+
  geom_smooth(se=F)
#This time both the trend line and the points on the scatter plot are colour-coded to the drv.

ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(mapping = aes(colour=drv), size=3)+
  geom_smooth(mapping = aes(linetype=drv), se=F)
#This time on the geom_smooth the drv is reflected by the line type and not by colour-coding.




#Box Plots

ggplot(data = mpg, mapping = aes(x=hwy, y = class))+
  geom_boxplot()
#Box plots showing the highway miles per gallon (hwy) for each (class) of vehicle.



#Bar Charts

ggplot(data = mpg)+ 
  geom_bar(mapping = aes(x = class, fill = class))+
  coord_flip()
#Looking at the number of cars that are in each class.

ggplot(data = mpg)+ 
  geom_bar(mapping = aes(x = class, fill = drv))+
  coord_flip()
#Looking at the number of cars in each class but breaking each down into drv type. 


ggplot(data = mpg)+ 
  geom_bar(
    mapping = aes(x = class, fill = drv),
    position = "dodge"
           )
#Doing the same thing as above, but breaking each down into separate bars for each drv type.


#Pie Chart/Polar Coordinates.

names(mpg)
bar <- ggplot(data = mpg)+
  geom_bar(
    mapping = aes(x = manufacturer, fill = drv),
    show.legend = T,
    width = 1
  )+
  theme(aspect.ratio = 1)+
  labs(x = NULL, y = NULL)
Bar_Chart <- bar + coord_flip()
Pie_Chart <- bar+coord_polar()

Fig1 <- ggarrange(Bar_Chart, Pie_Chart,
                  labels = c("Bar", "Pie"),
                  ncol = 2, nrow = 1)
  
Fig1  
#A bar chart and a pie chart showing the breakdowns of different drv types by manufacturer.
