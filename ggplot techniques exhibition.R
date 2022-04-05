library(ggplot2)
library(dplyr)
attach(mpg)
library(ggpubr)
library(viridis)
theme_set(theme_pubr())
names(mpg)

#-----
# Basic use of ggplot to Produce Plots of Data
#-----

#Scatter plots and trend lines. 
ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(size=3)+
  theme_minimal()+
  labs(title = "Engine Displacement vs Highway mpg")+
  xlab(label = "Engine Displacement")+
  ylab(label = "Highway mpg")
#Scatter plot showing engine displacement (displ) vs highway miles per gallon (hwy).

ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(size=3, position = "jitter")
  theme_minimal()+
  labs(title = "Engine Displacement vs Highway mpg")+
  xlab(label = "Engine Displacement")+
  ylab(label = "Highway mpg")
#jittering the points so that all data entries can be plotted and minimizing overlap.


ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(size=3)+
  theme_minimal()+
  labs(title = "Engine Displacement vs Highway mpg")+
  xlab(label = "Engine Displacement")+
  ylab(label = "Highway mpg")+
  geom_smooth(se=F)

#Scatter plot accompanied by a smooth trend line. 

ggplot(data = mpg)+
  geom_point(size=3, mapping = aes(x = displ, y = hwy, colour = drv))+
  theme_minimal()+
  labs(title = "Engine Displacement vs Highway mpg")+
  xlab(label = "Engine Displacement")+
  ylab(label = "Highway mpg")+
  geom_smooth(mapping = aes(x = displ, y= hwy), se=F)
#The same thing, but with the scatter plots colour coded by the drive train (drv).

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv))+
  geom_point(size=3)+
  theme_minimal()+
  labs(title = "Engine Displacement vs Highway mpg")+
  xlab(label = "Engine Displacement")+
  ylab(label = "Highway mpg")+
  geom_smooth(se=F)
#This time both the trend line and the points on the scatter plot are colour-coded to the drv.

ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(mapping = aes(colour=drv), size=3)+
  theme_minimal()+
  labs(title = "Engine Displacement vs Highway mpg")+
  xlab(label = "Engine Displacement")+
  ylab(label = "Highway mpg")+
  geom_smooth(mapping = aes(linetype=drv), se=F)
#This time on the geom_smooth the drv is reflected by the line type and not by colour-coding.




#Box Plots

ggplot(data = mpg, mapping = aes(x=hwy, y = class))+
  geom_boxplot()+
  labs(title = "Boxplots of Highway mpg for Each Class of Vehicle")+
  xlab(label = "Highway mpg")+
  ylab(label = "Class of Vehicle")
#Box plots showing the highway miles per gallon (hwy) for each (class) of vehicle.



#Bar Charts

ggplot(data = mpg)+ 
  labs(title = "Bar Charts of The Amount of Vehicles in each Class", fill = "Class of Vehicle")+
  xlab(label = "Class of Vehicle")+
  ylab(label = "n")+
  geom_bar(mapping = aes(x = class, fill = class))+
  coord_flip()
#Looking at the number of cars that are in each class.

ggplot(data = mpg)+ 
  labs(title = "Bar Charts of The Amount of Vehicles in each Class \n Broken Down into Drive Trains", fill = "Drive Train")+
  xlab(label = "Class of Vehicle")+
  ylab(label = "n")+  geom_bar(mapping = aes(x = class, fill = drv))+
  coord_flip()
#Looking at the number of cars in each class but breaking each down into drv type. 


ggplot(data = mpg)+ 
  labs(title = "Bar Charts of The Amount of Vehicles in each Class \n Broken Down into Separate Bars for Drive Train", fill = "Class of Vehicle")+
  xlab(label = "Class of Vehicle")+
  ylab(label = "n")+
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
  labs(x = NULL, y = NULL)+
  
Bar_Chart <- bar + coord_flip()
Pie_Chart <- bar+coord_polar()

Fig1 <- ggarrange(Bar_Chart, Pie_Chart,
                  labels = c("Bar Chart", "Pie Chart"),
                  ncol = 2, nrow = 1)
Fig1  
#A bar chart and a pie chart showing the breakdowns of different drv types by manufacturer.







#-----
# Plots Using Aggregation
#-----

AVG_hwy <- mpg %>% 
  group_by(manufacturer) %>% 
  summarise(
    average_highway_mpg = mean(hwy, na.rm = T),
    n = n()
  )
View(AVG_hwy)

ggplot(data = AVG_hwy, mapping = aes(x = manufacturer, y = average_highway_mpg))+
  geom_col(mapping = aes(fill = manufacturer), width = 0.5, show.legend = F)+
  labs(title = "Bar Chart Showing the Average Highway mpg of each Manufacturer")+
  xlab(label = "Manufacturer")+
  ylab(label = "Average Highway mpg")+
  scale_fill_viridis_d()+
  coord_flip()


ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

#Plotting hwy and displ variables to see if there are any outliers present within the data. 


x_bar <- mean(mpg$hwy)
#Finding sample mean for the distribution of hwy.
s_hwy <- sd(mpg$hwy)
#Finding the sample standard deviation of hwy.
se_hwy <- qnorm(0.975)*s_hwy/sqrt(length(mpg$hwy))
#finding the accompanying standard error (assuming Normality)
upper <- x_bar + se_hwy
#upper 95% confidence interval
lower <- x_bar - se_hwy
#lower 95% confidence interval


#Eliminating values +/- 3sds from mean

limit_upper <- x_bar + 3*s_hwy
limit_upper <- as.numeric(limit_upper)
limit_lower <- x_bar - 3*s_hwy
limit_lower <- as.numeric(limit_lower)
#setting the upper and lower cutoff limits of 3 standard deviations away from the mean.

mpg$hwy <- as.factor(mpg$hwy)
mpg$hwy <- as.numeric(mpg$hwy)
trimmed_mpg <- filter(mpg, hwy > limit_lower, hwy < limit_upper)
#trimming the data, removing the datapoints that fall outisde the outlier values.

ggplot(data = trimmed_mpg, mapping = aes(x = displ, y = hwy))+ 
  geom_point()+
  geom_smooth(method = "lm", se = F)
#Plotting the new data. 


