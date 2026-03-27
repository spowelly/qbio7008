library('tidyverse')
library('ggplot2')
library('readxl')


install.packages("Interpol.T")

library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
# library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 


# Importing Data
#rainfall <- read_csv("./data/Rainfall.csv")
#minTemp <- read_csv("./data/MeanMin.csv")
#maxTemp <- read_csv("./data/MeanMax.csv")
climate <- read_csv("./data/allstats.csv")
climate$middate <- dmy(paste(climate$MidMonDay, climate$Month, climate$Year, sep = "/"))
climate$T_HMinDate <- dmy(paste(climate$T_HighestMinDay, climate$Month, climate$Year, sep = "/"))
climate$T_HMaxDate <- dmy(paste(climate$T_HighestMaxDay, climate$Month, climate$Year, sep = "/"))

#### Rainfall over the years by month ####

## Rainfall LINE GRAPH
rainfallgraph <- ggplot(climate, aes(Month, Rainfall, group = Year, colour = Year)) +
  geom_point() +
  geom_line(lwd = 1) +
  scale_color_viridis_b()
print(rainfallgraph)

## Rainfall BAR GRAPH
RainfallBar <- ggplot(climate, aes(x = Year, y = Rainfall, fill = Month)) +
  geom_bar(position="stack", stat = 'identity') +
  scale_fill_gradient2(
    low = '#d90711',
    mid = "blue",
    midpoint = 6,
    high = '#d90711',
    space = "Lab",
    guide = "colourbar",
    aesthetics = "fill"
  )

print(RainfallBar)



climateNew <- left_join(climate, YearlyData, by = 'middate')

#### Temperature over the years by month ####

## Min Average over the years by month
mingraph <- ggplot(climate, aes(Month, T_MeanMin, group = Year, colour = Year)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_b()
print(mingraph)

## Max Average over the years by month
maxgraph <- ggplot(climate, aes(Month, T_MeanMax, group = Year, colour = Year)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_b()
print(maxgraph)


## Temps over the months
tempsGraph <- ggplot(climate) +
  geom_point(aes(middate, T_Mean), colour = 'black') +
  geom_point(aes(middate, T_MeanMax), colour = 'red') +
  geom_point(aes(middate, T_MeanMin), colour = 'blue') +
  geom_point(aes(middate, T_HighestMax), colour = 'red', alpha = 0.2) +
  geom_point(aes(middate, T_LowestMin), colour = 'blue', alpha = 0.2) +
  geom_line(aes(middate, T_Mean, na.rm = TRUE), colour = 'black', lwd = 1) 
  
print(tempsGraph)


## Just Jan
climateJan <- climate |>
  filter(Month == 1)
tempsGraphJan <- ggplot(climateJan) +
  geom_point(aes(middate, T_Mean), colour = 'black') +
  geom_point(aes(middate, T_MeanMax), colour = 'red') +
  geom_point(aes(middate, T_MeanMin), colour = 'blue') +
  geom_point(aes(middate, T_HighestMax), colour = 'red', alpha = 0.2) +
  geom_point(aes(middate, T_LowestMin), colour = 'blue', alpha = 0.2) +
  geom_line(aes(middate, T_Mean, na.rm = TRUE), colour = 'black', lwd = 1) 

print(tempsGraphJan)


## Average Temp over the years by month

# Temp Ribbon Graph
Temp <- ggplot(climate) +
  geom_ribbon(aes(x=Month, ymin=T_MeanMin,ymax=T_MeanMax, group = Year, fill = Year), alpha=0.1) +
  scale_fill_viridis_b() +
  geom_line(aes(Month, T_Mean, group = Year, colour = Year)) +
  scale_color_viridis_b()
print(Temp)

# Temp Line Graph
AvTemp <- ggplot(climate, aes(Month, T_Mean, group = Year, colour = Year)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_b() + 
print(AvTemp)


# Bar graph of month by year - NOT GOOD
AvTempBar <- ggplot(climate, aes(x = Year, y = T_Mean, fill = Month)) +
  geom_bar(position="stack", stat = 'identity') +
  scale_fill_viridis_c()
print(AvTempBar)


### Yearly Totals ###
YearlyData<- climate |>
  group_by(Year) |>
  summarise(MeanMin = mean(T_MeanMin),
            MeanMax = mean(T_MeanMax),
            Average = mean(T_Mean),
            SumRainfall = sum(Rainfall),
            MeanRainfall = mean(Rainfall)) |>
  filter(Year != '1988')
YearlyData$middate <- dmy(paste("15","6", YearlyData$Year, sep = "/"))


YearlyData_2023 <- YearlyData |>
  filter(Year < 2024)

YearlyData_2020 <- YearlyData |>
  filter(Year < 2021)

# Average yearly temperatures
m1 <- lm(Average ~ Year, YearlyData_2020)
summary(m1)
anova(m1)
plot(Average ~ Year, YearlyData_2023)
abline(m1, col = "red")


library(esquisse)
esquisser(YearlyData_2023)

annual <- read.csv("./data/nationalaveragesSUMMARY.csv")

m2 <- lm(AverageTemp ~ Year, annual)
summary(m2)
anova(m2)
plot(Average ~ Year, YearlyData_2023)
abline(m1, col = "red")








########### GOOD PLOTS ##########
averagemodel <- lm(Average ~ Year, YearlyData_2023)

newdat <- data.frame("Year" = c(1989, 2023))
preds <- predict.lm(averagemodel, newdata = newdat)
changedeg <- preds[2]-preds[1]

changedeg

YearGraph <- ggplot(YearlyData_2023, aes(x=Year, y=Average)) +
  geom_point(size = 3) +
  geom_line(lwd = 1) +
  geom_smooth(method=lm, col = "red") +
  labs(y = 'Average Yearly Temperature (C)', x ="Year", title = "Lord Howe Island Annual Average Temperatures since 1989") +
  annotate(geom="text", x=1992, y=20.1, label="Multiple R-squared = 0.327***",
            color="black", fontface="italic") +
  annotate(geom="text", x=2018, y=19.2, label="0.592°C increase in average temperature since 1989",
           color="black") 
  
  theme_minimal()
YearGraph


# Average yearly temperatures
YearGraphRibbon <- ggplot(YearlyData) +
  geom_ribbon(aes(x=Year, ymin=MeanMin, ymax=MeanMax), alpha=0.1) +
  geom_line(aes(Year, Average))

print(YearGraphRibbon)


# Temp Ribbon Graph
Temp <- ggplot(tempInfo) +
  geom_ribbon(aes(x=Month, ymin=MeanMin,ymax=MeanMax, group = Year, , fill = Year), alpha=0.1) +
  scale_fill_viridis_b() +
  geom_line(aes(Month, Average, group = Year, colour = Year)) +
  scale_color_viridis_b()
print(Temp)

# Average yearly rainfall
YearGraphRain <- ggplot(YearlyData_2023, aes(Year, MeanRainfall)) +
  geom_point() +
  geom_line() +
  geom_smooth(method=lm) +
labs(y = 'Average Annual Rainfall (mm)', x ="Year", title = "Lord Howe Island Annual Average Rainfall since 1989") +
  theme_light()
print(YearGraphRain)

YearGraphRainSum <- ggplot(YearlyData_2023, aes(Year, SumRainfall)) +
  geom_point() +
  geom_line() + 
  geom_smooth(method=lm)
print(YearGraphRainSum)




######## Plotting starts here#####################
p <-ggplot(climate)+
  geom_tile(aes(Month,Year,fill=T_Mean),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="Monthly Temps C",option ="C")
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(climate$Year))
p <-p + scale_x_continuous(breaks =c(1:12))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Average Temps"), x="Month", y="Year")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

# you will want to expand your plot screen before this bit!
p #awesomeness



######## Plotting starts here#####################
p <-ggplot(climate)+
  geom_tile(aes(Month,Year,fill=T_HighestMax),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="Monthly Absolute Max Temps C",option ="C")
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(climate$Year))
p <-p + scale_x_continuous(breaks =c(1:12))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Average Temps"), x="Month", y="Year")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

# you will want to expand your plot screen before this bit!
p #awesomeness

######## Plotting starts here#####################
p <-ggplot(climate)+
  geom_tile(aes(Year,Month,fill=T_HighestMax),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="Monthly Absolute Max Temps C",option ="C")
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(climate$Month))
p <-p + scale_x_continuous(breaks =c(1988:2024))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Monthly Absolute Max Temps C"), x="Year", y="Month")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

# you will want to expand your plot screen before this bit!
p #awesomeness


######## Plotting starts here#####################
p <-ggplot(climate)+
  geom_tile(aes(Year,Month,fill=T_MeanMax),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="Average Monthly Max Temps C",option ="C")
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(climate$Month))
p <-p + scale_x_continuous(breaks =c(1988:2024))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Max Temps"), x="Year", y="Month")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

# you will want to expand your plot screen before this bit!
p #awesomeness


######## RAINFALL#####################
p <-ggplot(climate)+
  geom_tile(aes(Year,Month,fill=Rainfall),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="Average Monthly Rainfall (mm)",option ="G", direction = -1)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(climate$Month))
p <-p + scale_x_continuous(breaks =c(1988:2024))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Average Monthly Rainfall (mm)"), x="Year", y="Month")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

# you will want to expand your plot screen before this bit!
p #awesomeness


######## RAINFALL#####################
p <-ggplot(climate)+
  geom_tile(aes(Year,Month,fill=T_MeanMin),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="Average Monthly Minimum Temp (C)",option ="C")
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(climate$Month))
p <-p + scale_x_continuous(breaks =c(1988:2024))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Average Monthly Minimum Temp (C)"), x="Year", y="Month")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

# you will want to expand your plot screen before this bit!
p #awesomeness
