library('tidyverse')
library('ggplot2')
library('readxl')
library("climaemet")

install.packages("Interpol.T")

library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
# library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 



#### Rainfall over the years by month ####

## Rainfall LINE GRAPH
rainfallgraph <- ggplot(precipYr, aes(month, rain, group = year, colour = year)) +
  geom_point() +
  geom_line(lwd = 1) +
  scale_color_viridis_b()
print(rainfallgraph)

## Rainfall BAR GRAPH
RainfallBar <- ggplot(precipYr, aes(x = year, y = rain, fill = month)) +
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



homvar_monthNew <- left_join(homvar_month, yearlyData, by = 'date')

#### Temperature over the years by month ####

## Min Average over the years by month
mingraph <- ggplot(homvar_month, aes(month, tmin, group = year, colour = year)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_b()
print(mingraph)

## Max Average over the years by month
maxgraph <- ggplot(homvar_month, aes(month, tmax, group = year, colour = year)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_b()
print(maxgraph)


## Temps over the months
tempsGraph <- ggplot(homvar_month) +
  geom_point(aes(date, tmean), colour = 'black') +
  geom_point(aes(date, tmax), colour = 'red') +
  geom_point(aes(date, tmin), colour = 'blue') +
#  geom_point(aes(date, T_HighestMax), colour = 'red', alpha = 0.2) +
#  geom_point(aes(date, T_LowestMin), colour = 'blue', alpha = 0.2) +
  geom_line(aes(date, tmean, na.rm = TRUE), colour = 'black', lwd = 1) 
  
print(tempsGraph)


## Just Jan
homvar_monthJan <- homvar_month |>
  filter(month == 1)
tempsGraphJan <- ggplot(homvar_monthJan) +
  geom_point(aes(date, tmean), colour = 'black') +
  geom_point(aes(date, tmax), colour = 'red') +
  geom_point(aes(date, tmin), colour = 'blue') +
  #geom_point(aes(date, T_HighestMax), colour = 'red', alpha = 0.2) +
  #geom_point(aes(date, T_LowestMin), colour = 'blue', alpha = 0.2) +
  geom_line(aes(date, tmean, na.rm = TRUE), colour = 'black', lwd = 1) 

print(tempsGraphJan)


## Average Temp over the years by month

# Temp Ribbon Graph
Temp <- ggplot(homvar_month) +
  geom_ribbon(aes(x=month, ymin=tmin,ymax=tmax, group = year, fill = year), alpha=0.1) +
  scale_fill_viridis_b() +
  geom_line(aes(month, tmean, group = year, colour = year)) +
  scale_color_viridis_b()
print(Temp)

# Temp Line Graph
AvTemp <- ggplot(homvar_month, aes(month, tmean, group = year, colour = year)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_b()
print(AvTemp)


# Bar graph of month by year - NOT GOOD
AvTempBar <- ggplot(homvar_month, aes(x = year, y = tmean, fill = month)) +
  geom_bar(position="stack", stat = 'identity') +
  scale_fill_viridis_c()
print(AvTempBar)


### yearly Totals ###
yearlyData<- homvar_month |>
  group_by(year) |>
  summarise(MeanMin = mean(tmin),
            MeanMax = mean(tmax),
            Average = mean(tmean),
            SumRainfall = sum(rain),
            MeanRainfall = mean(rain)) |>
  filter(year != '1988')
yearlyData$date <- dmy(paste("15","6", yearlyData$year, sep = "/"))


yearlyData_2023 <- yearlyData |>
  filter(year < 2024)

yearlyData_2020 <- yearlyData |>
  filter(year < 2021)

# Average yearly temperatures
m1 <- lm(Average ~ year, yearlyData_2020)
summary(m1)
anova(m1)
plot(Average ~ year, yearlyData_2023)
abline(m1, col = "red")


library(esquisse)
esquisser(yearlyData_2023)

annual <- read.csv("./data/nationalaveragesSUMMARY.csv")

m2 <- lm(AverageTemp ~ year, annual)
summary(m2)
anova(m2)
plot(Average ~ year, yearlyData_2023)
abline(m1, col = "red")








########### GOOD PLOTS ##########
averagemodel <- lm(Average ~ year, yearlyData_2023)

newdat <- data.frame("year" = c(1989, 2023))
preds <- predict.lm(averagemodel, newdata = newdat)
changedeg <- preds[2]-preds[1]

changedeg

yearGraph <- ggplot(yearlyData_2023, aes(x=year, y=Average)) +
  geom_point(size = 3) +
  geom_line(lwd = 1) +
  geom_smooth(method=lm, col = "red") +
  labs(y = 'Average yearly Temperature (C)', x ="year", title = "Lord Howe Island Annual Average Temperatures since 1989") +
  annotate(geom="text", x=1992, y=20.1, label="Multiple R-squared = 0.327***",
            color="black", fontface="italic") +
  annotate(geom="text", x=2018, y=19.2, label="0.592°C increase in average temperature since 1989",
           color="black") 
  
  theme_minimal()
yearGraph


# Average yearly temperatures
yearGraphRibbon <- ggplot(yearlyData) +
  geom_ribbon(aes(x=year, ymin=MeanMin, ymax=MeanMax), alpha=0.1) +
  geom_line(aes(year, Average))

print(yearGraphRibbon)


# Temp Ribbon Graph
Temp <- ggplot(homvar_month) +
  geom_ribbon(aes(x=month, ymin=tmin,ymax=tmax, group = year, , fill = year), alpha=0.1) +
  scale_fill_viridis_b() +
  geom_line(aes(month, tmean, group = year, colour = year)) +
  scale_color_viridis_b()
print(Temp)

# Average yearly rainfall
yearGraphRain <- ggplot(yearlyData_2023, aes(year, MeanRainfall)) +
  geom_point() +
  geom_line() +
  geom_smooth(method=lm) +
labs(y = 'Average Annual Rainfall (mm)', x ="year", title = "Lord Howe Island Annual Average Rainfall since 1989") +
  theme_light()
print(yearGraphRain)

yearGraphRainSum <- ggplot(yearlyData_2023, aes(year, SumRainfall)) +
  geom_point() +
  geom_line() + 
  geom_smooth(method=lm)
print(yearGraphRainSum)




######## Plotting starts here#####################
p <-ggplot(homvar_month)+
  geom_tile(aes(month,year,fill=tmean),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="monthly Temps C",option ="C")
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(homvar_month$year))
p <-p + scale_x_continuous(breaks =c(1:12))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Average Temps"), x="month", y="year")
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
p <-ggplot(homvar_month)+
  geom_tile(aes(month,year,fill=T_HighestMax),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="monthly Absolute Max Temps C",option ="C")
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(homvar_month$year))
p <-p + scale_x_continuous(breaks =c(1:12))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Average Temps"), x="month", y="year")
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
p <-ggplot(homvar_month)+
  geom_tile(aes(year,month,fill=T_HighestMax),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="monthly Absolute Max Temps C",option ="C")
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(homvar_month$month))
p <-p + scale_x_continuous(breaks =c(1988:2024))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("monthly Absolute Max Temps C"), x="year", y="month")
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
p <-ggplot(homvar_month)+
  geom_tile(aes(year,month,fill=tmax),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="Average monthly Max Temps C",option ="C")
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(homvar_month$month))
p <-p + scale_x_continuous(breaks =c(1988:2024))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Max Temps"), x="year", y="month")
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
p <-ggplot(homvar_month)+
  geom_tile(aes(year,month,fill=rain),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="Average monthly Rainfall (mm)",option ="G", direction = -1)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(homvar_month$month))
p <-p + scale_x_continuous(breaks =c(1988:2024))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Average monthly Rainfall (mm)"), x="year", y="month")
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
p <-ggplot(homvar_month)+
  geom_tile(aes(year,month,fill=tmin),color= "white",lwd=0.1) + 
  scale_fill_viridis(name="Average monthly Minimum Temp (C)",option ="C")
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(homvar_month$month))
p <-p + scale_x_continuous(breaks =c(1988:2024))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Average monthly Minimum Temp (C)"), x="year", y="month")
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

#### STRIPE PLOTS ####

ggstripes(
    stripe_av,
    plot_type = c("stripes"),
    plot_title = "LHI Average"
  )

stripeplot <- function(df, var, plotname) {
  var_name <- deparse(substitute(var))
  col <- df[[var_name]]
  stripe_df <- df |>
    mutate(temp = col) |>
    dplyr::select(c("year", "temp"))
  ggstripes(
    stripe_df,
    plot_type = c("stripes"),
    plot_title = plotname
    )
  ggsave(paste("../graphs/climatestripes", var_name, ".jpeg", sep = ""))
}


stripeplot(tempYr, meanmax, "LHI mean max")
stripeplot(tempYr, meanmin, "LHI mean min")
stripeplot(tempYr, meantemp, "LHI mean temp")

