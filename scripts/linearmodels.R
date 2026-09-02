#1. identify your response and predictor variables.
#2. explore your data (graphically is a good place to start).
#3. define a probability distribution that is, at least to a first approximation, likely to describe variation in your response variable (go with a normal distribution for now).
#4. mathematically describe a potential relationship between predictor and response. For each model, write this out so you can fully absorb what you are doing when you shift to fitting the model in R.
#5. fit the model in R.
#6. plot the model over the fitted observations. To a first approximation, does the model seem like a reasonable description of the data?
#7.  check model assumptions.
#8. if assumptions are violated, do what you need to do to resolve any issues (e.g. refit the model after transforming the data, include extra terms to account for curvature, etc.). Repeat steps 4 to 8 until you are happy that you have ‘found’ a good model to describe your data.
#9. test hypotheses (assume a null hypothesis of no effect)
#10. provide estimates of uncertainty (calculate confidence intervals on the parameters and on the regression line).
#11. plot the data together with the fitted model and confidence intervals.
#12. determine how much variation in your data the model explains.

#2. Explore graphically - WITH SCATTER, HIST, PAIRPLOTS
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(SiZer)
library(ggpubr)

setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# Read in data and clean
homvar_month <- read.csv("data/cipsap/Lord-Howe-Island-Aero_variables_monthly_1940_2024_hom.csv") |>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  dplyr::select(!c(country, station.name, day, dtr))

#Format temperature data to usable structure
tempYr <- homvar_month |>
  group_by(year) |>
  filter(!any(is.na(tmean))) |>
  summarise(meanmax = mean(tmax),
         meanmin = mean(tmin),
         meantemp = mean(tmean)) |>
  filter(year >2011)

#Format precip data to usable structure
precipYr <-  homvar_month |>
  group_by(year) |>
  filter(!any(is.na(rain))) |>
  ungroup()

### LINEAR MODEL ###

## Plot lm vs loess curve ##
ggplot(tempYr, aes(year, meantemp)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = TRUE) +
  geom_smooth(method = "loess", se = TRUE) +
  stat_regline_equation(label.x = 2000, label.y = 18.7)

### Linear model testing ###

m1 <- lm(meantemp ~ year, data = tempYr)
change <- round(m1[["coefficients"]][[2]]*10, 2)
summary(m1)
ggplot(tempYr) +
  geom_point(aes(year, meantemp)) +
  geom_smooth(aes(year, meantemp), method = "loess", se = TRUE) +
  geom_smooth(aes(year, meantemp), method = "lm", se = TRUE) +
  annotate("text", x = 2000, y = 18.7, label =change , color = "black", size = 5)

par(mfrow = c(2, 2))
plot(m1)
abline(m1), col = "red", lwd = 2)
confint(m1)


par(mfrow = c(1, 1))
acf(residuals(m1))
m0 <- lm(meantemp ~ 1, data = df)
anova(m0,m1) # reduced model before full model


linearmods <- function(df, var) {
  
  var_name <- deparse(substitute(var))
  
  form <- reformulate("year", response = var_name)
  
  m1 <- lm(form, data = df)
  m0 <- lm(as.formula(paste(var_name, "~ 1")), data = df)
  
  par(mfrow = c(1, 1))
  plot(form, data = df)
  abline(m1, col = "red", lwd = 2)
  
  print(summary(m1))
  print(confint(m1))
  
  par(mfrow = c(2, 2))
  plot(m1)
  
  par(mfrow = c(1, 1))
  acf(residuals(m1))
  
  print(anova(m0, m1))
  
  return(m1)
}

mod_meantemp <- linearmods(tempYr, meantemp)
plot(m1)
par(mfrow = c(1, 1))
mod_rainfall <- linearmods(precipYr, rain)
mod_rainfall


#6. Plot variables & model
par(mfrow = c(1, 1))
plot(meantemp ~ year, data = tempYr)
# Initial linear model
m1 <- lm(meantemp ~ year, data = tempYr)
abline(m1, col = "red", lwd = 2)

# Check r squared
summary(m1)

#7. Check assumptions - quick check
par(mfrow = c(2, 2))
plot(m1)
acf(residuals(m1))
# Autocorrelation exists (expected)

# Check residuals - is there a pattern?
par(mfrow = c(1, 1))
plot(m1$residuals ~ m1$fitted.values)
abline(h = 0, col = "blue", lwd = 3)

# Distributional assumptions (error normally distributed)
# QQ PLOTS - should be 45 degrees - LOOKS OK
qqnorm(m1$residuals)
qqline(m1$residuals)

# No excessively influential observations
# PLOT RESIDUALS/LEVERAGE (not within cooks distance)
par(mfrow = c(2, 2))
plot(m1)
plot(m1)[4] 
# Gives the cooks distance
cooksD <- cooks.distance(m1)
influential <- cooksD[(cooksD > (3*mean(cooksD)))]
influential

#9. Test hypothesis
m0 <- lm(formula = meantemp ~ 1, data = tempYr)
anova(m0,m1) # reduced model before full model

#10. Provide estimate of uncertainty (confidence intervals)
confint(m1)

#11. Plot data with confidence & prediction intervals
par(mfrow = c(1, 1))
newdat <- data.frame(x=seq(min(tempYr$year),max(tempYr$year),length = 100))
names(newdat) <- "year"
conf <- predict(object = m1, newdat, interval = "confidence")
preds <-predict(object = m1, newdat, interval = "predict")
newdat <-cbind(newdat, y=conf, z=preds)
plot(meantemp~year, data = tempYr)
with(newdat, {lines(y.lwr ~ year, col = "blue", lty =2)
  lines(y.upr ~ year, col = "blue", lty = 2)})
with(newdat, {lines(z.lwr ~ year, col = "green", lty =2)
  lines(z.upr ~ year, col = "green", lty = 2)})


#12. How much variation does the model explain?
# Check r squared
summary(model)
# Rsquared value - 0.4812 - not very good.

# Model 2 - generalised least squares? To account for data being correlated temporally
m2 <- gls(
  meantemp ~ year,
  data = tempYr,
  correlation = corAR1(form = ~ year)
)

summary(m2)


## Plot lm vs loess curve for minmean, minmax, meanmax##
m1 <- lm(meantemp ~ year, data = tempYr)
change <- round(m1[["coefficients"]][[2]]*10, 2)
m_min <- lm(meanmin ~ year, data = tempYr)
change_min <- round(m_min[["coefficients"]][[2]]*10, 2)
m_max <- lm(meanmax ~ year, data = tempYr)
change_max <- round(m_max[["coefficients"]][[2]]*10, 2)

ggplot(tempYr) +
  geom_ribbon(aes(year, ymin = meanmin, ymax = meanmax), fill = "lightblue", alpha = 0.5) +
  geom_point(aes(year, meantemp), colour = "black") +
  geom_smooth(aes(year, meantemp), method = "lm", se = TRUE, , colour = "black") +
  annotate("text", x = 2030, y = 20, label =change , color = "black", size = 3) +
  geom_point(aes(year, meanmin), colour = "blue") +
  geom_smooth(aes(year, meanmin), method = "lm", se = TRUE, colour = "blue") +
  annotate("text", x = 2030, y = 17.5, label =change_min , color = "blue", size = 3) +
  geom_point(aes(year, meanmax), colour = "red") +
  geom_smooth(aes(year, meanmax), method = "lm", se = TRUE, colour = "red") +
  annotate("text", x = 2030, y = 22.5, label =change_max , color = "red", size = 3)

stder_mean <- round(m1[["coefficients"]][[2]]-confint(m1)[2], 4)
paste(round(m1[["coefficients"]][[2]], 3), stder_mean, sep = "+/-")

stder_min <- round(m_min[["coefficients"]][[2]]-confint(m_min)[2], 4)
paste(round(m_min[["coefficients"]][[2]], 3), stder_min, sep = "+/-")


stder_max <-round(m_max[["coefficients"]][[2]]-confint(m_max)[2], 4)
paste(round(m_max[["coefficients"]][[2]], 3), stder_max, sep = "+/-")
