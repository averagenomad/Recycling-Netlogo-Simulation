library(readr)
library(haven)
library(dplyr)
library(readstata13)
library(lattice)
library(ggcorrplot)
library(GGally)
library(PerformanceAnalytics)
library(stargazer)
library(ggcorrplot)
library(ggplot2)
library(lm.beta)

##Load the file
setwd("/Users/jamilion/Documents/CGU/317 Advanced formal Models/Final Project")
doc<- read_csv("new.csv")%>%
  mutate(green.)
bin <- read_csv("binary.csv")

###cor plot
chart.Correlation(bin)

###Regression
lm <- lm(mean_ideology ~   mean_wealth  + vision  + recyclingbins,  doc) 
summary(lm)

lm1 <- lm(mean_normalizedIdeology ~  mean_wealth  + vision  + recyclingbins,  doc) 
summary(lm1)

### Green people
lm2 <- lm(green ~   mean_wealth  + vision  + recyclingbins, doc) 
summary(lm2)


###BINARY
chart.Correlation(bin)

###Regression
# reg <- lm(mean_ideology ~   mean_wealth  + vision  + recyclingbins + Trashbins,  bin) 
# summary(reg)

##Ideology
reg <- lm(scale(mean_ideology) ~   scale(mean_wealth)  + scale(vision)  + scale(recyclingbins) + scale(Trashbins),  bin) 
summary(reg)

### recycling people
reg2 <- lm(scale(green) ~ scale(mean_wealth)  + scale(vision)  + scale(recyclingbins) + scale(Trashbins), bin) 
summary(reg2)

##contamination
reg3 <- lm(scale(mean_contamination) ~   scale(mean_wealth)  + scale(vision)  + scale(recyclingbins) + scale(Trashbins), bin) 
summary(reg3)

###output
stargazer(reg, reg2, reg3, title="Regression Results", align=TRUE, 
          type = "text", out = "latex.doc")




















































