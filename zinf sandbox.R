# Game Plan? 

# - split up 
# - fit 2-3 models
# - compare w prediction
# - conconlude

# Library Statements
library(tidyverse)
library(here)
library(tinytex)
library(haven)
library(lsmeans)
library(lme4)
library(car)
library(MASS)
install.packages("countreg", repos="http://R-Forge.R-project.org")
install.packages("distributions3")
library(countreg)


#################################
### Create training/test data ###
set.seed(330|3)
orchidsfinalb <- orchidsfinal %>% add_column(tset = sample.int(2,nrow(orchidsfinal),replace=TRUE,prob=c(0.67,0.33)))

orchidsfinalb
orchidstrain <- filter(orchidsfinalb,tset==1)
orchidstrain$med <- relevel(orchidstrain$med,ref = "P4")
orchidstrain$l <- relevel(orchidstrain$l,ref = "Oscuridad")
orchidstest <-  filter(orchidsfinalb,tset==2)

orchidstrain
orchidstest

# Negative Binomial

orchidsnb <- glm.nb(nr~med*l,orchidsfinal)
summary(orchidsnb)

# Zero Inflated Negative Binomial

orchidsznb <- zeroinfl(nr~med*l,orchidsfinal,dist="negbin")
summary(orchidsznb) 

# Negative Binomial With Numbers

orchidsnb <- glm.nb(nr~IBAconc*l,orchidsfinal)
summary(orchidsnb)

# Zero Inflated Negative Binomial With Numbers

orchidsznb <- zeroinfl(nr~IBAconc*l,orchidsfinal,dist="negbin")
summary(orchidsznb) 

library(ggplot2)
ggplot(data = orchidsfinal, aes(x=IBAconc,y=rnum,color=es)) +
  geom_jitter()
ggplot(data = orchidsfinal, aes(x=IBAconc,y=nr,color=es)) +
  geom_jitter()

lrtest(orchidsznb,orchidsnb)

315.566 /2
