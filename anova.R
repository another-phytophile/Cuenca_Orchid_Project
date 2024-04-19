# Purpose Conduct Analysis on Cleaned Data

# Library Statements
library(tidyverse)
library(here)
library(tinytex)
library(haven)
library(lsmeans)
library(lme4)
install.packages("car")
library(car)



orchidsbnrm <- lm(nr~es*med*l,orchidsfinal)
summary(orchidsbnrm)
anova(orchidsbnrm)
# lsmeans(orchidsbnrm)

lsmeans(orchidsbnrm,specs="l")

# Plots

#Residuals
orchidsbnrmr <- tibble(
  "fit" = orchidsbnrm$fitted.values,
  "resid" = orchidsbnrm$residuals
)

orchidsbnrmrs <- add_column(orchidsbnrmr, "rstandardized"=rstandard(orchidsbnrm))

ggplot(orchidsbnrmr,aes(x=fit,y=resid))+
  geom_jitter(color="darkmagenta")+
  geom_hline(yintercept = 0, linetype="dotted")+
  labs(title = paste("Q1: Residuals Versus Fitted Values for the orchidsbnr Data Set"),
       subtitle = "by Jerry Yu")+ 
  theme(plot.title = element_text(size = 14))

# QQ
ggplot(data = orchidsbnrmrs, aes(sample = resid))+
  geom_qq( color="coral")+
  geom_qq_line( color="turquoise")+
  labs(
    title = paste("Q1: QQ Plot of Residuals for orchidsbnr Linear Regression Model"),
    subtitle = "by Jerry Yu"
  ) +
  xlab("Theoretical Quantiles")+
  ylab("Sample Quantiles")




orchidsrnumm <- lm(rnum~es*med*l,orchidsfinal)
summary(orchidsrnumm)
anova(orchidsrnumm)

#Residuals
orchidsrnummr <- tibble(
  "fit" = orchidsrnumm$fitted.values,
  "resid" = orchidsrnumm$residuals
)

orchidsrnummrs <- add_column(orchidsrnummr, "rstandardized"=rstandard(orchidsrnumm))

ggplot(orchidsrnummr,aes(x=fit,y=resid))+
  geom_jitter(color="darkmagenta")+
  geom_hline(yintercept = 0, linetype="dotted")+
  labs(title = paste("Q1: Residuals Versus Fitted Values for the orchidsbnr Data Set"),
       subtitle = "by Jerry Yu")+ 
  theme(plot.title = element_text(size = 14))

# QQ
ggplot(data = orchidsrnummrs, aes(sample = resid))+
  geom_qq( color="coral")+
  geom_qq_line( color="turquoise")+
  labs(
    title = paste("Q1: QQ Plot of Residuals for orchidsbnr Linear Regression Model"),
    subtitle = "by Jerry Yu"
  ) +
  xlab("Theoretical Quantiles")+
  ylab("Sample Quantiles")


cmax = orchidsfinal %>% filter(es=="11")
cmax
cmaxm <- lm(rnum~med*l,cmax)
cmaxm <- lmer(rnum~med*l,cmax)
summary(cmaxm)
anova(cmaxm)

Anova(cmaxm,type=3)


# Test w Numeric IBAconc

orchidsrnummIBA <- lm(rnum~IBAconc*l,orchidsfinal)
summary(orchidsrnummIBA)


# Negative Bionamial

# 0 Inflated Posson

# 0 Inflated Neg Bin













































