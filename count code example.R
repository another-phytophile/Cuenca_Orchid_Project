library(readxl)
library(compareDF)
library(tidyverse)
library(here)
library(tinytex)
library(haven)
library(lsmeans)
library(lmtest)

# Data Import and Observation
orchidsbase <- read_csv("Data/finaltest.csv") %>% as_tibble()

# Data Visualization and Summarization
ggplot(orchidsbase,aes(nr))+
  geom_dotplot()

fivenum(orchidsbase$nr)
mean(orchidsbase$nr)
var(orchidsbase$nr)

# Observations

# - Clearly many extra 0s
# - Overdispersed
# - Not normal
# - Integers only

# Poisson
glm(nr~med*l*es,orchidsbase,family="poisson") %>% summary()
glm(nr~med*l,orchidsbase,family="poisson") %>% anova()
glm(nr~med*l,orchidsbase,family="poisson") %>% lrtest()

# Quasi-Poisson
glm(nr~med*l,orchidsbase,family="quasipoisson") %>% summary()
glm(nr~med*l,orchidsbase,family="quasipoisson") %>% anova()
glm(nr~med*l,orchidsbase,family="quasipoisson") %>% lrtest()
AIC(glm(nr~med*l,orchidsbase,family=quasipoisson()))
