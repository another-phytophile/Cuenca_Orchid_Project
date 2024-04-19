# Purpose: Clean and Process Data for the Cuenca Orchid Project to a point where it can be used for an ANOVA. 

# Library Statements
library(here)
library(readxl)
library(tidyverse)
library(compareDF)


# Import
raworchids=read_excel("Data/Tabla_de_medidas.xlsx") %>% as_tibble()

# Data Cleaning

#Create Simpler Names
names(raworchids) <- c("es","med","p","nr","r","lr","l")

orchids$med
# Assign Values to Merged Data
orchids <-  raworchids %>% fill(es, med, p, nr, l) %>%
  mutate(l = as.factor(l)) %>%
  mutate(es = as.factor(es)) %>%
  mutate(med = as.factor(med))
  


# Remove Blank data (will create dummy data later)
orchids <-   filter(orchids,!grepl("N",p) | is.na(p))

# Average Root Numbers and Root Length

# Check to see if any observations with 0 roots are incorrect
ifelse(nrow((filter(orchids,nr==0 & r !=0)))==0, TRUE,FALSE)

# Sum values
orchidsum <-
  orchids %>% 
  group_by(es, med, l, p) %>% 
  summarise(rnum = mean(lr),nr1=last(nr),nr2=n()) 

# Get correct Root Number Values
orchidsrn <- orchidsum %>% mutate(nr = ifelse(nr1 == 0, 0,
                                              ifelse(nr1 >= nr2, nr1, nr2))) %>% 
  ungroup()

# Create Dummy/Assumed Data for 11, 0.5
orchidsfinal <- orchidsrn %>% select(-nr2,-nr1) %>%
  add_row(
    es = c(rep(as.factor(11), 19)),
    med = c(rep(as.factor(0.5), 19)),
    l = c(rep(as.factor("Oscuridad"), 11), rep(as.factor("Luz"), 8)),
    p = c(as.character(1:19)),
    rnum = 0,
    nr = 0
  ) %>%
  mutate(IBAconc = case_when(med == "P4" ~ 0,
                             med == '0.5' ~ 5,
                             med == '1' ~ 1))


write.csv(orchidsfinal,"Data/finaltest.csv")

