#require("here")
library("tidyverse")
#library("lubridate")
library("dplyr")
library("data.table")
#library(caret)
#require("ggplot2")
#library("pracma")
require("kableExtra")
require("broom")

load("Reli_Demo.Rda")
Reli_Demo$TBI_Non <- "Non"

load("Vib_Demo.Rda")
Vib_Demo <- Vib_Demo[-c(4)]

load("Chiro_Demo.Rda")
Chiro_Demo <- Chiro_Demo[-c(3)]

AllPartic <- rbind(Reli_Demo, Vib_Demo, Chiro_Demo)
n_Partic <-     nrow(AllPartic)
age_Range_T <- range(AllPartic$ageYrs)
mean_Age <-     mean(AllPartic$ageYrs, na.rm = T)
sd_Age <-         sd(AllPartic$ageYrs, na.rm = T)

female <- AllPartic %>% filter(PxGender == "Female")
n_Female <-     nrow(female)
age_Range_F <- range(female$ageYrs)
mean_Age_F <-   mean(female$ageYrs, na.rm = T)
sd_Age_F <-       sd(female$ageYrs, na.rm = T) 

male <- AllPartic %>% filter(PxGender == "Male")
n_Male <-       nrow(male)
age_Range_M <- range(male$ageYrs)
mean_Age_M <-   mean(male$ageYrs, na.rm = T)
sd_Age_M <-       sd(male$ageYrs, na.rm = T)

All_Dem <- data.frame(Tot_Partic = n_Partic,
                        age_Range_T = paste0(age_Range_T[1],"-", age_Range_T[2]),
                        mean_Age = mean_Age,
                        sd_Age = sd_Age,
                        n_Female = n_Female,
                        age_Range_F = paste0(age_Range_F[1],"-", age_Range_F[2]),
                        mean_Age_F = mean_Age_F,
                        sd_Age_F = sd_Age_F,
                        n_Male = n_Male,
                        age_Range_M = paste0(age_Range_M[1],"-", age_Range_M[2]),
                        mean_Age_M = mean_Age_M,
                        sd_Age_M = sd_Age_M)
view(All_Dem)

All_Dem %>%
  kbl(caption = "Demographic Data for All") %>%
  kable_classic(full_width = F, html_font = "Arial")



NonPartic <- AllPartic %>% filter(TBI_Non == "Non")
n_Partic <-     nrow(NonPartic)
age_Range_T <- range(NonPartic$ageYrs)
mean_Age <-     mean(NonPartic$ageYrs, na.rm = T)
sd_Age <-         sd(NonPartic$ageYrs, na.rm = T)

female <- NonPartic %>% filter(PxGender == "Female")
n_Female <-     nrow(female)
age_Range_F <- range(female$ageYrs)
mean_Age_F <-   mean(female$ageYrs, na.rm = T)
sd_Age_F <-       sd(female$ageYrs, na.rm = T) 

male <- NonPartic %>% filter(PxGender == "Male")
n_Male <-       nrow(male)
age_Range_M <- range(male$ageYrs)
mean_Age_M <-   mean(male$ageYrs, na.rm = T)
sd_Age_M <-       sd(male$ageYrs, na.rm = T)

Non_Dem <- data.frame(Tot_Partic = n_Partic,
                      age_Range_T = paste0(age_Range_T[1],"-", age_Range_T[2]),
                      mean_Age = mean_Age,
                      sd_Age = sd_Age,
                      n_Female = n_Female,
                      age_Range_F = paste0(age_Range_F[1],"-", age_Range_F[2]),
                      mean_Age_F = mean_Age_F,
                      sd_Age_F = sd_Age_F,
                      n_Male = n_Male,
                      age_Range_M = paste0(age_Range_M[1],"-", age_Range_M[2]),
                      mean_Age_M = mean_Age_M,
                      sd_Age_M = sd_Age_M)
view(Non_Dem)

Non_Dem %>%
  kbl(caption = "Demographic Data for Non") %>%
  kable_classic(full_width = F, html_font = "Arial")

########
mTBIPartic <- AllPartic %>% filter(TBI_Non == "short")
n_Partic <-     nrow(mTBIPartic)
age_Range_T <- range(mTBIPartic$ageYrs)
mean_Age <-     mean(mTBIPartic$ageYrs, na.rm = T)
sd_Age <-         sd(mTBIPartic$ageYrs, na.rm = T)

female <- mTBIPartic %>% filter(PxGender == "Female")
n_Female <-     nrow(female)
age_Range_F <- range(female$ageYrs)
mean_Age_F <-   mean(female$ageYrs, na.rm = T)
sd_Age_F <-       sd(female$ageYrs, na.rm = T) 

male <- mTBIPartic %>% filter(PxGender == "Male")
n_Male <-       nrow(male)
age_Range_M <- range(male$ageYrs)
mean_Age_M <-   mean(male$ageYrs, na.rm = T)
sd_Age_M <-       sd(male$ageYrs, na.rm = T)

mTBI_Dem <- data.frame(Tot_Partic = n_Partic,
                      age_Range_T = paste0(age_Range_T[1],"-", age_Range_T[2]),
                      mean_Age = mean_Age,
                      sd_Age = sd_Age,
                      n_Female = n_Female,
                      age_Range_F = paste0(age_Range_F[1],"-", age_Range_F[2]),
                      mean_Age_F = mean_Age_F,
                      sd_Age_F = sd_Age_F,
                      n_Male = n_Male,
                      age_Range_M = paste0(age_Range_M[1],"-", age_Range_M[2]),
                      mean_Age_M = mean_Age_M,
                      sd_Age_M = sd_Age_M)

mTBI_Dem %>%
  kbl(caption = "Demographic Data for mTBI") %>%
  kable_classic(full_width = F, html_font = "Arial")


########
PCS_DemPartic <- AllPartic %>% filter(TBI_Non == "long")
n_Partic <-     nrow(PCS_DemPartic)
age_Range_T <- range(PCS_DemPartic$ageYrs)
mean_Age <-     mean(PCS_DemPartic$ageYrs, na.rm = T)
sd_Age <-         sd(PCS_DemPartic$ageYrs, na.rm = T)

female <- PCS_DemPartic %>% filter(PxGender == "Female")
n_Female <-     nrow(female)
age_Range_F <- range(female$ageYrs)
mean_Age_F <-   mean(female$ageYrs, na.rm = T)
sd_Age_F <-       sd(female$ageYrs, na.rm = T) 

male <- PCS_DemPartic %>% filter(PxGender == "Male")
n_Male <-       nrow(male)
age_Range_M <- range(male$ageYrs)
mean_Age_M <-   mean(male$ageYrs, na.rm = T)
sd_Age_M <-       sd(male$ageYrs, na.rm = T)

PCS_Dem <- data.frame(Tot_Partic = n_Partic,
                       age_Range_T = paste0(age_Range_T[1],"-", age_Range_T[2]),
                       mean_Age = mean_Age,
                       sd_Age = sd_Age,
                       n_Female = n_Female,
                       age_Range_F = paste0(age_Range_F[1],"-", age_Range_F[2]),
                       mean_Age_F = mean_Age_F,
                       sd_Age_F = sd_Age_F,
                       n_Male = n_Male,
                       age_Range_M = paste0(age_Range_M[1],"-", age_Range_M[2]),
                       mean_Age_M = mean_Age_M,
                       sd_Age_M = sd_Age_M)

PCS_Dem %>%
  kbl(caption = "Demographic Data for PCS") %>%
  kable_classic(full_width = F, html_font = "Arial")

