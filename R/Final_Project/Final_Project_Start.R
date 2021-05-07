library(tidyverse)
library(modelr)
library(moderndive)
library(gapminder)
library(broom)
library(knitr)
library(stargazer)

marissa_data1 <- read.csv('~/R/Final_Project/Final_Project_Data.csv', 
                        header=T, na.strings=".")
View(marissa_data1)

#Change column headers
marissa_data <- marissa_data1 %>% 
  rename(
    Sex = M.F,
    LarvaeAge = Age.month,
    InVitroViability = In.Vitro.Viability,
    InVitroExsheathment = In.vitro.Exsheathment,
    InVivoViability = In.Vivo.Viability,
    InVivoExsheathment = In.Vivo.Exsheathment
    )
view(marissa_data)

#Filter by Cycle to make comparisons within each experiment
cycle_1 <- filter(marissa_data, Cycle == 1)
cycle_2 <- filter(marissa_data, Cycle == 2)
cycle_3 <- filter(marissa_data, Cycle == 3)
cycle_4 <- filter(marissa_data, Cycle == 4)
cycle_5 <- filter(marissa_data, Cycle == 5)

#Looking to compare the following
  #LarvalAge vs viability, exsheathment, and hatachability
  #inviro vs in vivo for larval age
  #Cycle to cycle for viability, exsheathment, an hatchability by larval age

#data sets that remove NA values for accurate results
no_hatch_comp_data <- filter(marissa_data, !is.na(Hatchability))
view(no_hatch_comp_data)

no_InVivo_comp_data <- filter(marissa_data, !is.na(InVivoViability))
view(no_InVivo_comp_data)

no_InVitro_comp_data <- filter(no_InVivo_comp_data, !is.na(InVitroViability))
view(no_InVitro_comp_data)

no_cycle1 <- filter(marissa_data, Cycle == 2:5)
view(no_cycle1)

cycle_1NA_vitro <- filter(cycle_1, !is.na(InVitroViability))
view(cycle_1NA_vitro)

cycle_1NA_hatch <- filter(cycle_1, !is.na(Hatchability))
view(cycle_1NA_hatch)

cycle_2NA_vitro <- filter(cycle_2, !is.na(InVitroViability))
view(cycle_2NA_vitro)

cycle_2NA_hatch <- filter(cycle_2, !is.na(Hatchability))
view(cycle_2NA_hatch)

cycle_3NA_vitro <- filter(cycle_3, !is.na(InVitroViability))
view(cycle_3NA_vitro)

cycle_3NA_hatch <- filter(cycle_3, !is.na(Hatchability))
view(cycle_3NA_hatch)

cycle_4NA_vitro <- filter(cycle_4, !is.na(InVitroViability))
view(cycle_4NA_vitro)

cycle_4NA_hatch <- filter(cycle_4, !is.na(Hatchability))
view(cycle_4NA_hatch)

cycle_5NA_vitro <- filter(cycle_5, !is.na(InVitroViability))
view(cycle_5NA_vitro)

cycle_5NA_hatch <- filter(cycle_5, !is.na(Hatchability))
view(cycle_5NA_hatch)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create Models for all possible outcomes

#Cycle 1
model_A1 <- lm(LarvaeAge ~ InVitroViability, data = cycle_1NA_vitro)
summary(model_A1) 

model_B1 <- lm(LarvaeAge ~ InVitroExsheathment, data = cycle_1NA_vitro)
summary(model_B1)

model_C1 <- lm(LarvaeAge ~ InVivoViability, data = cycle_1)
summary(model_C1)

model_D1 <- lm(LarvaeAge ~ InVivoExsheathment, data = cycle_1)
summary(model_D1)

model_E1 <- lm(LarvaeAge ~ Hatchability, data = cycle_1NA_hatch)
summary(model_E1)

model_F1 <- lm(LarvaeAge ~ InVitroViability + InVitroExsheathment, data = cycle_1NA_vitro)
summary(model_F1)

model_G1 <- lm(LarvaeAge ~ InVivoViability + InVivoExsheathment, data = cycle_1)
summary(model_G1)

model_H1 <- lm(LarvaeAge ~ InVitroViability + InVivoViability, data = cycle_1NA_vitro)
summary(model_H1)

model_I1 <- lm(LarvaeAge ~ InVitroExsheathment + InVivoExsheathment, data = cycle_1NA_vitro)
summary(model_I1)

model_J1 <- lm(LarvaeAge ~ InVitroViability + InVitroExsheathment + 
                 InVivoViability + InVivoExsheathment + Hatchability, data = cycle_1NA_hatch)
summary(model_J1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Cycle 2
model_A2 <- lm(LarvaeAge ~ InVitroViability, data = cycle_2NA_vitro)
summary(model_A2) 

model_B2 <- lm(LarvaeAge ~ InVitroExsheathment, data = cycle_2NA_vitro)
summary(model_B2)

model_C2 <- lm(LarvaeAge ~ InVivoViability, data = cycle_2)
summary(model_C2)

model_D2 <- lm(LarvaeAge ~ InVivoExsheathment, data = cycle_2)
summary(model_D2)

model_E2 <- lm(LarvaeAge ~ Hatchability, data = cycle_2NA_hatch)
summary(model_E2)

model_F2 <- lm(LarvaeAge ~ InVitroViability + InVitroExsheathment, data = cycle_2NA_vitro)
summary(model_F2)

model_G2 <- lm(LarvaeAge ~ InVivoViability + InVivoExsheathment, data = cycle_2)
summary(model_G2)

model_H2 <- lm(LarvaeAge ~ InVitroViability + InVivoViability, data = cycle_2NA_vitro)
summary(model_H2)

model_I2 <- lm(LarvaeAge ~ InVitroExsheathment + InVivoExsheathment, data = cycle_2NA_vitro)
summary(model_I2)

model_J2 <- lm(LarvaeAge ~ InVitroViability + InVitroExsheathment + 
                 InVivoViability + InVivoExsheathment + Hatchability, data = cycle_2NA_hatch)
summary(model_J2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Cycle 3
model_A3 <- lm(LarvaeAge ~ InVitroViability, data = cycle_3NA_vitro)
summary(model_A3) 

model_B3 <- lm(LarvaeAge ~ InVitroExsheathment, data = cycle_3NA_vitro)
summary(model_B3)

model_C3 <- lm(LarvaeAge ~ InVivoViability, data = cycle_3)
summary(model_C3)

model_D3 <- lm(LarvaeAge ~ InVivoExsheathment, data = cycle_3)
summary(model_D3)

model_E3 <- lm(LarvaeAge ~ Hatchability, data = cycle_3NA_hatch)
summary(model_E3)

model_F3 <- lm(LarvaeAge ~ InVitroViability + InVitroExsheathment, data = cycle_3NA_vitro)
summary(model_F3)

model_G3 <- lm(LarvaeAge ~ InVivoViability + InVivoExsheathment, data = cycle_3)
summary(model_G3)

model_H3 <- lm(LarvaeAge ~ InVitroViability + InVivoViability, data = cycle_3NA_vitro)
summary(model_H3)

model_I3 <- lm(LarvaeAge ~ InVitroExsheathment + InVivoExsheathment, data = cycle_3NA_vitro)
summary(model_I3)

model_J3 <- lm(LarvaeAge ~ InVitroViability + InVitroExsheathment + 
                 InVivoViability + InVivoExsheathment + Hatchability, data = cycle_3NA_hatch)
summary(model_J3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Cycle 4
model_A4 <- lm(LarvaeAge ~ InVitroViability, data = cycle_4NA_vitro)
summary(model_A4) 

model_B4 <- lm(LarvaeAge ~ InVitroExsheathment, data = cycle_4NA_vitro)
summary(model_B4)

model_C4 <- lm(LarvaeAge ~ InVivoViability, data = cycle_4)
summary(model_C4)

model_D4 <- lm(LarvaeAge ~ InVivoExsheathment, data = cycle_4)
summary(model_D4)

model_E4 <- lm(LarvaeAge ~ Hatchability, data = cycle_4NA_hatch)
summary(model_E4)

model_F4 <- lm(LarvaeAge ~ InVitroViability + InVitroExsheathment, data = cycle_4NA_vitro)
summary(model_F4)

model_G4 <- lm(LarvaeAge ~ InVivoViability + InVivoExsheathment, data = cycle_4)
summary(model_G4)

model_H4 <- lm(LarvaeAge ~ InVitroViability + InVivoViability, data = cycle_4NA_vitro)
summary(model_H4)

model_I4 <- lm(LarvaeAge ~ InVitroExsheathment + InVivoExsheathment, data = cycle_4NA_vitro)
summary(model_I4)

model_J4 <- lm(LarvaeAge ~ InVitroViability + InVitroExsheathment + 
                 InVivoViability + InVivoExsheathment + Hatchability, data = cycle_4NA_hatch)
summary(model_J4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Cycle 5
model_A5 <- lm(LarvaeAge ~ InVitroViability, data = cycle_5NA_vitro)
summary(model_A5) 

model_B5 <- lm(LarvaeAge ~ InVitroExsheathment, data = cycle_5NA_vitro)
summary(model_B5)

model_C5 <- lm(LarvaeAge ~ InVivoViability, data = cycle_5)
summary(model_C5)

model_D5 <- lm(LarvaeAge ~ InVivoExsheathment, data = cycle_5)
summary(model_D5)

model_E5 <- lm(LarvaeAge ~ Hatchability, data = cycle_5NA_hatch)
summary(model_E5)

model_F5 <- lm(LarvaeAge ~ InVitroViability + InVitroExsheathment, data = cycle_5NA_vitro)
summary(model_F5)

model_G5 <- lm(LarvaeAge ~ InVivoViability + InVivoExsheathment, data = cycle_5)
summary(model_G5)

model_H5 <- lm(LarvaeAge ~ InVitroViability + InVivoViability, data = cycle_5NA_vitro)
summary(model_H5)

model_I5 <- lm(LarvaeAge ~ InVitroExsheathment + InVivoExsheathment, data = cycle_5NA_vitro)
summary(model_I5)

model_J5 <- lm(LarvaeAge ~ InVitroViability + InVitroExsheathment + 
                 InVivoViability + InVivoExsheathment + Hatchability, data = cycle_5NA_hatch)
summary(model_J5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inter cycle modeling
model_K <- lm(Cycle ~ InVitroViability + InVitroExsheathment + 
                InVivoViability + InVivoExsheathment, data = no_cycle1)
summary(model_K)

model_L <- lm(LarvaeAge + Cycle ~ InVitroViability + InVitroExsheathment + 
                InVivoViability + InVivoExsheathment, data = no_cycle1)
summary(model_L)

model_M <- lm(LarvaeAge + Cycle ~ InVitroViability + InVivoViability, data = no_cycle1)
summary(model_M)

model_N <- lm(LarvaeAge + Cycle ~ InVitroExsheathment + InVivoExsheathment, data = no_cycle1)
summary(model_N)

model_O <- lm(LarvaeAge + Cycle ~ Hatchability, data = no_cycle1)
summary(model_O)

model_P <- lm(LarvaeAge + Cycle ~ InVitroViability + InVitroExsheathment + 
                InVivoViability + InVivoExsheathment + Hatchability, data = no_cycle1)
summary(model_P)

model_Q <- lm(LarvaeAge + Cycle ~ InVivoExsheathment, data = no_cycle1)
summary(model_Q)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Graphs
#Cycle 1
ggplot(model_A1, aes(LarvaeAge, InVitroViability)) + geom_point() + geom_smooth()
ggplot(model_B1, aes(LarvaeAge, InVitroExsheathment)) + geom_point() + geom_smooth()
ggplot(model_C1, aes(LarvaeAge, InVivoViability)) + geom_point() + geom_smooth()
ggplot(model_D1, aes(LarvaeAge, InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_E1, aes(LarvaeAge, Hatchability)) + geom_point() + geom_smooth()
ggplot(model_F1, aes(LarvaeAge, InVitroViability + InVitroExsheathment)) + geom_point() + geom_smooth()
ggplot(model_G1, aes(LarvaeAge, InVivoViability + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_H1, aes(LarvaeAge, InVivoViability + InVitroViability)) + geom_point() + geom_smooth()
ggplot(model_I1, aes(LarvaeAge, InVitroExsheathment + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_J1, aes(LarvaeAge, InVitroViability + InVitroExsheathment + InVivoViability + InVivoExsheathment + Hatchability)) + geom_point() + geom_smooth()

#Cycle 2
ggplot(model_A2, aes(LarvaeAge, InVitroViability)) + geom_point() + geom_smooth()
ggplot(model_B2, aes(LarvaeAge, InVitroExsheathment)) + geom_point() + geom_smooth()
ggplot(model_C2, aes(LarvaeAge, InVivoViability)) + geom_point() + geom_smooth()
ggplot(model_D2, aes(LarvaeAge, InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_E2, aes(LarvaeAge, Hatchability)) + geom_point() + geom_smooth()
ggplot(model_F2, aes(LarvaeAge, InVitroViability + InVitroExsheathment)) + geom_point() + geom_smooth()
ggplot(model_G2, aes(LarvaeAge, InVivoViability + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_H2, aes(LarvaeAge, InVivoViability + InVitroViability)) + geom_point() + geom_smooth()
ggplot(model_I2, aes(LarvaeAge, InVitroExsheathment + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_J2, aes(LarvaeAge, InVitroViability + InVitroExsheathment + InVivoViability + InVivoExsheathment + Hatchability)) + geom_point() + geom_smooth()

#Cycle 3
ggplot(model_A3, aes(LarvaeAge, InVitroViability)) + geom_point() + geom_smooth()
ggplot(model_B3, aes(LarvaeAge, InVitroExsheathment)) + geom_point() + geom_smooth()
ggplot(model_C3, aes(LarvaeAge, InVivoViability)) + geom_point() + geom_smooth()
ggplot(model_D3, aes(LarvaeAge, InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_E3, aes(LarvaeAge, Hatchability)) + geom_point() + geom_smooth()
ggplot(model_F3, aes(LarvaeAge, InVitroViability + InVitroExsheathment)) + geom_point() + geom_smooth()
ggplot(model_G3, aes(LarvaeAge, InVivoViability + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_H3, aes(LarvaeAge, InVivoViability + InVitroViability)) + geom_point() + geom_smooth()
ggplot(model_I3, aes(LarvaeAge, InVitroExsheathment + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_J3, aes(LarvaeAge, InVitroViability + InVitroExsheathment + InVivoViability + InVivoExsheathment + Hatchability)) + geom_point() + geom_smooth()

#Cycle 4
ggplot(model_A4, aes(LarvaeAge, InVitroViability)) + geom_point() + geom_smooth()
ggplot(model_B4, aes(LarvaeAge, InVitroExsheathment)) + geom_point() + geom_smooth()
ggplot(model_C4, aes(LarvaeAge, InVivoViability)) + geom_point() + geom_smooth()
ggplot(model_D4, aes(LarvaeAge, InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_E4, aes(LarvaeAge, Hatchability)) + geom_point() + geom_smooth()
ggplot(model_F4, aes(LarvaeAge, InVitroViability + InVitroExsheathment)) + geom_point() + geom_smooth()
ggplot(model_G4, aes(LarvaeAge, InVivoViability + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_H4, aes(LarvaeAge, InVivoViability + InVitroViability)) + geom_point() + geom_smooth()
ggplot(model_I4, aes(LarvaeAge, InVitroExsheathment + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_J4, aes(LarvaeAge, InVitroViability + InVitroExsheathment + InVivoViability + InVivoExsheathment + Hatchability)) + geom_point() + geom_smooth()

#Cycle 5
ggplot(model_A5, aes(LarvaeAge, InVitroViability)) + geom_point() + geom_smooth()
ggplot(model_B5, aes(LarvaeAge, InVitroExsheathment)) + geom_point() + geom_smooth()
ggplot(model_C5, aes(LarvaeAge, InVivoViability)) + geom_point() + geom_smooth()
ggplot(model_D5, aes(LarvaeAge, InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_E5, aes(LarvaeAge, Hatchability)) + geom_point() + geom_smooth()
ggplot(model_F5, aes(LarvaeAge, InVitroViability + InVitroExsheathment)) + geom_point() + geom_smooth()
ggplot(model_G5, aes(LarvaeAge, InVivoViability + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_H5, aes(LarvaeAge, InVivoViability + InVitroViability)) + geom_point() + geom_smooth()
ggplot(model_I5, aes(LarvaeAge, InVitroExsheathment + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(model_J5, aes(LarvaeAge, InVitroViability + InVitroExsheathment + InVivoViability + InVivoExsheathment + Hatchability)) + geom_point() + geom_smooth()

#Intercycle
ggplot(no_cycle1, aes(Cycle, InVitroViability + InVitroExsheathment + InVivoViability + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(no_cycle1, aes(LarvaeAge + Cycle, InVitroViability + InVitroExsheathment + InVivoViability + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(no_cycle1, aes(LarvaeAge + Cycle, InVitroViability + InVivoViability)) + geom_point() + geom_smooth()
ggplot(no_cycle1, aes(LarvaeAge + Cycle, InVitroExsheathment + InVivoExsheathment)) + geom_point() + geom_smooth()
ggplot(no_cycle1, aes(LarvaeAge + Cycle, Hatchability)) + geom_point() + geom_smooth()
ggplot(no_cycle1, aes(LarvaeAge + Cycle, InVitroViability + InVitroExsheathment + InVivoViability + InVivoExsheathment + Hatchability)) + geom_point() + geom_smooth()
