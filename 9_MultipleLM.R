################################################################################
#                                                                              #
#                     Multiple linear regression models                        #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################
#load libraries
library(car)
require(ggplot2)
library(GGally)

#load data
pkmn <- read.csv("C:/Users/valen/Desktop/PhD/stat_modelling/exercises/pkmn.csv")

#show data
head(pkmn)
#Attack, Speed and HP are the base stats to evaluate each pokémon.

ggpairs(pkmn,columns = c(3:6), 
        title = "Scatter Plot Matrix for Pokémon Dataset", 
        axisLabels = "show") 

#At the beginning, we want to exclude the variable *Type*. 
#Multiple linear model
mod <- lm(Total~HP + Attack + Speed, data=pkmn)
summary(mod)

#diagnostic plot
par(mfrow=c(2,2))
plot(mod)


#ANCOVA

mod_2 <- lm(Total~HP + Attack + Speed + Type, data=pkmn)
summary(mod_2)
plot(mod_2)

mod_3 <- lm(Total~HP + Attack + Speed + Type + Type:Attack, data=pkmn)
summary(mod_3)
plot(mod_3)
