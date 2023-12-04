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

#load data
pkmn <- read.csv("pkmn.csv")

#show data
head(pkmn)
#Attack, Speed and HP are the base stats to evaluate each pokémon.

GGally::ggpairs(pkmn,columns = c(3:6), title = "Scatter Plot Matrix for Pokémon Dataset", axisLabels = "show") 

#At the beginning, we want to exclude the variable *Type*. 
#Multiple linear model
mod <- lm(Total~HP + Attack + Speed, data=pkmn)
summary(mod)

#added variable plot
avPlots(mod)

#diagnostic plot
par(mfrow=c(2,2))
plot(mod)

#test coefficients of Hp and speed
mod_1 <- lm(Total~Attack, data=pkmn) #restricted model
anova(mod_1, mod)

#test coefficient of speed
mod_2 <- lm(Total~Attack +HP, data=pkmn) #unconstrained model
anova(mod_2, mod)
#we will exclude speed

rm(mod, mod_2)

#ANCOVA

#relationship between total and type

theme_set(theme_bw())
ggplot(pkmn, aes(x=Type, y= Total, color=Type)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=8, outlier.size=4)

#marginal relationship among variables

GGally::ggpairs(pkmn,columns = c(2:5), 
                title = "Scatter Plot Matrix for Pokémon Dataset", 
                axisLabels = "show") 

#model
mod_anc_1 <- lm(Total~Attack +Type, data=pkmn)
summary(mod_anc_1)

#two estimated regression line
ggplot(pkmn, aes(x=Attack, y=Total, color=Type))+ geom_point() + geom_smooth(method = "lm", aes(fill=Type))

#adding an interaction
mod_anc_2 <- lm(Total~ Attack + Type + Type:Attack, data=pkmn)
summary(mod_anc_2)

#compare these two models
anova(mod_anc_1, mod_anc_2)

#compare means
anova(mod_1, mod_anc_2)

#diagnostic plot Ancova with interaction
plot(mod_anc_2)

rm(list=ls())

#####ANOVA

active_data <- read.csv("active.csv")
head(active_data)

active_data$sex <- as.factor(active_data$sex)
active_data$group <- as.factor(active_data$group)

#marginal relationship among variables
GGally::ggpairs(active_data,columns = c(10,6,4), 
                title = "Scatter Plot Matrix for Active Dataset", 
                axisLabels = "show") 

#Combined plot
ggplot(active_data,  aes(x = factor(group), y = hvltt2, fill = factor(sex)))  + geom_bar(stat="identity", position = "dodge", width = 0.7) + labs(x = "Group", y = "Memory performance", fill="Sex")  + scale_x_discrete(breaks=c("1", "2", "3", "4"), labels=c("Memory", "Reasoning", "Speed", "Control")) + scale_fill_discrete(breaks=c("1", "2"), labels=c("Male", "Female"))

#model
model<-lm(hvltt2~group +sex,data=active_data)
summary(model)

#evaluating interaction graphically

ggplot(active_data,aes(group, hvltt2, fill=sex)) + 
  stat_summary(aes(group = sex, lty = sex), fun = "mean",  geom = "line")+ labs(x = "Group", y = "Memory performance - Average", fill="Sex")  + scale_x_discrete(breaks=c("1", "2", "3", "4"), labels=c("Memory", "Reasoning", "Speed", "Control")) + scale_fill_discrete(breaks=c("1", "2"), labels=c("Male", "Female"))

#two-way anova with interaction
mod_int<-lm(hvltt2~group +sex+ group:sex,data=active_data)
summary(mod_int)