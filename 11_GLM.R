#load libraries
library(car)
library(ggplot2)

#LOGISTIC REGRESSION

#load and show data

icu <- read.csv("ICU.csv")
head(icu)

#checking normal distribution

hist(icu$stato,prob=T, xlim=c(-1,1), main= "Histogram of Status")
curve(dnorm(x,mean(icu$stato), sd(icu$stato)),add=T, col=2)

qqnorm(icu$stato)

#plot dependent variable

icu$stato <- as.factor(icu$stato)
icu$causa <- as.factor(icu$causa)
icu$coscienza <- as.factor(icu$coscienza)

ggplot(data=icu, aes(x=stato)) +
  geom_bar(fill="pink") + labs(x = "Status") +
  theme_minimal()

#transform to grouped dat

icu$cleta <- ifelse(icu$eta < 70, 0, 1)

table(icu$stato[which(icu$cleta==0 & icu$causa == 0 & icu$coscienza == 0)])
table(icu$stato[which(icu$cleta==0 & icu$causa == 0 & icu$coscienza == 1)])
table(icu$stato[which(icu$cleta==0 & icu$causa == 1 & icu$coscienza == 0)])
table(icu$stato[which(icu$cleta==0 & icu$causa == 1 & icu$coscienza == 1)])
table(icu$stato[which(icu$cleta==1 & icu$causa == 0 & icu$coscienza == 0)])
table(icu$stato[which(icu$cleta==1 & icu$causa == 0 & icu$coscienza == 1)])
table(icu$stato[which(icu$cleta==1 & icu$causa == 1 & icu$coscienza == 0)])
table(icu$stato[which(icu$cleta==1 & icu$causa == 1 & icu$coscienza == 1)])


ICU.binomial <- data.frame(cleta=c(0,0,0,0,1,1,1,1),
                           causa=c(0,0,1,1,0,0,1,1),
                           coscienza=c(0,1,0,1,0,1,0,1),
                           morti=c(0,0,9,13,0,5,7,6),
                           ni=c(25,6,73,32,15,10,28,11))

#logistic model

mod_glm <- glm(I(morti/ni) ~ cleta + causa + coscienza, family="binomial", weights = ni, data=ICU.binomial)
summary(mod_glm)

#odds ratio

# age

exp(coefficients(mod_glm)[2])

# causa

exp(coefficients(mod_glm)[3])

#null model

mod_0 <- glm(I(morti/ni) ~ 1, family="binomial", weights = ni, data=ICU.binomial)
summary(mod_0)

#test about overall significance

(W <- 2*(as.numeric(logLik(mod_glm)) - as.numeric(logLik(mod_0)))) #wobs

#pvalue

1-pchisq(W,3) #we reject h0

#evaluating predictions

(predicted <- ifelse(as.numeric(mod_glm$fitted.values) >= 0.5, ICU.binomial$ni, 0))
ICU.binomial$morti

#Confusion matrix
(true_positive<- sum(ICU.binomial$morti[predicted != 0]))
(false_positive <- sum(ICU.binomial$ni[predicted != 0]) - sum(ICU.binomial$morti[predicted!= 0]))

(true_negative <- 160 - false_positive)
(false_negative <- 40 - true_positive)

#accuracy

(6 + 155)/(6+155+5+34)

#sensitivity

6/(6+34)

#specificity

155/(155+5)

#####POISSON REGRESSION

#load and show data

crabs <- read.csv("Granchi.csv")
head(crabs)

#check normal distribution

hist(crabs$Satellites,prob=T,xlim=c(-3,18), main= "Histogram of Satellites")
curve(dnorm(x,mean(crabs$Satellites),sd(crabs$Satellites)),col=2, add=T)

qqnorm(crabs$Satellites)

#data exploration

theme_set(theme_bw()) 
ggplot(data = crabs, aes(x=Width, y=Satellites)) + geom_point( alpha = 0.5, aes(color= factor(Dark))) + labs(x="Crabs' Width", y="Satellites", color="Dark", title = "Plot Satellites vs Width & Dark")

theme_set(theme_bw()) 
ggplot(data = crabs, aes(x=Width, y=Satellites)) + geom_point( alpha = 0.5, aes(color= factor(GoodSpine))) + labs(x="Crabs' Width", y="Satellites", color="GoodSpine", title = "Plot Satellites vs Width & GoodSpine")

#check poisson distribution

# Graficamente:
tab <- xtabs(~crabs$Satellites)
par(mfrow=c(1,2))
ascisse<-as.numeric(names(tab))

# Empirical distribution
plot(ascisse,tab,type="h",xlab="Number of Satellites",ylab="Frequence", main = "Empirical distribution")
points(ascisse,tab,pch=16)

# Theoretical distribution
media<-mean(crabs$Satellites)
camp <-rpois(1000,media)
tab.camp<-xtabs(~camp)
ascisse.camp<-as.numeric(names(tab.camp))
plot(ascisse.camp,tab.camp,type="h",xlab="Number of Satellites",ylab="Frequence",main = "Theoretical distribution")
points(ascisse.camp,tab.camp,pch=16)
par(mfrow=c(1,1))

#qqplot
qqPlot(crabs$Satellites,distribution="pois",lambda=mean(crabs$Satellites))

#poisson model

mod_glm <- glm(Satellites ~ Width + Dark + GoodSpine, family=poisson, data=crabs)
summary(mod_glm)

#interpretation of width coefficient

exp(coefficients(mod_glm)[2])

#null model

mod_0 <- glm(Satellites ~ 1, family=poisson, data=crabs)
summary(mod_0)

#test about overall significance

(W <- 2*(as.numeric(logLik(mod_glm)) - as.numeric(logLik(mod_0)))) #wobs

#pvalue

1-pchisq(W,3) #we reject h0

#or
anova(mod_0,mod_glm,test="Chisq")





