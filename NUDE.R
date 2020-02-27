#NUDE Group Project

#Background: plates that they pre-seeded with perct initial cover and initial richness (specific species), and proportion of species that are native
#T1: recruits from the start
#T84: cumulative # recruits, 7 time points
#blocks: spatial treatments, places the plates were placed
#tile ID: so oyu can track same tile across 

####Question 1:
#How does recruitment of all species vary over time by 1) assemblage diversity, 2) bare space of assemblage, 3) % native species

library(tidyverse)
library(lme4)

setwd("...")

#Question 1
#read in data
dataT<- read.csv("cumrecruitTreatments_MM.csv")
names(dataT)
#fix data
#convert block to factor, percents to decimals
dataT$Block<- as.factor(dataT$Block)
dataT$TileID<- as.factor(dataT$"Treatment.number")
dataT$InitialCover<- (dataT$InitialCover)/100
dataT$InitialNative<- (dataT$InitialNative)/100
str(dataT)


###start by visualizing to know what matters vs make models with and without blocks and then decide
#Does block need to be included in our model?

#visualize
ggplot(data=dataT, aes(x=Time, y = Recruits, color = InitialNative)) +
  geom_jitter()

#more cover when you start, then less recruits
#richness changes over time
#initial native is mixed together


#models
#nest in block as a random effect
#what distribution for our count data
#negative binomial for a long tail, there are a few points up at 100, do those all belong to the same block? r poisson? What is causing zeros? Are the zeros because they didn't even have opportunities to get recruits? Poisson alone doesn't encorporate zeros very well. What do we want to do with the outliers- they may get soaked up by our random effect.
hist(dataT$Recruits)

#simple mixed model (poisson) with all the variables
# recruits are function of main effects plus random intercept in which individual tiles are nested within each Block. "1|" means it's a random intercept. This it standard if I want a random intercept, if you want a random slope, code it different 
simple<- glmer(Recruits~Time+InitialCover+InitialRichness+InitialNative + (1|Block/TileID), family = "poisson", data = dataT)
#error we need to rescale variables to z scores bc the initial data are all on different scales (except don't rescale time)
summary(simple) #shows us the correlation matrix

#re-scale
dataTz<- data.frame(matrix(NA, nrow = length(dataT[,1]), ncol = length(dataT[1,])))
for (c in 1:length(dataT[1,])){
  dataTz[,c]<- scale(dataT[,c])
}
colnames(dataTz)<- colnames(dataT)

dataTz$Time.1<- dataT$Time
dataTz$Block.1<- dataT$Block
dataTz$TileID.1<- dataT$TileID
dataTz$Recruits.1<- dataT$Recruits
str(dataTz)

#re-try poisson model with re-scaled variables
simple.2<- glmer(Recruits.1~Time.1+InitialCover+InitialRichness+InitialNative + (1|Block.1/TileID.1), family = "poisson", data = dataTz)
#still doesn't like it

#re-try as a negative binomial, un-scaled
simple.nb<- glmer.nb(Recruits~Time+InitialCover+InitialRichness+InitialNative + (1|Block/TileID), data = dataT)
#didn't like that, wanted it to be scaled

#re-try negative binomial, scaled
simple.nb.2<- glmer.nb(Recruits.1~Time.1+InitialCover+InitialRichness+InitialNative + (1|Block.1/TileID.1), data = dataTz)

#large eigenvalue means it needs more re-scaling or it just isn't fitting the data well
#we can plot the residuals to see how the data actually fits the model

#residuals from simplest poisson model
plot(simple) #this pattern means a lot of values are close to zero, not many things much larger than zero
qqnorm(residuals(simple)) #unclear if it's really necessary to do this plot or if it means anything for glmms

#maybe it's not working because we didn't transform the data. Since the data aren't linear, we need to make them linear, compared to our explanatory variable
dataT$Recruits.log<- log(dataT$Recruits+1)
hist(dataT$Recruits.log) #these data look much better
#now that the data are transformed, the zeros are still a problem, but the rest of the data looks normal. The problem is that the ggplot still isn't linear, maybe log(time) help?
dataT$Time.log<- log(data$Time)

ggplot(data=dataT, aes(x=Time.log, y = Recruits.log, color = Block)) +
  geom_jitter()

#model with log for both to see what happens
simple.log<- glmer(Recruits.log~Time.log+InitialCover+InitialRichness+InitialNative + (1|Block/TileID), family = "poisson", data = dataT)
summary(simple.log)

# make a zero inflated model
#install.packages("glmmTMB")
library(glmmTMB)


