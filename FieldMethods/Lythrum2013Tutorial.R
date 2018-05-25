LyDat<-read.csv("./Lythrum2013Data.csv")

View(LyDat) # Inspect the data

str(LyDat) # Structure of the data

library(ggplot2) # Import ggplot2 library

# Scatterplot of infructescence biomass by number of fruits
qplot(x=InfMass07,y=Fruits07,data=LyDat,colour=Site)

LyDat$Site[136:285] # Subset only KSR sites in the Site column

KSR<-LyDat[136:285,] # Subset only KSR sites in the LyDat data.frame object

# Plot only KSR data
qplot(x=InfMass07,y=Fruits07,data=KSR,colour=Site) + theme_classic() +
  geom_smooth(method="lm")

# Estimate the equation for the line of Fruits ~ InfMass07
mod1<-lm(Fruits07~InfMass07,data=KSR)
summary(mod1)

# Add a new column to LyDat containing total Infructescence biomass for 2007-2010
LyDat$InfMassTot<-LyDat$InfMass07+LyDat$InfMass08+LyDat$InfMass09+LyDat$InfMass10

# Problem: Adding columns with NA returns NA

# New data frame replacing NAs with zero
LyDat0<-LyDat
LyDat0[is.na(LyDat0)]<-0

# Use LyDat0 as an index for LyDat
LyDat$InfMassTot<-LyDat0$InfMass07+LyDat0$InfMass08+LyDat0$InfMass09+LyDat0$InfMass10

# Add Column for Total Fruit Number called FruitTot
LyDat$FruitTot<-LyDat$InfMassTot*26.6-53.986
# Add a column for relative fitness
LyDat$RelFit<-LyDat$FruitTot/mean(LyDat$FruitTot,na.rm=T) 
# na.rm=T removes NAs when calculating the mean. 
# Otherwise it will return NA instead of the mean.

# Visualize Relative Fitness for Flowering Date in 2010
qplot(x=Flwr10,y=RelFit,data=LyDat)+geom_smooth()
qplot(x=Flwr10,y=RelFit,data=LyDat)+geom_smooth(method=lm)

# Test for directional selection
# IMPORTANT: Must use relative fitness (divided by mean fitness)

# Test for stabilizing selection using a quadratic equation
mod2<-lm(RelFit~Flwr10+I(Flwr10^2),data=LyDat)
summary(mod2)

Origin<-gsub("\\d","",LyDat$Mat)
Origin<-gsub("[A-C]","1_North",Origin)
Origin<-gsub("[E-J]","2_Mid",Origin)
Origin<-gsub("[S-T]","3_South",Origin)

LyDat$Origin<-Origin

# Test for local adaptation
mod3<-lm(log(RelFit+1)~Site+Origin+Site:Origin,data=LyDat)
# Shorthand
mod3<-lm(log(RelFit+1)~Site*Origin,data=LyDat)
summary(mod3)
qplot(x=Origin,y=RelFit,data=LyDat,geom="boxplot")
# Inspect distribution of RelFit
qplot(x=log(RelFit+1),data=LyDat,facets=Origin~.)+theme_classic()

# Approximate local adaptation figure
qplot(x=Site,y=RelFit,data=LyDat,geom="boxplot",
      facets=.~Origin,shape=Site,fill=Site)+theme_classic()

# Mixed Effects
library(lme4)
# Mixed effects models include random effects
# Random effects are added with +()
LMM1<-lmer(log(RelFit+1)~Site*Origin+(1|Mat),data=LyDat)
summary(LMM1)
# Compare to a simple model
LMM0<-lmer(log(RelFit+1)~1+(1|Mat),data=LyDat)
summary(LMM0)
# Different among-family variances for each Origin
LMM2<-lmer(log(RelFit+1)~Site*Origin+(Origin|Mat),data=LyDat)
summary(LMM2)

# Do origins differ in among-family variance?
# Use a likelihood ratio test
anova(LMM2,LMM1)


library(lme4)
lmer(log(RelFit+1)~Site*Origin+(1|Mat),data=LyDat)


