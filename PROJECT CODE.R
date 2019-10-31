ojdata= read.csv("R data.csv", header=TRUE, sep=",")
attach(ojdata)

## How does the demand for a brand depend on price? What is the price elasticity of demand of a brand?
## Is price elasticity different for different brands?
  
modelprice=lm(logMOVE~logPRICE+BRAND*logPRICE)
summary(modelprice)

## Test linear Hypothesis, p<0.05, coefficients for 3 brands are different
c1=c(0,0,0,0,1,0)
c2=c(0,0,0,0,0,1)
h=rbind(c1,c2)
r=c(0,0)
library(car)
linearHypothesis(modelprice,h,rhs=r)

## How does demand depend on whether the product is on sale (Feat =1)? Is this dependence same for all brands?
##Overall regardless Brand
modelfeat=lm(logMOVE~Feat+BRAND*Feat)
summary(modelfeat)

## Test linear Hypothesis, p<0.05, coefficient for 3 brands are different
c1=c(0,0,0,0,1,0)
c2=c(0,0,0,0,0,1)
h=rbind(c1,c2)
r=c(0,0)
library(car)
linearHypothesis(modelfeat,h,rhs=r)

##demand for a brand depend on the price of another brand, same storeweek, data compiled from ACCESS, use p value to explain
demandbyotherbrands= read.csv("FG TROP GV PURE STOREWEEK.csv", header=TRUE, sep=",")
FGlogMOVE=demandbyotherbrands$FG_logMOVE
FGlogPRICE=demandbyotherbrands$FG_logPRICE
TROPPURElogPRICE=demandbyotherbrands$logPRICE
TROPGVlogPRICE=demandbyotherbrands$TROP.GV_logPRICE
modellogMOVEFG=lm(FGlogMOVE~FGlogPRICE+TROPGVlogPRICE+TROPPURElogPRICE)
summary(modellogMOVEFG)

TROPGVlogMOVE=demandbyotherbrands$TROP.GV_logMOVE
modellogMOVETROPGV=lm(TROPGVlogMOVE~TROPGVlogPRICE+FGlogPRICE+TROPPURElogPRICE)
summary(modellogMOVETROPGV)

TROPPURElogMOVE=demandbyotherbrands$logMOVE
modellogMOVETROPPURE=lm(TROPPURElogMOVE~TROPGVlogPRICE+FGlogPRICE+TROPPURElogPRICE)
summary(modellogMOVETROPPURE)

## Demo original, use P value to explain the significance
modeldemo=lm(logMOVE~logPRICE+Feat+BRAND*logPRICE+AGE9+AGE60+HH3PLUS+HH4PLUS+HHLARGE+HSIZEAVG+HHSINGLE+HVAL150+HVAL200+INCOME+MORTGAGE+NOCAR+NWHITE+POVERTY+RETIRED+SINGLE+UNEMP+WORKWOM+SSTRDIST+SSTRVOL+CPDIST5+CPWVOL5)
summary(modeldemo)


## Addintional Analysis:create model based on 2/3 of observations and Validate model use the rest of 1/3 of the observation
finalmodel=lm(logMOVE[1:41590]~logPRICE[1:41590]+Feat[1:41590]+BRAND[1:41590]*logPRICE[1:41590]+AGE9[1:41590]+AGE60[1:41590]+HH3PLUS[1:41590]+HH4PLUS[1:41590]+HHLARGE[1:41590]+HSIZEAVG[1:41590]+HHSINGLE[1:41590]+HVAL150[1:41590]+HVAL200[1:41590]+INCOME[1:41590]+MORTGAGE[1:41590]+NOCAR[1:41590]+NWHITE[1:41590]+POVERTY[1:41590]+RETIRED[1:41590]+SINGLE[1:41590]+UNEMP[1:41590]+WORKWOM[1:41590]+SSTRDIST[1:41590]+SSTRVOL[1:41590]+CPDIST5[1:41590]+CPWVOL5[1:41590])
summary(finalmodel)



finalmodelreduced1=lm(logMOVE[1:41590]~logPRICE[1:41590]+Feat[1:41590]+BRAND[1:41590]*logPRICE[1:41590]+AGE9[1:41590]+AGE60[1:41590]+HH3PLUS[1:41590]+HH4PLUS[1:41590]+HHLARGE[1:41590]+HSIZEAVG[1:41590]+HHSINGLE[1:41590]+HVAL150[1:41590]+HVAL200[1:41590]+INCOME[1:41590]+MORTGAGE[1:41590]+NOCAR[1:41590]+NWHITE[1:41590]+POVERTY[1:41590]+RETIRED[1:41590]+SINGLE[1:41590]+WORKWOM[1:41590]+SSTRDIST[1:41590]+SSTRVOL[1:41590]+CPDIST5[1:41590]+CPWVOL5[1:41590])
summary(finalmodelreduced1)

finalmodelreduced2=lm(logMOVE[1:41590]~logPRICE[1:41590]+Feat[1:41590]+BRAND[1:41590]*logPRICE[1:41590]+AGE60[1:41590]+HH3PLUS[1:41590]+HH4PLUS[1:41590]+HHLARGE[1:41590]+HSIZEAVG[1:41590]+HHSINGLE[1:41590]+HVAL150[1:41590]+HVAL200[1:41590]+MORTGAGE[1:41590]+NOCAR[1:41590]+NWHITE[1:41590]+POVERTY[1:41590]+SINGLE[1:41590]+WORKWOM[1:41590]+SSTRDIST[1:41590]+SSTRVOL[1:41590]+CPDIST5[1:41590]+CPWVOL5[1:41590])
summary(finalmodelreduced2)

LogMOVETP=logMOVE[1:41590]
LogPRICETP=logPRICE[1:41590]
FeatTP=Feat[1:41590]
BRANDTP=BRAND[1:41590]
AGE60TP=AGE60[1:41590]
HH3PLUSTP=HH3PLUS[1:41590]
HH4PLUSTP=HH4PLUS[1:41590]
HHLARGETP=HHLARGE[1:41590]
HSIZEAVGTP=HSIZEAVG[1:41590]
HHSINGLETP=HHSINGLE[1:41590]
HVAL150TP=HVAL150[1:41590]
HVAL200TP=HVAL200[1:41590]
MORTGAGETP=MORTGAGE[1:41590]
NOCARTP=NOCAR[1:41590]
NWHITETP=NWHITE[1:41590]
POVERTYTP=POVERTY[1:41590]
SINGLETP=SINGLE[1:41590]
WORKWOMTP=WORKWOM[1:41590]
SSTRDISTTP=SSTRDIST[1:41590]
SSTRVOLTP=SSTRVOL[1:41590]
CPDIST5TP=CPDIST5[1:41590]
CPWVOL5TP=CPWVOL5[1:41590]

finalmodelreduced2=lm(LogMOVETP~logPRICETP+FeatTP+BRANDTP*LogPRICETP+AGE60TP+HH3PLUSTP+HH4PLUSTP+HHLARGETP+HSIZEAVGTP+HHSINGLETP+HVAL150TP+HVAL200TP+MORTGAGETP+NOCARTP+NWHITETP+POVERTYTP+SINGLETP+WORKWOMTP+SSTRDISTTP+SSTRVOLTP+CPDIST5TP+CPWVOL5TP)
summary(finalmodelreduced2)


## all x variables are significant, we are going to validate the model by using the rest of 1/3 of observation

validation=read.csv("validation data.csv", header=TRUE, sep=",")
Predict(finalmodelreduced2, interval = "prediction", level = .99, newdata = validation)


