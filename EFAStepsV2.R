install.packages("psych")
library(psych)

install.packages("lavaan")
library(lavaan)

install.packages("GPArotation")
library(GPArotation)

setwd("/Volumes/labshare/Experiments/DEAR /Dialectical Beliefs Studies/Dialectical Emotions Scale Validation Data")
#setwd("C:/Users/marqschieber/Library/Mobile Documents/com~apple~CloudDocs/Wake Forest Psychology/Waugh Research/DEAR Data /Dialectical Emotions Scale Validation Data/DES validation data.sav")

## read in posdistract data from 206 subjects for EFA

dat <- read.csv('DES_EFA_3:2:22.csv')

##get descriptives on all the items

describe(dat)

##exclude items with poor descriptives

des <-data.frame(dat$DES_1CPN, dat$DES_2NPS, dat$DES_3NPS, dat$DES_4NPS, dat$DES_5CPN, dat$DES_6CPN, dat$DES_7PNS, dat$DES_8CPN,
                 dat$DES_9CPN, dat$DES_10PNS, dat$DES_11PNS, dat$DES_12CPN, dat$DES_13PNS, dat$DES_14NPS, dat$DES_15NPS,
                 dat$DES_16CPN, dat$DES_17PNS, dat$DES_18PNS)

## determine number of factors using parallel and VSS (velicer MAP, BIC, etc.) nusing maximum likelihood

fa.parallel(des, fm="mle", fa="fa")

VSS(des, n=3, rotate="promax", fm="ml")

## factor analysis using ML, rotate = promax

efa3factorPromaxML <- fa(des, nfactors=3, rotate="promax", fm="mle")

## identify those variables with loadings > .6 on one of the factors and formed new data matrix with them

des11 <- data.frame(dat$DES_2NPS, dat$DES_3NPS, dat$DES_4NPS, dat$DES_6CPN, dat$DES_7PNS, dat$DES_8CPN,
                    dat$DES_9CPN, dat$DES_10PNS, dat$DES_13PNS,
                    dat$DES_16CPN, dat$DES_17PNS)

## redo FA on new 12 items 

fa.parallel(des11, fm="mle", fa="fa")

VSS(des11, n = 3, rotate="promax", fm="ml")


efa3factorPromaxML11 <- fa(des11, nfactors=3, rotate="promax", fm="mle")

## get DES scales into data frames

DES_NPS <- rowMeans(cbind(dat$DES_2NPS, dat$DES_3NPS, dat$DES_4NPS))
DES_PNS <- rowMeans(cbind(dat$DES_7PNS, dat$DES_10PNS, dat$DES_13PNS, dat$DES_17PNS))
DES_CPN <- rowMeans(cbind(dat$DES_6CPN, dat$DES_8CPN, dat$DES_9CPN, dat$DES_16CPN))

## get other scales into data frames

DTI_PFD <- rowMeans(cbind(dat$DTI_10PFD, dat$DTI_13PFD, dat$DTI_1PFD, dat$DTI_4PFD, dat$DTI_7PFD))
DTI_DB <- rowMeans(cbind(dat$DTI_2DB, dat$DTI_5DB, dat$DTI_8DB, dat$DTI_11DB, dat$DTI_14DB))
DTI_PLT <-rowMeans(cbind(dat$DTI_3PLT, dat$DTI_6PLT, dat$DTI_9PLT, dat$DTI_12PLT, dat$Q1_15PLT)) 

DSS_C <- rowMeans(cbind(dat$DSS_2C, dat$DSS_6C, dat$DSS_12C, dat$DSS_15C, dat$DSS_20C, dat$DSS_22C, dat$DSS_24C, dat$DSS_25C, dat$DSS_26C, dat$DSS_27C, dat$DSS_28C, dat$DSS_31C, dat$DSS_32C))
DSS_CC <- rowMeans(cbind(dat$DSS_4CC, dat$DSS_7CC, dat$DSS_8CC, dat$DSS_9CC, dat$DSS_13CC, dat$DSS_14CC, dat$DSS_18CC, dat$DSS_19CC, dat$DSS_21CC, dat$DSS_29CC, dat$DSS_30CC))
DSS_BC <- rowMeans(cbind(dat$DSS_1BC, dat$DSS_3BC, dat$DSS_5BC, dat$DSS_10BC, dat$DSS_11BC, dat$DSS_16BC, dat$DSS_17BC, dat$DSS_23BC))

## correlations among the scales
SitPos <- dat$Situationrating_P
SitNeg <- dat$Situationrating_N
corr.test(cbind(DES_NPS, DES_PNS, DES_CPN, DTI_PFD, DTI_DB, DTI_PLT, DSS_C, DSS_CC, DSS_BC, SitNeg, SitPos))
                         
desSitPos <- lm(SitPos ~ DES_PNS + SitNeg)
                         