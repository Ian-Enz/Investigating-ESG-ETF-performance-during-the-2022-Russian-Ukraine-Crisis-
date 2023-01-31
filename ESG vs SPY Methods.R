library (quantmod)
library (moments)
library (ggplot2)
library (ggcorrplot)

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis Drive/R Studio/ESG vs SPY")
factors <- read.csv("factors.csv", header = FALSE)
momentum <- read.csv("momentum.csv", header = FALSE)

dt1.0 <- "2021-8-26"  #how rigorous in our estimation do we want to be vs. how much is enough?
dt1.1 <- "2022-2-16" #how much earlier than the event do we want to go? 5 days?
dt2.0 <- "2022-2-16" #on the event (before)
dt2.1 <- "2022-3-4" #on the event (after)

####Define ETFs and Benchmark for both estimation and event windows####
  #Factors and Momentum are from French's data library http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
#Estimation Window
SPY1 <- getSymbols.yahoo("^GSPC", from = dt1.0, to = dt1.1, auto.assign = F)
  SMB1 <- factors$V3[c(14643:14762)]
  HML1 <- factors$V4[c(14643:14762)]
  RMW1 <- factors$V5[c(14643:14762)]
  CMA1 <- factors$V6[c(14643:14762)]
  WML1 <- momentum$V2[c(24971:25090)]
SNPE1 <- getSymbols.yahoo("SNPE", from = dt1.0, to = dt1.1, auto.assign = F)
ESGE1 <- getSymbols.yahoo("ESGE", from = dt1.0, to = dt1.1, auto.assign = F)
ESGD1 <- getSymbols.yahoo("ESGD", from = dt1.0, to = dt1.1, auto.assign = F)
ESGU1 <- getSymbols.yahoo("ESGU", from = dt1.0, to = dt1.1, auto.assign = F)
NUMG1 <- getSymbols.yahoo("NUMG", from = dt1.0, to = dt1.1, auto.assign = F)

#Event Window
SPY2 <- getSymbols.yahoo("^GSPC", from = dt2.0, to = dt2.1, auto.assign = F)
  SMB2 <- factors$V3[c(14763:14773)]
  HML2 <- factors$V4[c(14763:14773)]
  RMW2 <- factors$V5[c(14763:14773)]
  CMA2 <- factors$V6[c(14763:14773)]
  WML2 <- momentum$V2[c(25091:25101)]
SNPE2 <- getSymbols.yahoo("SNPE", from = dt2.0, to = dt2.1, auto.assign = F)
ESGE2 <- getSymbols.yahoo("ESGE", from = dt2.0, to = dt2.1, auto.assign = F)
ESGD2 <- getSymbols.yahoo("ESGD", from = dt2.0, to = dt2.1, auto.assign = F)
ESGU2 <- getSymbols.yahoo("ESGU", from = dt2.0, to = dt2.1, auto.assign = F)
NUMG2 <- getSymbols.yahoo("NUMG", from = dt2.0, to = dt2.1, auto.assign = F)

#Merge data into a single data table
tickers1 <- c("SPY1", "SMB1", "HML1", "RMW1", "CMA1", "WML1", "SNPE1", "ESGE1", "ESGD1", "ESGU1", "NUMG1")
tickers2 <- c("SPY2", "SMB2", "HML2", "RMW2", "CMA2", "WML2", "SNPE2", "ESGE2", "ESGD2", "ESGU2", "NUMG2")

dailyreturns1 <- cbind(dailyReturn(SPY1),
                       SMB1,
                       HML1,
                       RMW1,
                       CMA1,
                       WML1,
                       dailyReturn(SNPE1),
                       dailyReturn(ESGE1),
                       dailyReturn(ESGD1),
                       dailyReturn(ESGU1),
                       dailyReturn(NUMG1))
dailyreturns2 <- cbind(dailyReturn(SPY2),
                       SMB2,
                       HML2,
                       RMW2,
                       CMA2,
                       WML2,
                       dailyReturn(SNPE2),
                       dailyReturn(ESGE2),
                       dailyReturn(ESGD2),
                       dailyReturn(ESGU2),
                       dailyReturn(NUMG2))

colnames(dailyreturns1) <- tickers1
colnames(dailyreturns2) <- tickers2

####Find Estimate Values; estimated alphas and betas####
#SNPE
  SNPE1_CAPM <- lm(SNPE1 ~ SPY1, data = dailyreturns1)
  SNPE1_FF3 <-  lm(SNPE1 ~ SPY1 + SMB1 + HML1, data = dailyreturns1)
  SNPE1_CarH <- lm(SNPE1 ~ SPY1 + SMB1 + HML1 + WML1, data = dailyreturns1)
  SNPE1_FF5 <-  lm(SNPE1 ~ SPY1 + SMB1 + HML1 + RMW1 + CMA1, data = dailyreturns1)
  SNPE1_FF5m <- lm(SNPE1 ~ SPY1 + SMB1 + HML1 + RMW1 + CMA1 + WML1, data = dailyreturns1)
#ESGE
  ESGE1_CAPM <- lm(ESGE1 ~ SPY1, data = dailyreturns1)
  ESGE1_FF3 <-  lm(ESGE1 ~ SPY1 + SMB1 + HML1, data = dailyreturns1)
  ESGE1_CarH <- lm(ESGE1 ~ SPY1 + SMB1 + HML1 + WML1, data = dailyreturns1)
  ESGE1_FF5 <-  lm(ESGE1 ~ SPY1 + SMB1 + HML1 + RMW1 + CMA1, data = dailyreturns1)
  ESGE1_FF5m <- lm(ESGE1 ~ SPY1 + SMB1 + HML1 + RMW1 + CMA1 + WML1, data = dailyreturns1)
#ESGD
  ESGD1_CAPM <- lm(ESGD1 ~ SPY1, data = dailyreturns1)
  ESGD1_FF3 <-  lm(ESGD1 ~ SPY1 + SMB1 + HML1, data = dailyreturns1)
  ESGD1_CarH <- lm(ESGD1 ~ SPY1 + SMB1 + HML1 + WML1, data = dailyreturns1)
  ESGD1_FF5 <-  lm(ESGD1 ~ SPY1 + SMB1 + HML1 + RMW1 + CMA1, data = dailyreturns1)
  ESGD1_FF5m <- lm(ESGD1 ~ SPY1 + SMB1 + HML1 + RMW1 + CMA1 + WML1, data = dailyreturns1)
#ESGU
  ESGU1_CAPM <- lm(ESGU1 ~ SPY1, data = dailyreturns1)
  ESGU1_FF3 <-  lm(ESGU1 ~ SPY1 + SMB1 + HML1, data = dailyreturns1)
  ESGU1_CarH <- lm(ESGU1 ~ SPY1 + SMB1 + HML1 + WML1, data = dailyreturns1)
  ESGU1_FF5 <-  lm(ESGU1 ~ SPY1 + SMB1 + HML1 + RMW1 + CMA1, data = dailyreturns1)
  ESGU1_FF5m <- lm(ESGU1 ~ SPY1 + SMB1 + HML1 + RMW1 + CMA1 + WML1, data = dailyreturns1)
#NUMG
  NUMG1_CAPM <- lm(NUMG1 ~ SPY1, data = dailyreturns1)
  NUMG1_FF3 <-  lm(NUMG1 ~ SPY1 + SMB1 + HML1, data = dailyreturns1)
  NUMG1_CarH <- lm(NUMG1 ~ SPY1 + SMB1 + HML1 + WML1, data = dailyreturns1)
  NUMG1_FF5 <-  lm(NUMG1 ~ SPY1 + SMB1 + HML1 + RMW1 + CMA1, data = dailyreturns1)
  NUMG1_FF5m <- lm(NUMG1 ~ SPY1 + SMB1 + HML1 + RMW1 + CMA1 + WML1, data = dailyreturns1)

####Estimation Tables####
  #1. Estimated alphas
  #2. Estimated betas
#1. Estimated alphas
  Estimated_Alphas_data <- c(coef(SNPE1_CAPM)[1], coef(SNPE1_FF3)[1], coef(SNPE1_CarH)[1], coef(SNPE1_FF5)[1], coef(SNPE1_FF5m)[1],
                            coef(ESGE1_CAPM)[1], coef(ESGE1_FF3)[1], coef(ESGE1_CarH)[1], coef(ESGE1_FF5)[1], coef(ESGE1_FF5m)[1],
                            coef(ESGD1_CAPM)[1], coef(ESGD1_FF3)[1], coef(ESGD1_CarH)[1], coef(ESGD1_FF5)[1], coef(ESGD1_FF5m)[1],
                            coef(ESGU1_CAPM)[1], coef(ESGU1_FF3)[1], coef(ESGU1_CarH)[1], coef(ESGU1_FF5)[1], coef(ESGU1_FF5m)[1],
                            coef(NUMG1_CAPM)[1], coef(NUMG1_FF3)[1], coef(NUMG1_CarH)[1], coef(NUMG1_FF5)[1], coef(NUMG1_FF5m)[1])
  Estimated_Alphas_data <- round(Estimated_Alphas_data, digits = 5)
  Estimated_Alphas_Table <- matrix(c(Estimated_Alphas_data), ncol = 5, byrow = TRUE)
    colnames(Estimated_Alphas_Table) <- c('CAPM', 'FF3', 'CarH', 'FF5', 'FF5m')
    rownames(Estimated_Alphas_Table) <- c('SNPE', 'ESGE', 'ESGD', 'ESGU', 'NUMG')
    print(Estimated_Alphas_Table)

#2. Estimated betas
  Estimated_Betas_data <- c(coef(SNPE1_CAPM)[2], coef(SNPE1_FF3)[2], coef(SNPE1_CarH)[2], coef(SNPE1_FF5)[2], coef(SNPE1_FF5m)[2],
                            coef(ESGE1_CAPM)[2], coef(ESGE1_FF3)[2], coef(ESGE1_CarH)[2], coef(ESGE1_FF5)[2], coef(ESGE1_FF5m)[2],
                            coef(ESGD1_CAPM)[2], coef(ESGD1_FF3)[2], coef(ESGD1_CarH)[2], coef(ESGD1_FF5)[2], coef(ESGD1_FF5m)[2],
                            coef(ESGU1_CAPM)[2], coef(ESGU1_FF3)[2], coef(ESGU1_CarH)[2], coef(ESGU1_FF5)[2], coef(ESGU1_FF5m)[2],
                            coef(NUMG1_CAPM)[2], coef(NUMG1_FF3)[2], coef(NUMG1_CarH)[2], coef(NUMG1_FF5)[2], coef(NUMG1_FF5m)[2])
  Estimated_Betas_data <- round(Estimated_Betas_data, digits = 3)
  Estimated_Betas_Table <- matrix(c(Estimated_Betas_data), ncol = 5, byrow = TRUE)
    colnames(Estimated_Betas_Table) <- c('CAPM', 'FF3', 'CarH', 'FF5', 'FF5m')
    rownames(Estimated_Betas_Table) <- c('SNPE', 'ESGE', 'ESGD', 'ESGU', 'NUMG')
    print(Estimated_Betas_Table)

  
####Abnormal Returns AR; comes in a table of values####
    #abnormal return <- what happened - estimated alpha - estimated beta * what happened)
#SNPE  
  SNPE_AR_CAPM <- (dailyreturns2$SNPE2 
                    - coef(SNPE1_CAPM)[1] 
                    - coef(SNPE1_CAPM)[2]*dailyreturns2$SPY2)
  SNPE_AR_FF3 <-  (dailyreturns2$SNPE2 
                    - coef(SNPE1_FF3)[1] 
                    - coef(SNPE1_FF3)[2]*dailyreturns2$SPY2
                    - coef(SNPE1_FF3)[3]*dailyreturns2$SMB2
                    - coef(SNPE1_FF3)[4]*dailyreturns2$HML2)
  SNPE_AR_CarH <- (dailyreturns2$SNPE2 
                    - coef(SNPE1_CarH)[1] 
                    - coef(SNPE1_CarH)[2]*dailyreturns2$SPY2
                    - coef(SNPE1_CarH)[3]*dailyreturns2$SMB2
                    - coef(SNPE1_CarH)[4]*dailyreturns2$HML2
                    - coef(SNPE1_CarH)[5]*dailyreturns2$WML2)
  SNPE_AR_FF5 <-  (dailyreturns2$SNPE2 
                    - coef(SNPE1_FF5)[1] 
                    - coef(SNPE1_FF5)[2]*dailyreturns2$SPY2
                    - coef(SNPE1_FF5)[3]*dailyreturns2$SMB2
                    - coef(SNPE1_FF5)[4]*dailyreturns2$HML2
                    - coef(SNPE1_FF5)[5]*dailyreturns2$RMW2
                    - coef(SNPE1_FF5)[6]*dailyreturns2$CMA2)
  SNPE_AR_FF5m <- (dailyreturns2$SNPE2 
                    - coef(SNPE1_FF5m)[1] 
                    - coef(SNPE1_FF5m)[2]*dailyreturns2$SPY2
                    - coef(SNPE1_FF5m)[3]*dailyreturns2$SMB2
                    - coef(SNPE1_FF5m)[4]*dailyreturns2$HML2
                    - coef(SNPE1_FF5m)[5]*dailyreturns2$RMW2
                    - coef(SNPE1_FF5m)[6]*dailyreturns2$CMA2
                    - coef(SNPE1_FF5m)[7]*dailyreturns2$WML2)
#ESGE
  ESGE_AR_CAPM <- (dailyreturns2$SNPE2 
                   - coef(ESGE1_CAPM)[1] 
                   - coef(ESGE1_CAPM)[2]*dailyreturns2$SPY2)
  ESGE_AR_FF3 <-  (dailyreturns2$SNPE2 
                   - coef(ESGE1_FF3)[1] 
                   - coef(ESGE1_FF3)[2]*dailyreturns2$SPY2
                   - coef(ESGE1_FF3)[3]*dailyreturns2$SMB2
                   - coef(ESGE1_FF3)[4]*dailyreturns2$HML2)
  ESGE_AR_CarH <- (dailyreturns2$SNPE2 
                   - coef(ESGE1_CarH)[1] 
                   - coef(ESGE1_CarH)[2]*dailyreturns2$SPY2
                   - coef(ESGE1_CarH)[3]*dailyreturns2$SMB2
                   - coef(ESGE1_CarH)[4]*dailyreturns2$HML2
                   - coef(ESGE1_CarH)[5]*dailyreturns2$WML2)
  ESGE_AR_FF5 <-  (dailyreturns2$SNPE2 
                   - coef(ESGE1_FF5)[1] 
                   - coef(ESGE1_FF5)[2]*dailyreturns2$SPY2
                   - coef(ESGE1_FF5)[3]*dailyreturns2$SMB2
                   - coef(ESGE1_FF5)[4]*dailyreturns2$HML2
                   - coef(ESGE1_FF5)[5]*dailyreturns2$RMW2
                   - coef(ESGE1_FF5)[6]*dailyreturns2$CMA2)
  ESGE_AR_FF5m <- (dailyreturns2$SNPE2 
                   - coef(ESGE1_FF5m)[1] 
                   - coef(ESGE1_FF5m)[2]*dailyreturns2$SPY2
                   - coef(ESGE1_FF5m)[3]*dailyreturns2$SMB2
                   - coef(ESGE1_FF5m)[4]*dailyreturns2$HML2
                   - coef(ESGE1_FF5m)[5]*dailyreturns2$RMW2
                   - coef(ESGE1_FF5m)[6]*dailyreturns2$CMA2
                   - coef(ESGE1_FF5m)[7]*dailyreturns2$WML2)
#ESGD
  ESGD_AR_CAPM <- (dailyreturns2$SNPE2 
                   - coef(ESGD1_CAPM)[1] 
                   - coef(ESGD1_CAPM)[2]*dailyreturns2$SPY2)
  ESGD_AR_FF3 <-  (dailyreturns2$SNPE2 
                   - coef(ESGD1_FF3)[1] 
                   - coef(ESGD1_FF3)[2]*dailyreturns2$SPY2
                   - coef(ESGD1_FF3)[3]*dailyreturns2$SMB2
                   - coef(ESGD1_FF3)[4]*dailyreturns2$HML2)
  ESGD_AR_CarH <- (dailyreturns2$SNPE2 
                   - coef(ESGD1_CarH)[1] 
                   - coef(ESGD1_CarH)[2]*dailyreturns2$SPY2
                   - coef(ESGD1_CarH)[3]*dailyreturns2$SMB2
                   - coef(ESGD1_CarH)[4]*dailyreturns2$HML2
                   - coef(ESGD1_CarH)[5]*dailyreturns2$WML2)
  ESGD_AR_FF5 <-  (dailyreturns2$SNPE2 
                   - coef(ESGD1_FF5)[1] 
                   - coef(ESGD1_FF5)[2]*dailyreturns2$SPY2
                   - coef(ESGD1_FF5)[3]*dailyreturns2$SMB2
                   - coef(ESGD1_FF5)[4]*dailyreturns2$HML2
                   - coef(ESGD1_FF5)[5]*dailyreturns2$RMW2
                   - coef(ESGD1_FF5)[6]*dailyreturns2$CMA2)
  ESGD_AR_FF5m <- (dailyreturns2$SNPE2 
                   - coef(ESGD1_FF5m)[1] 
                   - coef(ESGD1_FF5m)[2]*dailyreturns2$SPY2
                   - coef(ESGD1_FF5m)[3]*dailyreturns2$SMB2
                   - coef(ESGD1_FF5m)[4]*dailyreturns2$HML2
                   - coef(ESGD1_FF5m)[5]*dailyreturns2$RMW2
                   - coef(ESGD1_FF5m)[6]*dailyreturns2$CMA2
                   - coef(ESGD1_FF5m)[7]*dailyreturns2$WML2)
#ESGU
  ESGU_AR_CAPM <- (dailyreturns2$SNPE2 
                   - coef(ESGU1_CAPM)[1] 
                   - coef(ESGU1_CAPM)[2]*dailyreturns2$SPY2)
  ESGU_AR_FF3 <-  (dailyreturns2$SNPE2 
                   - coef(ESGU1_FF3)[1] 
                   - coef(ESGU1_FF3)[2]*dailyreturns2$SPY2
                   - coef(ESGU1_FF3)[3]*dailyreturns2$SMB2
                   - coef(ESGU1_FF3)[4]*dailyreturns2$HML2)
  ESGU_AR_CarH <- (dailyreturns2$SNPE2 
                   - coef(ESGU1_CarH)[1] 
                   - coef(ESGU1_CarH)[2]*dailyreturns2$SPY2
                   - coef(ESGU1_CarH)[3]*dailyreturns2$SMB2
                   - coef(ESGU1_CarH)[4]*dailyreturns2$HML2
                   - coef(ESGU1_CarH)[5]*dailyreturns2$WML2)
  ESGU_AR_FF5 <-  (dailyreturns2$SNPE2 
                   - coef(ESGU1_FF5)[1] 
                   - coef(ESGU1_FF5)[2]*dailyreturns2$SPY2
                   - coef(ESGU1_FF5)[3]*dailyreturns2$SMB2
                   - coef(ESGU1_FF5)[4]*dailyreturns2$HML2
                   - coef(ESGU1_FF5)[5]*dailyreturns2$RMW2
                   - coef(ESGU1_FF5)[6]*dailyreturns2$CMA2)
  ESGU_AR_FF5m <- (dailyreturns2$SNPE2 
                   - coef(ESGU1_FF5m)[1] 
                   - coef(ESGU1_FF5m)[2]*dailyreturns2$SPY2
                   - coef(ESGU1_FF5m)[3]*dailyreturns2$SMB2
                   - coef(ESGU1_FF5m)[4]*dailyreturns2$HML2
                   - coef(ESGU1_FF5m)[5]*dailyreturns2$RMW2
                   - coef(ESGU1_FF5m)[6]*dailyreturns2$CMA2
                   - coef(ESGU1_FF5m)[7]*dailyreturns2$WML2)
#NUMG
  NUMG_AR_CAPM <- (dailyreturns2$SNPE2 
                   - coef(NUMG1_CAPM)[1] 
                   - coef(NUMG1_CAPM)[2]*dailyreturns2$SPY2)
  NUMG_AR_FF3 <-  (dailyreturns2$SNPE2 
                   - coef(NUMG1_FF3)[1] 
                   - coef(NUMG1_FF3)[2]*dailyreturns2$SPY2
                   - coef(NUMG1_FF3)[3]*dailyreturns2$SMB2
                   - coef(NUMG1_FF3)[4]*dailyreturns2$HML2)
  NUMG_AR_CarH <- (dailyreturns2$SNPE2 
                   - coef(NUMG1_CarH)[1] 
                   - coef(NUMG1_CarH)[2]*dailyreturns2$SPY2
                   - coef(NUMG1_CarH)[3]*dailyreturns2$SMB2
                   - coef(NUMG1_CarH)[4]*dailyreturns2$HML2
                   - coef(NUMG1_CarH)[5]*dailyreturns2$WML2)
  NUMG_AR_FF5 <-  (dailyreturns2$SNPE2 
                   - coef(NUMG1_FF5)[1] 
                   - coef(NUMG1_FF5)[2]*dailyreturns2$SPY2
                   - coef(NUMG1_FF5)[3]*dailyreturns2$SMB2
                   - coef(NUMG1_FF5)[4]*dailyreturns2$HML2
                   - coef(NUMG1_FF5)[5]*dailyreturns2$RMW2
                   - coef(NUMG1_FF5)[6]*dailyreturns2$CMA2)
  NUMG_AR_FF5m <- (dailyreturns2$SNPE2 
                   - coef(NUMG1_FF5m)[1] 
                   - coef(NUMG1_FF5m)[2]*dailyreturns2$SPY2
                   - coef(NUMG1_FF5m)[3]*dailyreturns2$SMB2
                   - coef(NUMG1_FF5m)[4]*dailyreturns2$HML2
                   - coef(NUMG1_FF5m)[5]*dailyreturns2$RMW2
                   - coef(NUMG1_FF5m)[6]*dailyreturns2$CMA2
                   - coef(NUMG1_FF5m)[7]*dailyreturns2$WML2)

####Cumulative Abnormal Returns CAR; comes in a single value####
#SNPE
  SNPE_CAR_CAPM <- sum(SNPE_AR_CAPM, na.rm=F)
  SNPE_CAR_FF3 <-  sum(SNPE_AR_FF3, na.rm=F)
  SNPE_CAR_CarH <- sum(SNPE_AR_CarH, na.rm=F)
  SNPE_CAR_FF5 <-  sum(SNPE_AR_FF5, na.rm=F)
  SNPE_CAR_FF5m <- sum(SNPE_AR_FF5m, na.rm=F)
#ESGE
  ESGE_CAR_CAPM <- sum(ESGE_AR_CAPM, na.rm=F)
  ESGE_CAR_FF3 <-  sum(ESGE_AR_FF3, na.rm=F)
  ESGE_CAR_CarH <- sum(ESGE_AR_CarH, na.rm=F)
  ESGE_CAR_FF5 <-  sum(ESGE_AR_FF5, na.rm=F)
  ESGE_CAR_FF5m <- sum(ESGE_AR_FF5m, na.rm=F)
#ESGD
  ESGD_CAR_CAPM <- sum(ESGD_AR_CAPM, na.rm=F)
  ESGD_CAR_FF3 <-  sum(ESGD_AR_FF3, na.rm=F)
  ESGD_CAR_CarH <- sum(ESGD_AR_CarH, na.rm=F)
  ESGD_CAR_FF5 <-  sum(ESGD_AR_FF5, na.rm=F)
  ESGD_CAR_FF5m <- sum(ESGD_AR_FF5m, na.rm=F)
#ESGU
  ESGU_CAR_CAPM <- sum(ESGU_AR_CAPM, na.rm=F)
  ESGU_CAR_FF3 <-  sum(ESGU_AR_FF3, na.rm=F)
  ESGU_CAR_CarH <- sum(ESGU_AR_CarH, na.rm=F)
  ESGU_CAR_FF5 <-  sum(ESGU_AR_FF5, na.rm=F)
  ESGU_CAR_FF5m <- sum(ESGU_AR_FF5m, na.rm=F)
#NUMG
  NUMG_CAR_CAPM <- sum(NUMG_AR_CAPM, na.rm=F)
  NUMG_CAR_FF3 <-  sum(NUMG_AR_FF3, na.rm=F)
  NUMG_CAR_CarH <- sum(NUMG_AR_CarH, na.rm=F)
  NUMG_CAR_FF5 <-  sum(NUMG_AR_FF5, na.rm=F)
  NUMG_CAR_FF5m <- sum(NUMG_AR_FF5m, na.rm=F)
  
####Test for statistical significance (CAR/variance)####
  #1. Get var * L where L is length of event window (11 days)
  #2. Test statistic <- CAR/sqrt(var)
  #3. Convert to p-value
#1
#SNPE var
  SNPE_ARvar_CAPM <- (11)*(var(SNPE_AR_CAPM, na.rm = FALSE))
  SNPE_ARvar_FF3 <-  (11)*(var(SNPE_AR_FF3, na.rm = FALSE))
  SNPE_ARvar_CarH <- (11)*(var(SNPE_AR_CarH, na.rm = FALSE))
  SNPE_ARvar_FF5 <-  (11)*(var(SNPE_AR_FF5, na.rm = FALSE))
  SNPE_ARvar_FF5m <- (11)*(var(SNPE_AR_FF5m, na.rm = FALSE))
#ESGE var
  ESGE_ARvar_CAPM <- (11)*(var(ESGE_AR_CAPM, na.rm = FALSE))
  ESGE_ARvar_FF3 <-  (11)*(var(ESGE_AR_FF3, na.rm = FALSE))
  ESGE_ARvar_CarH <- (11)*(var(ESGE_AR_CarH, na.rm = FALSE))
  ESGE_ARvar_FF5 <-  (11)*(var(ESGE_AR_FF5, na.rm = FALSE))
  ESGE_ARvar_FF5m <- (11)*(var(ESGE_AR_FF5m, na.rm = FALSE))
#ESGD var
  ESGD_ARvar_CAPM <- (11)*(var(ESGD_AR_CAPM, na.rm = FALSE))
  ESGD_ARvar_FF3 <-  (11)*(var(ESGD_AR_FF3, na.rm = FALSE))
  ESGD_ARvar_CarH <- (11)*(var(ESGD_AR_CarH, na.rm = FALSE))
  ESGD_ARvar_FF5 <-  (11)*(var(ESGD_AR_FF5, na.rm = FALSE))
  ESGD_ARvar_FF5m <- (11)*(var(ESGD_AR_FF5m, na.rm = FALSE))
#ESGU var
  ESGU_ARvar_CAPM <- (11)*(var(ESGU_AR_CAPM, na.rm = FALSE))
  ESGU_ARvar_FF3 <-  (11)*(var(ESGU_AR_FF3, na.rm = FALSE))
  ESGU_ARvar_CarH <- (11)*(var(ESGU_AR_CarH, na.rm = FALSE))
  ESGU_ARvar_FF5 <-  (11)*(var(ESGU_AR_FF5, na.rm = FALSE))
  ESGU_ARvar_FF5m <- (11)*(var(ESGU_AR_FF5m, na.rm = FALSE))
#NUMG var
  NUMG_ARvar_CAPM <- (11)*(var(NUMG_AR_CAPM, na.rm = FALSE))
  NUMG_ARvar_FF3 <-  (11)*(var(NUMG_AR_FF3, na.rm = FALSE))
  NUMG_ARvar_CarH <- (11)*(var(NUMG_AR_CarH, na.rm = FALSE))
  NUMG_ARvar_FF5 <-  (11)*(var(NUMG_AR_FF5, na.rm = FALSE))
  NUMG_ARvar_FF5m <- (11)*(var(NUMG_AR_FF5m, na.rm = FALSE))

#2
#SNPE Sig.
  SNPE_Sig_CAPM <- (SNPE_CAR_CAPM)/(sqrt(SNPE_ARvar_CAPM))
  SNPE_Sig_FF3 <-  (SNPE_CAR_FF3)/(sqrt(SNPE_ARvar_FF3))
  SNPE_Sig_CarH <- (SNPE_CAR_CarH)/(sqrt(SNPE_ARvar_CarH))
  SNPE_Sig_FF5 <-  (SNPE_CAR_FF5)/(sqrt(SNPE_ARvar_FF5))
  SNPE_Sig_FF5m <- (SNPE_CAR_FF5m)/(sqrt(SNPE_ARvar_FF5m))
#ESGE Sig.
  ESGE_Sig_CAPM <- (ESGE_CAR_CAPM)/(sqrt(ESGE_ARvar_CAPM))
  ESGE_Sig_FF3 <-  (ESGE_CAR_FF3)/(sqrt(ESGE_ARvar_FF3))
  ESGE_Sig_CarH <- (ESGE_CAR_CarH)/(sqrt(ESGE_ARvar_CarH))
  ESGE_Sig_FF5 <-  (ESGE_CAR_FF5)/(sqrt(ESGE_ARvar_FF5))
  ESGE_Sig_FF5m <- (ESGE_CAR_FF5m)/(sqrt(ESGE_ARvar_FF5m))
#ESGD Sig.
  ESGD_Sig_CAPM <- (ESGD_CAR_CAPM)/(sqrt(ESGD_ARvar_CAPM))
  ESGD_Sig_FF3 <-  (ESGD_CAR_FF3)/(sqrt(ESGD_ARvar_FF3))
  ESGD_Sig_CarH <- (ESGD_CAR_CarH)/(sqrt(ESGD_ARvar_CarH))
  ESGD_Sig_FF5 <-  (ESGD_CAR_FF5)/(sqrt(ESGD_ARvar_FF5))
  ESGD_Sig_FF5m <- (ESGD_CAR_FF5m)/(sqrt(ESGD_ARvar_FF5m))
#ESGU Sig.
  ESGU_Sig_CAPM <- (ESGU_CAR_CAPM)/(sqrt(ESGU_ARvar_CAPM))
  ESGU_Sig_FF3 <-  (ESGU_CAR_FF3)/(sqrt(ESGU_ARvar_FF3))
  ESGU_Sig_CarH <- (ESGU_CAR_CarH)/(sqrt(ESGU_ARvar_CarH))
  ESGU_Sig_FF5 <-  (ESGU_CAR_FF5)/(sqrt(ESGU_ARvar_FF5))
  ESGU_Sig_FF5m <- (ESGU_CAR_FF5m)/(sqrt(ESGU_ARvar_FF5m))
#NUMG Sig.
  NUMG_Sig_CAPM <- (NUMG_CAR_CAPM)/(sqrt(NUMG_ARvar_CAPM))
  NUMG_Sig_FF3 <-  (NUMG_CAR_FF3)/(sqrt(NUMG_ARvar_FF3))
  NUMG_Sig_CarH <- (NUMG_CAR_CarH)/(sqrt(NUMG_ARvar_CarH))
  NUMG_Sig_FF5 <-  (NUMG_CAR_FF5)/(sqrt(NUMG_ARvar_FF5))
  NUMG_Sig_FF5m <- (NUMG_CAR_FF5m)/(sqrt(NUMG_ARvar_FF5m))

#3
  #lower.tail = TRUE when t stat is negative, FALSE when t stat is positive
#SNPE P Value
  SNPE_P_CAPM <- pt(q = SNPE_Sig_CAPM, df = 10, lower.tail = TRUE)
  SNPE_P_FF3 <-  pt(q = SNPE_Sig_FF3, df = 10, lower.tail = TRUE)
  SNPE_P_CarH <- pt(q = SNPE_Sig_CarH, df = 10, lower.tail = TRUE)
  SNPE_P_FF5 <-  pt(q = SNPE_Sig_FF5, df = 10, lower.tail = TRUE)
  SNPE_P_FF5m <- pt(q = SNPE_Sig_FF5m, df = 10, lower.tail = TRUE)
#ESGE P Value
  ESGE_P_CAPM <- pt(q = ESGE_Sig_CAPM, df = 10, lower.tail = TRUE)
  ESGE_P_FF3 <-  pt(q = ESGE_Sig_FF3, df = 10, lower.tail = TRUE)
  ESGE_P_CarH <- pt(q = ESGE_Sig_CarH, df = 10, lower.tail = TRUE)
  ESGE_P_FF5 <-  pt(q = ESGE_Sig_FF5, df = 10, lower.tail = TRUE)
  ESGE_P_FF5m <- pt(q = ESGE_Sig_FF5m, df = 10, lower.tail = TRUE)
#ESGD P Value
  ESGD_P_CAPM <- pt(q = ESGD_Sig_CAPM, df = 10, lower.tail = TRUE)
  ESGD_P_FF3 <-  pt(q = ESGD_Sig_FF3, df = 10, lower.tail = TRUE)
  ESGD_P_CarH <- pt(q = ESGD_Sig_CarH, df = 10, lower.tail = TRUE)
  ESGD_P_FF5 <-  pt(q = ESGD_Sig_FF5, df = 10, lower.tail = TRUE)
  ESGD_P_FF5m <- pt(q = ESGD_Sig_FF5m, df = 10, lower.tail = TRUE)
#ESGU P Value
  ESGU_P_CAPM <- pt(q = ESGU_Sig_CAPM, df = 10, lower.tail = TRUE)
  ESGU_P_FF3 <-  pt(q = ESGU_Sig_FF3, df = 10, lower.tail = TRUE)
  ESGU_P_CarH <- pt(q = ESGU_Sig_CarH, df = 10, lower.tail = TRUE)
  ESGU_P_FF5 <-  pt(q = ESGU_Sig_FF5, df = 10, lower.tail = FALSE)
  ESGU_P_FF5m <- pt(q = ESGU_Sig_FF5m, df = 10, lower.tail = FALSE)
#NUMG P Value
  NUMG_P_CAPM <- pt(q = NUMG_Sig_CAPM, df = 10, lower.tail = FALSE)
  NUMG_P_FF3 <-  pt(q = NUMG_Sig_FF3, df = 10, lower.tail = FALSE)
  NUMG_P_CarH <- pt(q = NUMG_Sig_CarH, df = 10, lower.tail = FALSE)
  NUMG_P_FF5 <-  pt(q = NUMG_Sig_FF5, df = 10, lower.tail = FALSE)
  NUMG_P_FF5m <- pt(q = NUMG_Sig_FF5m, df = 10, lower.tail = FALSE)
  
####Tables####
  #1. Standard Deviation
  #2. CAR
  #3. T Stats
  #4. P Values
#1. Standard Deviation
  StDev_Table_Data <- c(sqrt(SNPE_ARvar_CAPM), sqrt(SNPE_ARvar_FF3), sqrt(SNPE_ARvar_CarH), sqrt(SNPE_ARvar_FF5), sqrt(SNPE_ARvar_FF5m),
                        sqrt(ESGE_ARvar_CAPM), sqrt(ESGE_ARvar_FF3), sqrt(ESGE_ARvar_CarH), sqrt(ESGE_ARvar_FF5), sqrt(ESGE_ARvar_FF5m),
                        sqrt(ESGD_ARvar_CAPM), sqrt(ESGD_ARvar_FF3), sqrt(ESGD_ARvar_CarH), sqrt(ESGD_ARvar_FF5), sqrt(ESGD_ARvar_FF5m),
                        sqrt(ESGU_ARvar_CAPM), sqrt(ESGU_ARvar_FF3), sqrt(ESGU_ARvar_CarH), sqrt(ESGU_ARvar_FF5), sqrt(ESGU_ARvar_FF5m),
                        sqrt(NUMG_ARvar_CAPM), sqrt(NUMG_ARvar_FF3), sqrt(NUMG_ARvar_CarH), sqrt(NUMG_ARvar_FF5), sqrt(NUMG_ARvar_FF5m))
  StDev_Table_Data <-  round(StDev_Table_Data, digits = 3)
  StDev_Table <- matrix(c(StDev_Table_Data), ncol = 5, byrow = TRUE)
    colnames(StDev_Table) <- c('CAPM', 'FF3', 'CarH', 'FF5', 'FF5m')
    rownames(StDev_Table) <- c('SNPE', 'ESGE', 'ESGD', 'ESGU', 'NUMG')
    print(StDev_Table)

#2. CAR
  CAR_Table_Data <- c(SNPE_CAR_CAPM, SNPE_CAR_FF3, SNPE_CAR_CarH, SNPE_CAR_FF5, SNPE_CAR_FF5m,
                      ESGE_CAR_CAPM, ESGE_CAR_FF3, ESGE_CAR_CarH, ESGE_CAR_FF5, ESGE_CAR_FF5m,
                      ESGD_CAR_CAPM, ESGD_CAR_FF3, ESGD_CAR_CarH, ESGD_CAR_FF5, ESGD_CAR_FF5m,
                      ESGU_CAR_CAPM, ESGU_CAR_FF3, ESGU_CAR_CarH, ESGU_CAR_FF5, ESGU_CAR_FF5m,
                      NUMG_CAR_CAPM, NUMG_CAR_FF3, NUMG_CAR_CarH, NUMG_CAR_FF5, NUMG_CAR_FF5m)
  CAR_Table_Data <- round(CAR_Table_Data, digits = 3)
  CAR_Table <- matrix(c(CAR_Table_Data), ncol = 5, byrow = TRUE)
    colnames(CAR_Table) <- c('CAPM', 'FF3', 'CarH', 'FF5', 'FF5m')
    rownames(CAR_Table) <- c('SNPE', 'ESGE', 'ESGD', 'ESGU', 'NUMG')
    print(CAR_Table)
    
#3. T Stats
  T_Table_Data <- c(SNPE_Sig_CAPM, SNPE_Sig_FF3, SNPE_Sig_CarH, SNPE_Sig_FF5, SNPE_Sig_FF5m,
                    ESGE_Sig_CAPM, ESGE_Sig_FF3, ESGE_Sig_CarH, ESGE_Sig_FF5, ESGE_Sig_FF5m,
                    ESGD_Sig_CAPM, ESGD_Sig_FF3, ESGD_Sig_CarH, ESGD_Sig_FF5, ESGD_Sig_FF5m,
                    ESGU_Sig_CAPM, ESGU_Sig_FF3, ESGU_Sig_CarH, ESGU_Sig_FF5, ESGU_Sig_FF5m,
                    NUMG_Sig_CAPM, NUMG_Sig_FF3, NUMG_Sig_CarH, NUMG_Sig_FF5, NUMG_Sig_FF5m)
  T_Table_Data <- round(T_Table_Data, digits = 3)
  T_Table <- matrix(c(T_Table_Data), ncol = 5, byrow = TRUE)
    colnames(T_Table) <- c('CAPM', 'FF3', 'CarH', 'FF5', 'FF5m')
    rownames(T_Table) <- c('SNPE', 'ESGE', 'ESGD', 'ESGU', 'NUMG')
    print(T_Table)
    
#4. P Values
  P_Table_Data <- c(SNPE_P_CAPM, SNPE_P_FF3, SNPE_P_CarH, SNPE_P_FF5, SNPE_P_FF5m,
                    ESGE_P_CAPM, ESGE_P_FF3, ESGE_P_CarH, ESGE_P_FF5, ESGE_P_FF5m,
                    ESGD_P_CAPM, ESGD_P_FF3, ESGD_P_CarH, ESGD_P_FF5, ESGD_P_FF5m,
                    ESGU_P_CAPM, ESGU_P_FF3, ESGU_P_CarH, ESGU_P_FF5, ESGU_P_FF5m,
                    NUMG_P_CAPM, NUMG_P_FF3, NUMG_P_CarH, NUMG_P_FF5, NUMG_P_FF5m)
  P_Table_Data <- round(P_Table_Data, digits = 3)
  P_Table <- matrix(c(P_Table_Data), ncol = 5, byrow = TRUE)
    colnames(P_Table) <- c('CAPM', 'FF3', 'CarH', 'FF5', 'FF5m')
    rownames(P_Table) <- c('SNPE', 'ESGE', 'ESGD', 'ESGU', 'NUMG')
    print(P_Table)
    