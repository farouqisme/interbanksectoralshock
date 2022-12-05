library(tidyverse)
library(dplyr)
library(readxl)
library(Hmisc)
library(ggplot2)
library(writexl)
library(usethis)
library(urca)
library(tseries)
library(lubridate)
library(tsbox)
library(xts)
library(zoo)
library(vars)
setwd("E:/KULIAH!/SEMESTER 7/Wekmon/Replication Paper/DAta")
getwd()


##get data
data1 <- read_xlsx("datarep.xlsx", sheet = "00-09")
data2 <- read_xlsx("datarep.xlsx", sheet = "10-22")

##append both data
pdbsekt = rbind(data1, data2)

##generate pdb & period

pdbsektlong <- pdbsekt %>%
  gather(key = "lapangan",
         value = "pdb1",
         c(-kuartal))

pdbs <- pdbsektlong %>% group_by(kuartal) %>% summarise(pdb = sum(pdb1)) 

pdbf <- left_join(pdbsekt, pdbs)

##visualise
ggplot(data=pdbf, aes(x=period, y=pdb, group = 1)) + geom_line()


##pdb nom = pdb riil
data3 <- read_xlsx("datarep.xlsx", sheet = "cpi")

rep1 <- left_join(pdbf, data3)

rep1$pdbr <- rep1$pdb/rep1$cpi_per
rep1$agri <- rep1$agri/rep1$cpi_per
rep1$man <- rep1$agri/rep1$cpi_per
rep1$jasa <- rep1$jasa/rep1$cpi_per

##gen var aggregate production (excluding the sector under consideration)

rep1 <- rep1 %>% group_by(kuartal) %>% mutate(agrimin = sum(man, jasa))
rep1 <- rep1 %>% group_by(kuartal) %>% mutate(manmin = sum(agri, jasa))
rep1 <- rep1 %>% group_by(kuartal) %>% mutate(jasamin = sum(agri, man))

##visualise
ggplot(data=rep1, aes(x=period)) +
  geom_line(aes(y=pdbr),  color = "darkred") +
  geom_line(aes(y=pdb),  color = "steelblue", linetype = "twodash")

##change data into time-series

datafix <- subset(rep1, select = 
                    c(pdbr, agri, man, jasa, int, 
                      nex, cpi_per, agrimin, manmin, jasamin))

rep2 <- ts(datafix, frequency = 4, start = c(2000,1))
#write_xlsx(rep2,"datainit.xlsx")

#######stationarity test
##observing data's stationarity using plot
ts.plot(rep2[,"pdbr"])
ts.plot(rep2[,"agri"])
ts.plot(rep2[,"man"])
ts.plot(rep2[,"jasa"])
ts.plot(rep2[,"nex"])
ts.plot(rep2[,"cpi_per"])
ts.plot(rep2[,"int"])

##stationarity test using ADF & PP (level)
pptest <- NULL
for (i in 1:ncol(rep2)){
  pp <- pp.test(rep2[,i])
  pptest <- rbind(pptest,pp$p.value)
}

adftest <- NULL
for (i in 1:ncol(rep2)){
  adf <- adf.test(rep2[,i])
  adftest <- rbind(adftest,adf$p.value)
}

statest <- cbind(pptest, adftest)


##stationarity test using ADF & PP (first diff)
pptestdiff <- NULL
for (i in 1:ncol(rep2)){
  pp <- pp.test(diff(rep2[,i]))
  pptestdiff <- rbind(pptestdiff,pp$p.value)
}

adftestdiff <- NULL
for (i in 1:ncol(rep2)){
  adf <- adf.test(diff(rep2[,i]))
  adftestdiff <- rbind(adftestdiff,adf$p.value)
}

statestdiff <- cbind(pptestdiff, adftestdiff)


#######seasonality test
seasonalize <- NULL
for (i in 1:ncol(rep2)){
  stl.dec <-  rep2[,i] %>% stl(t.window = 4, s.window = "periodic")
  seasonalize <- cbind(seasonalize, stl.dec$time.series[,2])
}

##naming the variables
for (i in 1:ncol(seasonalize)){
  rename(seasonalize, i = c(pdbr, agri, man, jasa, int, nex, cpi_per,
                            agrimin, manmin, jasamin))
}

##divide into several groups of data

#GDP
gdp <- subset(rep2, select =
                c(pdbr, cpi_per, 
                  nex, int))
gdp <- ts(gdp, frequency = 4, start = c(2000,1))



#Agri
agri <- subset(rep2, select =
                c(agrimin, cpi_per, 
                  agri, nex, int))
agri <- ts(agri, frequency = 4, start = c(2000,1))

#Man
man <- subset(rep2, select =
                 c(manmin, cpi_per, 
                   man, nex, int))
man <- ts(man, frequency = 4, start = c(2000,1))

#Jasa
serv <- subset(rep2, select =
                c(jasamin, cpi_per, 
                  jasa, nex, int))
serv <- ts(serv, frequency = 4, start = c(2000,1))

########----------------------------------Data analysis



#optimal lag length
optlag <- NULL
for (i in 1:ncol(rep2)){
  testlag <- VARselect(rep2[,i], lag.max = 4)
  optlag <- rbind(optlag, testlag$selection)
}

stationarity <- as.data.frame(cbind(statest,statestdiff))
##write_xlsx(stationarity,"statest.xlsx")

#gdp
gdplag <- NULL
for (i in 1:ncol(gdp)){
  gdptest <- VARselect(gdp, lag.max = 4)
  gdplag <- rbind(gdplag, gdptest$selection)
}


#agri
agrilag <- NULL
for (i in 1:ncol(agri)){
  agritest <- VARselect(agri, lag.max = 4)
  agrilag <- rbind(agrilag, agritest$selection)
}

#man
manlag <- NULL
for (i in 1:ncol(man)){
  mantest <- VARselect(man, lag.max = 4)
  manlag <- rbind(manlag, mantest$selection)
}

#jasa
servlag <- NULL
for (i in 1:ncol(serv)){
  servtest <- VARselect(serv, lag.max = 4)
  servlag <- rbind(servlag, servtest$selection)
}


#cointegration test
gdp.co <- ca.jo(gdp, type = "trace", K = 3, season = 4)
summary(gdp.co)

agri.co <- ca.jo(agri, type = "trace", K = 4, season = 4)
summary(agri.co)

man.co <- ca.jo(man, type = "trace", K = 4, season = 4)
summary(man.co)

serv.co <- ca.jo(serv, type = "trace", K = 4)
summary(serv.co)

ss

