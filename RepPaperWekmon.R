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
data3 <- read_xlsx("datarep.xlsx", sheet = "cpi")

##append both data
pdbsekt = rbind(data1, data2)

##pdb nom = pdbriil
pdbsekt$agri <- pdbsekt$agri/data3$cpi_per
pdbsekt$man <- pdbsekt$man/data3$cpi_per
pdbsekt$jasa <- pdbsekt$jasa/data3$cpi_per
##generate pdb & period

pdbsektlong <- pdbsekt %>%
  gather(key = "lapangan",
         value = "pdb1",
         c(-kuartal))

pdbs <- pdbsektlong %>% group_by(kuartal) %>% summarise(pdbr = sum(pdb1)) 

rep1 <- left_join(pdbsekt, pdbs)
rep1 <- left_join(rep1, data3)
##visualise
ggplot(data=rep1, aes(x=kuartal, y=pdbr, group = 1)) + geom_line()



##gen var aggregate production (excluding the sector under consideration)

rep1 <- rep1 %>% group_by(kuartal) %>% mutate(agrimin = sum(man, jasa))
rep1 <- rep1 %>% group_by(kuartal) %>% mutate(manmin = sum(agri, jasa))
rep1 <- rep1 %>% group_by(kuartal) %>% mutate(jasamin = sum(agri, man))

##visualise
rep1$angka <- c(1:91)
ggplot(data=rep1, aes(x=angka)) +
  geom_line(aes(y=pdbr),  color = "darkred") +
  geom_line(aes(y=agri),  color = "steelblue", linetype = "twodash") +
  geom_line(aes(y=man),  color = "grey") +
  geom_line(aes(y=jasa),  color = "green")

ggplot(data=rep1, aes(x=angka)) +
  geom_line(aes(y=pdbr),  color = "darkred") +
  geom_line(aes(y=agrimin),  color = "steelblue", linetype = "twodash") +
  geom_line(aes(y=manmin),  color = "grey") +
  geom_line(aes(y=jasamin),  color = "green")
##change data into time-series
rep1$pdbr <- log(rep1$pdbr)
rep1$agri <- log(rep1$agri)
rep1$man <- log(rep1$man)
rep1$jasa <- log(rep1$jasa)
rep1$agrimin <- log(rep1$agrimin)
rep1$manmin <- log(rep1$manmin)
rep1$jasamin <- log(rep1$jasamin)


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

stationarity <- as.data.frame(cbind(statest,statestdiff))

#######seasonality test
seasonalize <- NULL
for (i in 1:ncol(rep2)){
  stl.dec <-  rep2[,i] %>% stl(t.window = 4, s.window = "periodic")
  seasonalize <- cbind(seasonalize, stl.dec$time.series[,2])
}

##naming the variables
rep3 <- as.data.frame(seasonalize)
names(rep3) = c('pdbr', 'agri', 'man', 'jasa', 'int', 'nex', 'cpi_per',
                            'agrimin', 'manmin', 'jasamin')

rep4 <- ts(rep3, frequency = 4, start = c(2000,1))

##divide into several groups of data

#GDP
gdp <- subset(rep3, select =
                c(pdbr, cpi_per, 
                  nex, int))
gdp <- ts(gdp, frequency = 4, start = c(2000,1))



#Agri
agri <- subset(rep3, select =
                c(agrimin, cpi_per, 
                  agri, nex, int))
agri <- ts(agri, frequency = 4, start = c(2000,1))

#Man
man <- subset(rep3, select =
                 c(manmin, cpi_per, 
                   man, nex, int))
man <- ts(man, frequency = 4, start = c(2000,1))

#Jasa
serv <- subset(rep3, select =
                c(jasamin, cpi_per, 
                  jasa, nex, int))
serv <- ts(serv, frequency = 4, start = c(2000,1))

########----------------------------------Data analysis



#optimal lag length
optlag <- NULL
for (i in 1:ncol(rep4)){
  testlag <- VARselect(rep4[,i], lag.max = 4)
  optlag <- rbind(optlag, testlag$selection)
}       ##lag = 4


##write_xlsx(stationarity,"statest.xlsx")

#gdp
gdplag <- NULL
for (i in 1:ncol(gdp)){
  gdptest <- VARselect(gdp, lag.max = 4)
  gdplag <- rbind(gdplag, gdptest$selection)
}       ##lag = 4     

#agri
agrilag <- NULL
for (i in 1:ncol(agri)){
  agritest <- VARselect(agri, lag.max = 4)
  agrilag <- rbind(agrilag, agritest$selection)
}       ##lag = 4


#man
manlag <- NULL
for (i in 1:ncol(man)){
  mantest <- VARselect(man, lag.max = 4)
  manlag <- rbind(manlag, mantest$selection)
}       ##lag = 4

#jasa
servlag <- NULL
for (i in 1:ncol(serv)){
  servtest <- VARselect(serv, lag.max = 4)
  servlag <- rbind(servlag, servtest$selection)
}       ##lag = 4


#cointegration test
gdp.co <- ca.jo(gdp, type = "trace", K = 4)
summary(gdp.co)

agri.co <- ca.jo(agri, type = "trace", K = 4)
summary(agri.co)

man.co <- ca.jo(man, type = "trace", K = 4)
summary(man.co)

serv.co <- ca.jo(serv, type = "trace", K = 4)
summary(serv.co)


#VAR analysis

vargdp <- VAR(gdp, p = 4, type = "const", season = NULL, exogen = NULL)
serialvargdp <- serial.test(vargdp, lags.pt = 12, type = "PT.adjusted")

plot(irf(vargdp, impulse = "int", response = "pdbr", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95))
plot(irf(vargdp, impulse = "int", response = "cpi_per", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95))
plot(irf(vargdp, impulse = "int", response = "nex", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95))
plot(irf(vargdp, impulse = "int", response = "int", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95))
fevd(vargdp, n.ahead = 24)



varagri <- VAR(agri, p = 4, type = "const", season = NULL, exogen = NULL)
plot(irf(varagri, impulse = "int", response = "agri", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95))


varman <- VAR(man, p = 4, type = "const", season = NULL, exogen = NULL)
plot(irf(varman, impulse = "int", response = "man", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95))

varserv <- VAR(serv, p = 4, type = "const", season = NULL, exogen = NULL)
plot(irf(varserv, impulse = "int", response = "jasa", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95))




