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
library(stargazer)
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


##/////////////////////////////////////##FULL SAMPLE##/////////////////////////////////////##
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

####################GDP
vargdp <- VAR(gdp, p = 4, type = "const", season = NULL, exogen = NULL)
serialvargdp <- serial.test(vargdp, lags.pt = 12, type = "PT.adjusted")

irfgdp <- irf(vargdp, impulse = "int", response = "pdbr", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95)
irfcpi <- irf(vargdp, impulse = "int", response = "cpi_per", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95)
irfnex <- irf(vargdp, impulse = "int", response = "nex", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95)
irfint <- irf(vargdp, impulse = "int", response = "int", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95)

plot(irfgdp, xlab = "", ylab = "", main = "GDP")
plot(irfcpi, xlab = "", ylab = "", main = "CPI")
plot(irfnex, xlab = "", ylab = "", main = "NEX")
plot(irfint, xlab = "", ylab = "", main = "INT")


####################Sectoral
varagri <- VAR(agri, p = 4, type = "const", season = NULL, exogen = NULL)
varman <- VAR(man, p = 4, type = "const", season = NULL, exogen = NULL)
varserv <- VAR(serv, p = 4, type = "const", season = NULL, exogen = NULL)


irfagri <- irf(varagri, impulse = "int", response = "agri", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95)
irfman <- irf(varman, impulse = "int", response = "man", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95)
irfjasa <- irf(varserv, impulse = "int", response = "jasa", boot = T, cumulative = FALSE, n.ahead = 50, ci=0.95)

###perbandingan FEVD
fevdgdp <- fevd(vargdp, n.ahead = 24)
fevdagri <- fevd(varagri, n.ahead = 24)
fevdman <- fevd(varman, n.ahead = 24)
fevdserv <- fevd(varserv, n.ahead = 24)

fevdtot <- data.frame(fevdgdp$int, fevdagri$int, fevdman$int, fevdserv$int)
fevdtot <- fevdtot %>% subset( , c(pdbr, agri, man, jasa))
#write_xlsx(fevdtot, "fevdsec.xlsx")

###plotting IRF (BELUM SOLVED)
irftotal <- data.frame(irfgdp$irf$int, irfagri$irf$int, irfman$irf$int, irfjasa$irf$int)
irftotal$x <- seq(1,51)
ggplot(data=irftotal, aes(x=x)) +
  geom_line(aes(y=pdbr), linetype = "solid")+
  geom_line(aes(y=jasa), linetype = "dashed")+
  labs(x = "horizons", y = "response")+
  theme_classic()




##/////////////////////////////////////##SUBSAMPLE##/////////////////////////////////////##
##divide full sample seasonalized data to 2 sub-sample 

sub1 <- head(rep3, 45)
sub2 <- tail(rep3, 46)


############SUBSAMPLE 1###########
gdp1 <- subset(sub1, select =
                c(pdbr, cpi_per, 
                  nex, int))
gdp1 <- ts(gdp1, frequency = 4, start = c(2000,1))

#Agri
agri1 <- subset(sub1, select =
                 c(agrimin, cpi_per, 
                   agri, nex, int))
agri1 <- ts(agri1, frequency = 4, start = c(2000,1))

#Man
man1 <- subset(sub1, select =
                c(manmin, cpi_per, 
                  man, nex, int))
man1 <- ts(man1, frequency = 4, start = c(2000,1))

#Jasa
serv1 <- subset(sub1, select =
                 c(jasamin, cpi_per, 
                   jasa, nex, int))
serv1 <- ts(serv1, frequency = 4, start = c(2000,1))


##VAR Analysis
vargdp1 <- VAR(gdp1, p = 4, type = "const", season = NULL, exogen = NULL)
varagri1 <- VAR(agri1, p = 4, type = "const", season = NULL, exogen = NULL)
varman1 <- VAR(man1, p = 4, type = "const", season = NULL, exogen = NULL)
varserv1 <- VAR(serv1, p = 4, type = "const", season = NULL, exogen = NULL)

###create tables
irfgdp1 <- irf(vargdp1, impulse = "int", response = "pdbr", boot = T, cumulative = FALSE, n.ahead = 24, ci=0.95)
irfagri1 <- irf(varagri1, impulse = "int", response = "agri", boot = T, cumulative = FALSE, n.ahead = 24, ci=0.95)
irfman1 <- irf(varman1, impulse = "int", response = "man", boot = T, cumulative = FALSE, n.ahead = 24, ci=0.95)
irfserv1 <- irf(varserv1, impulse = "int", response = "jasa", boot = T, cumulative = FALSE, n.ahead = 24, ci=0.95)

irf1 <- data.frame(irfgdp1$irf$int, irfagri1$irf$int, irfman1$irf$int, irfserv1$irf$int)

############SUBSAMPLE 2###########
gdp2 <- subset(sub2, select =
                 c(pdbr, cpi_per, 
                   nex, int))
gdp2 <- ts(gdp2, frequency = 4, start = c(2000,1))

#Agri
agri2 <- subset(sub2, select =
                  c(agrimin, cpi_per, 
                    agri, nex, int))
agri2 <- ts(agri2, frequency = 4, start = c(2000,1))

#Man
man2 <- subset(sub2, select =
                 c(manmin, cpi_per, 
                   man, nex, int))
man2 <- ts(man2, frequency = 4, start = c(2000,1))

#Jasa
serv2 <- subset(sub2, select =
                  c(jasamin, cpi_per, 
                    jasa, nex, int))
serv2 <- ts(serv2, frequency = 4, start = c(2000,1))


##VAR Analysis
vargdp2 <- VAR(gdp2, p = 4, type = "const", season = NULL, exogen = NULL)
varagri2 <- VAR(agri2, p = 4, type = "const", season = NULL, exogen = NULL)
varman2 <- VAR(man2, p = 4, type = "const", season = NULL, exogen = NULL)
varserv2 <- VAR(serv2, p = 4, type = "const", season = NULL, exogen = NULL)

###create tables
irfgdp2 <- irf(vargdp2, impulse = "int", response = "pdbr", boot = T, cumulative = FALSE, n.ahead = 24, ci=0.95)
irfagri2 <- irf(varagri2, impulse = "int", response = "agri", boot = T, cumulative = FALSE, n.ahead = 24, ci=0.95)
irfman2 <- irf(varman2, impulse = "int", response = "man", boot = T, cumulative = FALSE, n.ahead = 24, ci=0.95)
irfserv2 <- irf(varserv2, impulse = "int", response = "jasa", boot = T, cumulative = FALSE, n.ahead = 24, ci=0.95)

irf2 <- data.frame(irfgdp2$irf$int, irfagri2$irf$int, irfman2$irf$int, irfserv2$irf$int)
irf12 <- cbind(irf1, irf2)
#write_xlsx(irf12, "irfsubsample.xlsx")

#######PR-nya: 1. cari cara bikin tabel statistik deskriptif 
#######2. gabungin  irf subsample
#######3. masukin tabel cointest
#######4. masukin fevd

