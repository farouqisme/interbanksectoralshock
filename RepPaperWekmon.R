install.packages("urca")
install.packages("tseries")
install.packages("lubridate")
install.packages("tsbox")
install.packages("xts")
library(plyr)
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

pdbf$period <- c(1:91)


##visualise
ggplot(data=pdbf, aes(x=period, y=pdb, group = 1)) + geom_line()


##pdb nom = pdb riil
data3 <- read_xlsx("datarep.xlsx", sheet = "cpi")

rep1 <- left_join(pdbf, data3)

rep1$pdbr <- rep1$pdb/rep1$cpi_per
rep1$agri <- rep1$agri/rep1$cpi_per
rep1$man <- rep1$agri/rep1$cpi_per
rep1$jasa <- rep1$jasa/rep1$cpi_per


##divides into 2 periods.
rep1$subsample <- ifelse(rep1$period <= 48, 0, 1)

##gen var aggregate production (excluding the sector under consideration)

rep1 <- rep1 %>% group_by(kuartal) %>% mutate(agrimin = sum(man, jasa))
rep1 <- rep1 %>% group_by(kuartal) %>% mutate(manmin = sum(agri, jasa))
rep1 <- rep1 %>% group_by(kuartal) %>% mutate(jasamin = sum(agri, man))

##visualise
ggplot(data=rep1, aes(x=period)) +
  geom_line(aes(y=pdbr),  color = "darkred") +
  geom_line(aes(y=pdb),  color = "steelblue", linetype = "twodash")

##change data into time-series

class(rep1$kuartal)
rep1$kuartal <- yq(rep1$kuartal)
class(rep1$kuartal)

rep2 <- xts(rep1, rep1$kuartal)
class(rep2$jasa)
rep2 <- xts( rep, order.by= as.Date(rep2$kuartal))
class(rep2$pdbr)

########----------------------------------Data analysis

#######stationarity test
##observing data's stationarity using plot
##x <- subset(rep1, select = c(pdbr, agri, man, jasa, int, nex, cpi_per))

ts.plot(rep2$pdbr)
ts.plot(rep2$agri)
ts.plot(rep2$man)
ts.plot(rep2$jasa)
ts.plot(rep2$nex)
ts.plot(rep2$cpi)
ts.plot(rep2$int)


#ADF
adf.test(rep2$pdbr)
adf.test(rep2$agri)
adf.test(rep2$man)
adf.test(rep2$jasa)
adf.test(rep2$nex)
adf.test(rep2$cpi_per)
adf.test(rep2$int)


#PP
PP.test(rep2$pdbr)
PP.test(rep2$agri)
pp.test(rep2$man)
pp.test(rep2$jasa)
pp.test(rep2$nex)
pp.test(rep2$cpi_per)
pp.test(rep2$int)

#seasonality
