setwd("E:/KULIAH!/SEMESTER 7/Regional/Reg UAS")
library(haven)
library(tidyverse)
library(dplyr)
library(readxl)
io <- read_xlsx("Indo_160_9457240 (1).xlsx")
help(read.csv)
io <- read.csv("Indo_160_9457240 (1).csv", header = T, sep = ";")


##sakernas data cleaning
sakernas <- read_dta("layanan statistik 15_sak_08_2016_19122016.dta")

sakernas <- sakernas[!(sakernas$KODE_PROV != 64),]
sakernas <- sakernas[!(sakernas$B5_R5A1 != 1),]

colnames(sakernas)[colnames(sakernas) == "B5_R5A1"] <- "kerja"
colnames(sakernas)[colnames(sakernas) == "B5_R19_17"] <- "sektor"
colnames(sakernas)[colnames(sakernas) == "B5_R26A"] <- "gaji"

sakernas <- sakernas %>% group_by(sektor) %>% mutate(berat = sum(weight))

sakernas <- sakernas %>% group_by(sektor) %>% 
  mutate(wm = weight/berat) %>%
  mutate(wgaji = wm*gaji) 
sakernas2 <- sakernas %>% group_by(sektor) %>%
  summarise(labor = sum(weight), sal = sum(wgaji))
