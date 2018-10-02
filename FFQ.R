#----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")
#---------------Libraries-------------- (Step 1)
library(finalfit)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(tibble)
library(readxl)
library(data.table)
library(lubridate)
library(eeptools) 
library(zoo)
library(magrittr)
library(tibble)
library(compareGroups)
library(data.table)
library(qwraps2)
library(readxl)
library(plyr)
library(xlsxjars)
library(rJava)
library(psych)
library(gdata)
library(expss)
library(Hmisc)
library(foreign)
library(xlsx)
#---------------Loading Data---------- (Step 2)
Codes <- read_excel("Codes.xlsx")
f_portion<- read_excel("EatWellQ8_Results_Final.xlsx", "Freq portion")
M1 <- read_excel("M1.xlsx")
M2 <- read_excel("M2.xlsx")
M3 <- M2*0.01
M4 <- as.matrix(sapply(M1, as.numeric)) 
M5 <- as.matrix(sapply(M3, as.numeric)) 
M6 <- M4 %*% M5
#---------------Coding Baseline factors---------- (Step 3)
f_portion[ , 5:151] <- ifelse(f_portion[ , 5:151] == "1 per day", 1,
                       ifelse(f_portion[ , 5:151] == "1 per week", 0.14,
                       ifelse(f_portion[ , 5:151] == "1-3 in the last month", 0.03,
                       ifelse(f_portion[ , 5:151] == "2-3 per day", 2.50,
                       ifelse(f_portion[ , 5:151] == "2-4 per week", 0.43,
                       ifelse(f_portion[ , 5:151] == "4-6 per day", 4.50,
                       ifelse(f_portion[ , 5:151] == "5-6 per week", 0.79,
                       ifelse(f_portion[ , 5:151] == "7+ per day", 6.00,
                       ifelse(f_portion[ , 5:151] == "Not in the last month", 0,NA)))))))))



for (i in 1:1214){
  for (j in 152:298)
    for (k in 1:147)
    { 
      ifelse(f_portion[ i, j] == "A" & colnames(f_portion[ i , j]) == Codes$id[k], f_portion[ i , j] <- Codes$B[k], f_portion[ i , j])}}

for (i in 1:1214){
  for (j in 152:298)
    for (k in 1:147)
    { 
    ifelse(f_portion[ i, j] == "B" & colnames(f_portion[ i , j]) == Codes$id[k], f_portion[ i , j] <- Codes$B[k], f_portion[ i , j])}}

for (i in 1:1214){
  for (j in 152:298)
    for (k in 1:147)
    { 
      ifelse(f_portion[ i, j] == "C" & colnames(f_portion[ i , j]) == Codes$id[k], f_portion[ i , j] <- Codes$B[k], f_portion[ i , j])}}

for (i in 1:1214){
  for (j in 152:298)
    for (k in 1:147)
    { 
      ifelse(f_portion[ i, j] == "None" & colnames(f_portion[ i , j]) == Codes$id[k], f_portion[ i , j] <- 0, f_portion[ i , j])}}


#write.xlsx(f_portion, file = "table1.xlsx",sheetName="MTCARS", append=TRUE)

#write.xlsx(M6, file = "m6.xlsx",sheetName="m6", append=TRUE)

