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
NU <- read_excel("M2.xlsx")
NUN <- NU*0.01


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


f_portion1 <- as.matrix(f_portion)
Codes1 <- as.matrix(Codes)

system.time({
for (i in 1:1214){
  for (j in 152:298){
    for (k in 1:147){ 
      ifelse(f_portion1[ i, j] == "A" & names(f_portion1[ i , j]) == Codes1[k,1], f_portion1[ i , j] <- as.numeric(Codes1[k,4]),
             ifelse(f_portion1[ i, j] == "B" & names(f_portion1[ i , j]) == Codes1[k,1], f_portion1[ i , j] <- as.numeric(Codes1[k,5]),
                    ifelse(f_portion1[ i, j] == "C" & names(f_portion1[ i , j]) == Codes1[k,1], f_portion1[ i , j] <- as.numeric(Codes1[k,6]),
                           ifelse(f_portion1[ i, j] == "None" & names(f_portion1[ i , j]) == Codes1[k,1], f_portion1[ i , j] <- 0,0))))}
}}})


a <- as.data.frame(f_portion1[ , 152:298])
t1 <- data.matrix(a)
t2 <- data.matrix(NUN)
t3 <- t1 %*% t2


#FP <- as.matrix(sapply(f_portion1, as.numeric))
#NUN1 <- as.matrix(sapply(NUN, as.numeric))
#FP1 <- as.matrix(sapply(FP, as.numeric))
#PD <- FP1[,152:298] %*% NUN1

#write.xlsx(f_portion, file = "table1.xlsx",sheetName="MTCARS", append=TRUE)

#write.xlsx(M6, file = "m6.xlsx",sheetName="m6", append=TRUE)

