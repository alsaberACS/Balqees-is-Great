#----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")
#---------------Libraries-------------- (Step 1)
install.packages(finalfit)
install.packages(Hmisc)
install.packages(dplyr)
install.packages(ggplot2)
install.packages(tibble)
install.packages(readxl)
install.packages(data.table)
install.packages(lubridate)
install.packages(eeptools) 
install.packages(zoo)
install.packages(magrittr)
install.packages(tibble)
install.packages(compareGroups)
install.packages(data.table)
install.packages(qwraps2)
install.packages(readxl)
install.packages(plyr)
install.packages(xlsxjars)
install.packages(rJava)
install.packages(psych)
install.packages(gdata)
install.packages(expss)
install.packages(Hmisc)
install.packages(foreign)
install.packages(xlsx)
install.packages(arsenal)
install.packages(rmarkdown)

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
library(arsenal)
library(rmarkdown)
#---------------Loading Data---------- (Step 2)
Codes <- read_excel("Codes.xlsx")
Codes[, 4:7] <- sapply(Codes[, 4:7], as.numeric)
f_portion <- read_excel("EatWellQ8_Results_Final.xlsx", "Freq portion")
screening <- read_excel("EatWellQ8_Results_Final.xlsx", "Screening")
HES <- read_excel("EatWellQ8_Results_Final.xlsx", "HES")
Baecke <- read_excel("EatWellQ8_Results_Final.xlsx", "Baecke")
Feedback <- read_excel("EatWellQ8_Results_Final.xlsx", "Feedback")

NU <- read_excel("M2.xlsx")
NUN <- NU*0.01


#---------------Coding Baseline factors---------- (Step 3)
f_portion[ , 5:151] <- ifelse(f_portion[ , 5:151] == "1 per day", 0.0000,
                       ifelse(f_portion[ , 5:151] == "1 per week", 0.1429,
                       ifelse(f_portion[ , 5:151] == "1-3 in the last month", 0.0667,
                       ifelse(f_portion[ , 5:151] == "2-3 per day", 2.5000,
                       ifelse(f_portion[ , 5:151] == "2-4 per week", 0.4286,
                       ifelse(f_portion[ , 5:151] == "4-6 per day", 5.0000,
                       ifelse(f_portion[ , 5:151] == "5-6 per week", 0.7857,
                       ifelse(f_portion[ , 5:151] == "7+ per day", 6.0000,
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
                           ifelse(f_portion1[ i, j] == "None" & names(f_portion1[ i , j]) == Codes1[k,1], f_portion1[ i , j] <- as.numeric(Codes1[k,7]),0))))}
}}})

a <- as.data.frame(f_portion1[,152:298])
v <- as.data.frame(f_portion1[,5:151])
b <- matrix(0,1214,147)
c <- matrix(0,1214,147)

system.time({
for (m in 1:1214){
  for (n in 1:147){
    b[m,n] <- as.numeric(as.character(a[m,n]))
  }
}
})

system.time({
  for (m in 1:1214){
    for (n in 1:147){
      c[m,n] <- as.numeric(as.character(v[m,n]))
    }
  }
})


cb <- c * b
t2 <- data.matrix(NUN)
t3 <- cb %*% t2

Bale1 <- cbind(f_portion[ , 1],t3)
Bale1$FERQ <- sequence(rle(Bale1$userID)$lengths)
Bale2 <- left_join(Bale1,screening, by = "userID", all = TRUE)
Bale3 <- merge(Bale1,Feedback, by = "userID", all = TRUE)

r1<-tableby(~Group,data = Bale2)
r2<-tableby(~FERQ,data = Bale2)
r3<-crosstabs(Bale2,Bale2$FERQ,Bale2$Group)

write.xlsx(Bale2, file = "Bale1.xlsx", 
           sheetName="Data", append=TRUE)
write.xlsx(f_portion1, file = "f_portion1.xlsx", 
           sheetName="Data", append=TRUE)