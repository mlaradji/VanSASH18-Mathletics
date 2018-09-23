
install.packages("dplyr")

library(dplyr)

datafile = "VANSASHData.csv"

df = read.csv(datafile,encoding = "Windows-1252",stringsAsFactors = F, header =T)

summary(df)

colnames(df)

head(df)

attach(df)

VWF <- subset(df,Team.in.possession == "Vancouver Whitecaps FC")

VWF <- VWF %>%
filter(!(Action %in% c('direct free kick','penalty')))

GoalIndex <- which(VWF == 'Goal', arr.ind = TRUE)
Goalx <- GoalIndex[,1]

Goalx

Contribute <- c()

for (i in Goalx){
    t <- i
    while (VWF[i,17] !=  VWF[t,9]){
        i <- i-1
    }
    Ass <- VWF[i,17]
    Contribute <- c(Contribute,Ass)
    while (VWF[i,17] !=  VWF[t,9]){
        i <- i-1
    }
    Contribute <- Contribute <- c(Contribute,VWF[i,17])
}

Contribute

Hist <- Rle(sort(Contribute))
Count <- runLength(Hist)


T <- table(Contribute)
plot(T)
