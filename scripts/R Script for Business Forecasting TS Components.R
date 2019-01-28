ACF = Auto Corelation Function

eggnog = read.csv("eggnog.csv", header = T)

head(eggnog)

#ccf(eggnog[,1], eggnog[,4], lag.max = 16, type = c("correlation", "covariance"),plot = TRUE, na.action = na.fail)

acf(eggnog[,4], lag.max = 16, type = c("correlation", "covariance"),plot = TRUE, na.action = na.fail)

acf(eggnog[,4], lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE, na.action = na.fail, demean = TRUE)

dinv = read.csv("dinvent.csv", header = T)
head(dinv)

acf(dinv[,2], lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE, na.action = na.fail, demean = TRUE)

isc = read.csv("isc.csv", header = T)
head(isc)
acf(isc[,2], lag.max = 30,type = c("correlation", "covariance", "partial"),plot = TRUE, na.action = na.fail, demean = TRUE)


x = c(3,3,3,3,3,3,3,3,3,3,3)
acf(x, lag.max = 5,type = c("correlation", "covariance", "partial"),plot = TRUE, na.action = na.fail, demean = TRUE)



#TIME SERIES DECOMPOSITION


install.packages("fpp")
dinv[,2]
decompose(dinv[,2], type = c("additive", "multiplicative"), filter = NULL)

decompose(dinv[,2], type = c("additive", "multiplicative"), filter = NULL)

 plot(dinv[,2])
 plot(dinv[,2])
 search_index <- ts(dinv[,2])
 plot(decompose(search_index))


dinv.lm = lm(Inventory~., data=dinv)
summary(dinv.lm)
predict(dinv.lm, data.frame(Day=c(1:24)))

trend = ((dinv[1,2]+dinv[2,2]+dinv[3,2]+dinv[4,2]+dinv[5,2]+dinv[6,2]+dinv[7,2]))/7


a=0
trend <- c(1:24)
trendm<-matrix(ncol=24, nrow=1)
trend
while(a<24){
trendm[a][1]<- ((dinv[a+1,2]+dinv[a+2,2]+dinv[a+3,2]+dinv[a+4,2]+dinv[a+5,2]+dinv[a+6,2]+dinv[a+7,2]))/7
}
trend[0]=6
trend



irregularity = dinv[4,2] - trend;


irregularity
xc = c(dinv[1:7,2]+
x
dinv[1,1]



house = read.csv("housesales.csv", header = T)
plot(house[,1],house[,2]) 
head(house)
house.lm = lm(Sales~Number, data=house)
summary(house.lm)
abline(house.lm)
acf(house[,2], lag.max = 30,type = c("correlation", "covariance", "partial"),plot = TRUE, na.action = na.fail, demean = TRUE)

decompose(house[,2], type = c("additive", "multiplicative"), filter = NULL)
house[,2]


