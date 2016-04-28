####
#### Project Hedgable
####

##Objectives

#1. Mimic monthylreturns S&P 500 over 10 years
# Subject to stocks
# Holding no more than 10 stocks

###Set Global Vars & Directory Information
rm(list=ls(all=TRUE))
options("width"=200)
options(scipen=999)
options(digits=4)
wkdir <- "/home/johnconnor/projecthedge/"
lbdir <- "/home/johnconnor/projecthedge/lib/"
datdir <- "/home/johnconnor/projecthedge/data/"

##Set Parameters
enddate <- Sys.Date()
startdate <- enddate - 365*10 #appox 10 years of data, not counting trading days

#Load required libraries & utility scripts
setwd(lbdir)
library(Quandl)
library(zoo)
library(rmgarch)
library(parallel)
#Create parallel environment
cl <- makeCluster(min(detectCores(),5),type=ifelse(.Platform$OS.type=="unix","FORK","PSOCK"))
require(rugarch)
require(PerformanceAnalytics)
source("secruityscraper.R")
source("secruitycleaner.R")
setwd(datdir)

#Read in list of stocks on S&P 500 & get their data
SecruityScraper(name="SP500", startdate=startdate, enddate=enddate, type="WIKI", key="R5aou_vNYxLJjqscyBUg", sleep=0)

#Clean data and remove non-observations
SecruityCleaner(name="SP500", days=144)

#Read in clean data & append S&P500 price levels
raw <- read.csv("SP500-output-clean.csv")
dates <- raw$date
raw <- raw[,-c(1)]
#append S&P 500 for comparision
sp <- Quandl("YAHOO/INDEX_GSPC", start_date=startdate, end_date=enddate)
sp <- sp[-c(2,3,4,6,7)] #keep only dates and close date
sp <- sp[order(sp$Date),] #reorder data
rows <- match(as.character(sp[,1]),as.character(raw[,1])) #match SP levels to clean dataset
raw <- raw[rows,]
dates <- dates[rows]
raw$SP500 <- sp[,2] #append S&P 500 to vector list

#Convert data to log level and take difference
data <- data.frame(apply(raw[,-1], MARGIN=2, log)) #convert to log prices
data$Date <- as.character(dates)


rtrn <- data.frame(apply(data[,-dim(data)[2]], MARGIN=2, diff)) #take difference of log prices
rtrn$Date <- as.character(dates[-1])

###
#Method 1 Compute Correlation Matrix
L <- dim(rtrn)[2]-2 #all secruities minus SP500 and dates
timevarcor <- matrix(ncol=L, nrow=dim(rtrn)[1]) #Create data.frame for time conditional variance
colnames(timevarcor) <- head(colnames(rtrn), -2) #all secruites minus SP500 and dates
rownames(timevarcor) <- rtrn$Date

for(n in 1:L){

	###Create data.frame for models 
	moddata <- data.frame(market=rtrn[,"SP500"])
	moddata$secruity <- rtrn[,n]
	rownames(moddata) <- rtrn$Date

	#Build parameters for market GARCH
	spec1 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(2,2)),
							distribution.model= "std")
	#Build parameters for secruity GARCH
	spec2 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
							distribution.model= "std")

	#Fit GARCH models
	garch1 <- ugarchfit(spec=spec1, data=moddata[,1], solver.control = list(trace=0), cluster=cl)
	garch2 <- ugarchfit(spec=spec2, data=moddata[,2], solver.control = list(trace=0), cluster=cl)

	#Build parameters for DCC model
	dccspec <- dccspec(uspec = multispec(c(spec1, spec2)), dccOrder = c(1,1), distribution = "mvnorm")

	#Fit DCC model
	dcc <- dccfit(dccspec, data = moddata, fit.control=list(scale=TRUE), cluster=cl)

	#plot and show time varying correlation
	plot(dcc, which=4)
	corrmatrix <- rcor(dcc, type="R")
	corrmatrix <- zoo(corrmatrix[1,2,], order.by=as.Date(rownames(moddata)))
	timevarcor[,n] <- corrmatrix
	message("Calculating Time Varying Correlation for Secruity: ", n, "/", L)

}

stopCluster(cl)

setwd(datdir)
write.csv(timevarcor, file = "varying-corr.csv")
timevarcor <- as.matrix(read.csv("varying-corr.csv"))

#Find top 10 correlated secruities #[,-1] removes date column
top10names <- apply(timevarcor[,-1], MARGIN=1, FUN=function(x) names(head(sort(x, decreasing=TRUE),10)))
top10rtrn <- apply(timevarcor[,-1], MARGIN=1, FUN=function(x) head(sort(x, decreasing=TRUE),10)))

#Compare returns against SP&500 Returns
eval <- data.frame(Date=sp$Date[-1])
eval$SP500 <- rtrn[,474]
eval$benchmark <- NA

for(i in 1:L){

	calc <- mean(top10rtrn[i]) #equally weighted
	eval$benchmark[i] <- calc
}







##
#Method 2 Optimze based upon Beta

xusd <- merge(xu100.zoo,usd)
# Lets use USD based xu100 and usd series.. 
xusd <- xusd[,c("USD","usd")]
colnames(xusd)<-c("BIST","TL-USD")