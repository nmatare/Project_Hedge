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
library(ggplot2)
library(rmgarch)
library(parallel)
require(rugarch)			
require(gridExtra)
library(moments)
source("secruityscraper.R")
source("secruitycleaner.R")
setwd(datdir)

#Read in list of stocks on S&P 500 & get their data
#SecruityScraper(name="SP500", startdate=startdate, enddate=enddate, type="WIKI", key="R5aou_vNYxLJjqscyBUg", sleep=0)

#Clean data and remove non-observations
#SecruityCleaner(name="SP500", days=144)

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

#Difference log prices to find returns 
rtrn <- data.frame(apply(data[,-dim(data)[2]], MARGIN=2, diff)) #take difference of log prices
rtrn$Date <- as.character(dates[-1])
rownames(rtrn) <- sp$Date[-1]

#Method 1 Compute Correlation Matrix
L <- dim(rtrn)[2]-2 #all secruities minus SP500 and dates
timevarcor <- matrix(ncol=L, nrow=dim(rtrn)[1]-2) #Create data.frame for time conditional variance
colnames(timevarcor) <- head(colnames(rtrn), -2) #all secruites minus SP500 and dates
rownames(timevarcor) <- head(rtrn$Date,-2)

#Create parallel environment for each iteration
cl <- makeCluster(min(detectCores(),5),type=ifelse(.Platform$OS.type=="unix","FORK","PSOCK"))

for(n in 1:L){

		###Create data.frame for models #subtract 1 because of differencing
		moddata <- matrix(ncol=2, nrow=dim(rtrn)[1]-1)
		moddata[,1] <- head(rtrn[,"SP500"],-1)
		moddata[,2] <- head(rtrn[,n],-1)
		rownames(moddata) <- head(rtrn$Date,-1)
		moddata <- moddata[-nrow(moddata),] #remove last row
		moddata <- data.frame(moddata)

		#Build parameters for market GARCH
		spec1 <- ugarchspec(variance.model = list(model = "EGARCH", garchOrder = c(2,2)),
								distribution.model= "norm")
		#Build parameters for secruity GARCH
		spec2 <- ugarchspec(variance.model = list(model = "EGARCH", garchOrder = c(1,1)),
								distribution.model= "norm")
		
		#Fit GARCH models
		garch1 <- ugarchfit(spec=spec1, data=moddata[,1], solver.control = list(trace=0), cluster=cl)
		garch2 <- try(ugarchfit(spec=spec2, data=moddata[,2], solver.control = list(trace=0), cluster=cl), silent=TRUE)

		#Build parameters for DCC model 
		dccspec <- dccspec(VAR=TRUE, uspec = multispec(c(spec1, spec2)), dccOrder = c(1,1), distribution = "mvt")

		#Fit DCC model
		dcc <- try(dccfit(dccspec, data = moddata, fit.control=list(scale=TRUE), cluster=cl), silent=TRUE)

		tryCatch({

			plot(dcc, which=4)
			corrmatrix <- rcor(dcc, type="R")
			corrmatrix <- zoo(corrmatrix[1,2,], order.by=as.Date(rownames(moddata)))
			timevarcor[,n] <- corrmatrix

		}, error=function(e) {cat("ERROR :",conditionMessage(e), "\n")
		})

		message("Calculating Time Varying Correlation for Secruity: ", n, "/", L)
	
}

stopCluster(cl) 


setwd(datdir)

##Save Output
#write.csv(timevarcor, file = "varying-corr.csv")
timevarcor <- as.matrix(read.csv("varying-corr.csv"))
rownames(timevarcor) <- timevarcor[,1]

#Find top 10 correlated secruities #[,-1] removes date column
top10names <- apply(timevarcor[,-1], MARGIN=1, FUN=function(x) names(head(sort(x, decreasing=TRUE),10)))
low10names <- apply(timevarcor[,-1], MARGIN=1, FUN=function(x) names(head(sort(x, decreasing=FALSE),10)))

#Compare returns against SP&500 Returns
L <- dim(top10names)[2]
eval <- data.frame(Date=sp$Date[-1])
eval$SP500 <- rtrn[,474]
eval$benchmark <- NA
for(n in 1:L){
	date <- names(top10names[1,n])
	topstocks <- as.character(top10names[,n])
	#lowstocks <- as.character(low10names[,n])

	eval$benchmark[n] <- mean(as.numeric(rtrn[date,topstocks]))
}
eval$error <- eval$SP500 - eval$benchmark 

#Visual Representation
plot1 <-	ggplot() + 
			geom_line(data = eval, aes(x = Date, y = SP500, color = 'SP500'), color='firebrick') +
			geom_line(data = eval, aes(x = Date, y = benchmark, color = 'Benchmark'), alpha=0.4, color='slateblue') +
				ggtitle("Benchmark Superimposed on SP500") + theme(plot.title = element_text(lineheight=.8, face="bold")) +
				labs(color = "Legend") + 
				xlab('Date') +
				ylab('Price Levels')

plot2 <- ggplot() + geom_line(data = eval, aes(x = Date, y = SP500, color = 'SP500'), color='firebrick') +
				ggtitle("SP500") + theme(plot.title = element_text(lineheight=.4, face="bold")) +
				labs(color = "Legend") + 
				xlab('Date') +
				ylab('Price Levels')

plot3 <- ggplot() + geom_line(data = eval, aes(x = Date, y = benchmark, color = 'Benchmark'), color='slateblue') +
				ggtitle("Top 10 Correlated Secruities") + theme(plot.title = element_text(lineheight=.4, face="bold")) +
				labs(color = "Legend") + 
				xlab('Date') +
				ylab('Price Levels')

#png(filename="mainplot", width=1020, height=1020)
grid.arrange(plot1,plot2, plot3, ncol=1)
#dev.off()

#ggsave(paste(secr, "-graph"), plot=last_plot(), device='png')

sum(((eval$error) ** 2), na.rm=TRUE)
#sum of errors for dual eGarch 0.1836
#sum of errors for dual grjGARCH 0.1654 w/normal errors
#sum of errors for single grjGARCH(2,2) 0.1773
#sum of errors for double w/ snorm  grjGARCH(2,2) 0.1681
#sum of errors for double w/ std  grjGARCH(1,1) 0.1689
#(Mean portfolio return − Risk-free rate)/Standard deviation of portfolio retur