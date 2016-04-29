
# Utility script for Secruity Cleaner 
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


SecruityCleaner <- function(name, days){

					# Description
					# This utility script removes non-trading days from SecruityScraper and imputes missing values with the nearest value 

					# Arguments
					#'name' is the name of a csv file containing stock symbols to download #do not include .csv
					#'days' is the number of expected non-trading days per average year, default is 144

						# Example
						# SecruityCleaner("NASDAQ", 144)

					# Requirements

					# Requires the zoo library

					# Other

					# Error "ERROR(expected) : undefined columns selected" is excpected as columns are removed and length of for-loop does not dynamically change

					#setup environment
					raw <- read.csv(paste(name,"-output.csv", sep=''))
					colnames(raw)[colnames(raw)=="X"] <- "date"
					L <- length(raw)

					# take out the non trading days aka weekends and holidays
					narows <- as.numeric(rownames(raw[rowSums(is.na(raw))>=dim(raw)[2]-10,])) 
					raw <- raw[-narows,]
					U <- dim(raw)[1]/365*(days-3) #number of years worth of data * number of non-trading days = max expected NAs #where 3 is a buffer
					L <- length(raw)

					#Remove secruites with missing observations
					for(n in 2:(L-1)){

						tryCatch({
							if (sum(is.na(raw[,n])) < U) {
									message("Pass: ", n)
								} else {
									message("Remove (not enough observations): ", n)
									raw <- raw[,-c(n)]
								}
						}, error=function(e)	{#as columns are strunk, n becomes larger than initial column number set and produces errors at the end
															cat("ERROR(expected) :",conditionMessage(e), "\n")
						})

					}

					require(zoo)
					L <- length(raw)
					end <- dim(raw)[1]

					#Impute missing price levels with nearest price level
					for(n in 2:L){

						raw[1,n] <- ifelse(is.na(raw[1,n])==TRUE, na.locf(raw[,n])[1], raw[1,n]) #place value in first 
						raw[end,n] <- ifelse(is.na(raw[end,n])==TRUE, na.locf(raw[,n])[end], raw[end,n]) #place value in last
						raw[,n] <- na.locf(raw[,n] ,fromlast=TRUE) # scrub NA from backwards
						message("Imputing missing values with nearest value: ", n)

					}

					#Remove outliers and replace them with mean
					L <- length(raw)
					for(n in 2:L){
						removed <- remove_outliers(raw[,n])
						removed[is.na(removed)] = mean(removed, na.rm=TRUE)
						raw[,n] <- removed
						message("Removing outliers and imputing with mean: ", n)
					}

write.csv(raw, file = paste(name,"-output-clean.csv", sep=''))

}