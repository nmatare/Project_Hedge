

SecruityScraper <- function(name, startdate, enddate, type, key, sleep){

					# Description
					# This utility script scraps Quandl.com for data information given a date range and symbol list

					# Arguments

					#'name' is the name of a csv file containing stock symbols to download #do not include .csv
					#'startdate' is the start date of data
					#'enddate' is the end data of data
					#'type' is the Quandl database header; this should be input as a character IE 'WIKI'
					#'key' is the Quandl key
					#'sleep' is the number of seconds to wait before querying Quandl server

						# Example
						# SecruityScraper("NASDAQ", "2001-09-11", "2015-01-01", "WIKI", "40F..U&E")

					# Requirements

					# Requires the Quandl library
					# Requires loaded csv file to have a column header of "ticker" for all secruity symbols
					# Startdate and enddate must not be same date
					
					#setup environment
					name <- name
					raw <- read.csv(paste(name,".csv", sep=''))
					tickers <- as.character(raw$ticker)

					enddate <- enddate
					startdate <- startdate
					dates <- seq.Date(as.Date(startdate), as.Date(enddate), by='day') #create daily dates

					#allocate memory for data
					L <- length(tickers)
					D <- length(dates) 
					dataset <- matrix(ncol=L, nrow=D)
					dimnames(dataset) <- list(rownames(dataset, do.NULL = FALSE, prefix = "row"), 
											  colnames(dataset, do.NULL = FALSE, prefix = "col"))
					colnames(dataset) <- tickers
					rownames(dataset) <- as.character(dates)

					#specify date range
					enddate <- dates[length(dates)]
					startdate <- dates[1]
					header <- paste(type, "/", sep="")

					#retrive stock data
					require(Quandl)
					Quandl.api_key(key)

					for(i in 1:L){

						tryCatch({
						sym <- paste(header, tickers[i], sep="")
						info <- Quandl(sym, start_date=startdate, end_date=enddate)
						tempdate <- info$Date

						info <- data.frame(info$Close)
						rownames(info) <- tempdate
						put <- merge(info, dataset[,i], by=0, all=TRUE)
						dataset[,i] <- put$info.Close
						message("Scraping data for stock: ", tickers[i], " | Number: ", i, "/", L)

						Sys.sleep(sleep) #API speed limit 
						}, error=function(e)	{
												cat("ERROR :",conditionMessage(e), "\n")
						#Last value throws error
						})

					}

					#export data 
					write.csv(dataset, file = paste(name,"-output.csv", sep=''))

}
