
inst <- function(){
  require(roperators)
  library(tidyquant)
  install.packages("svDialogs")
  library(svDialogs)
  options("getSymbols.warning4.0"=FALSE)
  options("getSymbols.yahoo.warning"=FALSE)
}
clip <- function(r, minusEps, plusEps){
  if (r < minusEps) 
  {
    r <- minusEps
    return (r)
  }
  else if (r > plusEps)
  {
    r <- plusEps
    return (r)
  }
  else
  {
    return (r)
  }
  
}
stanDev <- function(closings, m)
{
  nClosings <- length(closings)
  t <- vector()
  for (i in 1:nClosings)
  {
    sq <- (closings[i]-m) ^ 2
    t <- c(t,sq )
  }
  t <- sum(t) 
  return (sqrt(t/nClosings))
}
inst()

balance <- 0
while (isTRUE(balance == 0)  )
{
 
  
  while (TRUE && balance < 100){
    balance <- dlgInput("How much do you have totally?")$res
    balance <- strtoi(balance)
    print(balance)
    if (isTRUE(balance < 100 ))
    {

      print ("TRY AGAIN! PLEASE ENTER A POSITIVE NUMBER NO LESS THAN 100")
    }
    else
    {
      break
    }
  }
}

symbol <- dlgInput("Enter a stock symbol")$res
fromDate <- dlgInput("Starting date (yyyy-mm-dd)")$res

toDate <- dlgInput("Ending date (yyyy-mm-dd)")$res
selectStock <- tq_get(symbol,
                      from = fromDate,
                      to = toDate,
                      get = "stock.prices")

plot(selectStock$date, selectStock$close, main=symbol, xlab="Closing Dates", ylab="Close Amounts", col="blue", type="l")

grid()
G <- c(selectStock$close[1])
print(symbol)
print(G)
gamma <- 0.9
S=vector()
for (k in 1:length(selectStock$close)-1) {
  cl1=selectStock$close[k+1]
  cl2=selectStock$close[k]
  R <-  cl1-cl2
  S <- c(S, R * (gamma ^ (k-1)))
}

R <- G + sum(S)

B <- mean(selectStock$close)

print(R)
print(B)
advantageEstimate <- R - B
rOld <- 0
rNew <- 1
numberOfShares <- 0
amountBought <- 0
stockBought <- FALSE
epsilon=0.2
purchases <- c(0)
for (i in 1:length(selectStock$close)){
  if (i == 1){
    rNew <- 1
  }
  else
  {
    rOld=rNew
    r=selectStock$close[i]/selectStock$close[i-1]
   # print (paste("r is now ", toString(r)))
    clp=clip(r,1-epsilon,1+epsilon)
   # print (paste("here's clip: ", toString(clp)))
    rNew=(selectStock$close[i]/selectStock$close[i-1]) * advantageEstimate
    L_Clip=min(c(rNew,clp))
    #print(paste("FINALLY, Lclip: ", toString(L_Clip)))
    if (selectStock$close[i] > selectStock$close[i-1])
    {
      if (stockBought == FALSE)
      {
        writeLines(paste0(toString(selectStock$date[i]), ' Buying stock ' ))
        investPercent <- runif(1, 0.25,0.75)
        amountBought<-investPercent * balance
        closingDay<-selectStock$close[i]
        numberOfShares<-amountBought/closingDay
        stockBought<-TRUE
        strNofShares<-toString(round(numberOfShares,2))
        strAmountBought<-toString(round(amountBought,2))
        balance <- balance - amountBought
        writeLines(paste0(toString(selectStock$date[i]),"Bought ", strNofShares, " shares for $", strAmountBought))
        writeLines(paste0("\tCurrent balance: $", toString(round(balance,2))))
        numPurchases=length(purchases) 
        purchases <- c(purchases,amountBought)
      }
      else
      {
         closingDay<-selectStock$close[i] 
         amountBought <- numberOfShares * closingDay
         writeLines(paste0(toString(selectStock$date[i]),"Holding stock at $", toString(round(amountBought,2) )))
         numPurchases=length(purchases) +1
         purchases <- c(purchases,amountBought)
      }
    }
    else
    {
      if (stockBought)
      {
       
        writeLines(paste0(toString(selectStock$date[i]),'Selling stock ' ))
        amountBought <- numberOfShares * closingDay
        balance <- balance + amountBought
        
        writeLines(paste0("\tSold shares for $", toString(round(amountBought,2))))
        writeLines(paste0("\tCurrent balance: ", toString(round(balance,2))))
        stockBought <- FALSE
        amountBought <- 0
        numPurchases=length(purchases) +1
        purchases <- c(purchases,amountBought)
      }
      else
      {
        print(paste0(toString(selectStock$date[i]),'Not yet ready to buy ' ))
        numPurchases=length(purchases) +1
        purchases <- c(purchases,amountBought)
      }
    }
  }
  
}
plot(selectStock$date,purchases,type="l", col="green")
grid()
summaryString <- c(paste("SYMBOL: ", symbol))
currentClosing=round(selectStock$close[length(selectStock$close)],2)
#print (paste("CURRENT CLOSING: $", currentClosing))
summaryString <- c(summaryString, c(paste("CURRENT CLOSING: $", currentClosing)))

closeMean <- round(mean(selectStock$close), 2)
print(paste("MEAN: $", closeMean))
summaryString <- c(summaryString,c(paste("MEAN: $", closeMean)))

closeMax <- round(max(selectStock$close),2)
maxDate <- which.max(selectStock$date)
#print(paste("MAX: $", closeMax,  toString(selectStock$date[maxDate])))
summaryString <- c(summaryString,c(  paste("MAX: $", closeMax,  toString(selectStock$date[maxDate]))))
closeMin <- round(min(selectStock$close),2)
minDate <- which.min(selectStock$date)
#print(paste("MIN: $", closeMin, " ", selectStock$date[minDate]))
summaryString <- c(summaryString,c(paste( "MIN: $", closeMin, toString(selectStock$date[minDate]))))
closeMedian <- round(median(selectStock$close),2)
print(paste("MEDIAN: $", closeMedian))
summaryString <- c(summaryString,c( paste("MEDIAN: $", closeMedian)))
closeMode <- names(sort(-table(selectStock$close)))[1]
#print(paste("MODE: $", round(as.double(closeMode,2))))
summaryString <- c(summaryString,c(paste("MODE: $",round(as.double(closeMode,2)))))
sd <- stanDev(selectStock$close, closeMean) 
#print(paste("STANDARD DEVIATION: ", round(sd,2)))
summaryString <- c(summaryString,c(paste("STANDARD DEVIATION: ", round(sd,2))))
percentile <- quantile(selectStock$close,c(0.8))
summaryString <- c(summaryString,c( paste("80% PERCENTILE: $", round(percentile,2))))
writeLines(summaryString)
winDialog("ok", toString(summaryString))












