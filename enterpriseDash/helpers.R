library(lubridate)
library(dplyr)

# extract first day of fiscal year
beginFY <- function(date) {
      if (month(date) < 6) {
            dayone <- paste(year(date)-1, "06-01", sep = "-")
            return((as.Date(ymd(dayone))))
      } else {
            dayone <- paste(year(date), "06-01", sep = "-")
            return((as.Date(ymd(dayone))))      
      }
}

# function to format large numbers
f2si2<-function (number,rounding=F) 
{
      lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
               0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
               1e+24)
      pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k", 
               "M", "G", "T", "P", "E", "Z", "Y")
      ix <- findInterval(number, lut)
      if (lut[ix]!=1) {
            if (rounding==T) {
                  sistring <- paste(round(number/lut[ix]), pre[ix])
            }
            else {
                  sistring <- paste(number/lut[ix], pre[ix])
            } 
      }
      else {
            sistring <- as.character(number)
      }
      return(sistring)
}

# fiscal year label
end.date <- Sys.Date()
current.year <- c(beginFY(end.date), end.date)
last.year <- c(beginFY(end.date-365), current.year[1]-1)

FY <- function(date){
      if (between(date, current.year[1], current.year[2])){
            return(factor("CY"))
      } else if (between(date, last.year[1], last.year[2])) {
            return(factor("PY"))
      } else return(factor("Year NA"))
}

# YOY
yoyR <- function(x){
      (x[1] - x[2]) / x[2]
}

yoyR2 <- function(x) {
      df <- arrange(x, desc(FY))
      df <- df[,sapply(df, is.numeric)]
      df <- data.frame(cbind(apply(df, 2, yoyR)))
      names(df) <- "YOY"
      df$Metrics <- row.names(df)
      df
}