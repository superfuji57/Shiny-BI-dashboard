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
f2si2<-function (number,rounding=F, digits=0) 
{
      lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
               0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
               1e+24)
      pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k", 
               "M", "G", "T", "P", "E", "Z", "Y")
      ix <- findInterval(number, lut)
      if (lut[ix]!=1) {
            if (rounding==T) {
                  sistring <- paste(round(number/lut[ix], digits), pre[ix])
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
            return(factor(paste0("FY", as.character(year(current.year[1])+1))))
      } else if (between(date, last.year[1], last.year[2])) {
            return(factor(paste0("FY", as.character(year(last.year[1])+1))))
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
      df$color <- ifelse(df$YOY < 0, "red", "green")
      df
}

# Pretty table output
prettyR <- function(df){
      perMetrics <- c("Conversion", "Engagement", "Engagement WUV/MUV")
      totMetrics <- c("Visits", "Revenue", "PageViews", "Orders", "UniqueVisitors")
      decMetrics <- c("Page Views per Visit", "Avg Time Spent")
      df[, names(df) %in% perMetrics] <- apply(df[, names(df) %in% perMetrics],1, percent)      
      df[, names(df) %in% totMetrics] <- sapply(df[, names(df) %in% totMetrics], function(x) f2si2(x, rounding=TRUE, digits=1))            
      df            
}

# FYTD dataframe
FYTD <- function(df, experience, end.date = today()-1, ...){
      current.year <- c(beginFY(end.date), end.date)
      last.year <- c(beginFY(end.date-365), end.date-365)
      content.sites <- c("Content", "TeacherContent", "Parents", "Kids")
      commerce.sites <- c("Commerce", "TSO CQS", "SSO", "BookFairsExperience", "Classroom Magazines",
                          "Printables MiniBooks", "Teacher Express", "BookClubsExperience")
      if (experience %in% commerce.sites) {
            df %>% filter(name == experience,
                          between(datetime, last.year[1], last.year[2]) | 
                                between(datetime, current.year[1], current.year[2])) %>%
                  group_by(FY) %>%
                  summarize(Revenue = sum(revenue),
                            Conversion = sum(orders) / sum(visits),
                            Visits = sum(visits),
                            PageViews = sum(pageviews),
                            Orders = sum(orders),
                            #UniqueVisitors = sum(uniquevisitors),
                            "Page Views per Visit" = round(sum(pageviews) / sum(visits), 1)) %>%
                  arrange(desc(FY))            
      } else if (experience %in% content.sites) {
            df %>% filter(name == experience,
                          between(datetime, last.year[1], last.year[2]) | 
                                between(datetime, current.year[1], current.year[2])) %>%
                  group_by(FY) %>%
                  summarize(#Revenue = sum(revenue),
                        #Conversion = sum(orders) / sum(visits),
                        Visits = sum(visits),
                        PageViews = sum(pageviews),
                        #Orders = sum(orders),
                        #UniqueVisitors = sum(uniquevisitors),
                        "Page Views per Visit" = round(sum(pageviews) / sum(visits), 1),
                        TimeSpent = round(sum(totaltimespent/60) / sum(visits), 1),
                        "Engagement WUV/MUV" = round(sum(tail(visitorsweekly,7)) / sum(tail(visitorsmonthly,30)), 3) ) %>%
                  arrange(desc(FY))
      } else return(warning("Invalid experience type"))
}