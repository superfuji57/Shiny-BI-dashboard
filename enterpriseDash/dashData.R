library(scales)
library(dplyr)
load("./data/enterprise.Rda")


f2si2<-function (number,rounding=F) # function to format large numbers
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

# Randomize
randomeyez <- function(df){
      r_df <- transform(df,
                        visits= visits + round(visits * runif(length(visits), -1, 2)),
                        uniquevisitors = uniquevisitors + round(uniquevisitors * runif(length(visits), -1, 2)),
                        pageviews= pageviews + round(pageviews * runif(length(pageviews), -1, 2)),
                        orders= orders + round(orders * runif(length(orders), -1, 2)),
                        revenue= revenue + round(revenue * runif(length(revenue), -1, 2))
      )
      return(r_df)
}

# add this to munging file

YTD.revenue <- t_experienceType %>% 
      filter(name == "Commerce",
             between(datetime, last.year[1], last.year[2]) | 
                   between(datetime, current.year[1], current.year[2])) %>%
      group_by(FY) %>%
      summarize(Revenue = f2si2(sum(revenue), rounding=TRUE),
                Conversion = percent(sum(orders) / sum(visits)))
      
      
      
      sum(filter(t_experienceType, name=="Commerce", datetime )$revenue)
YTD.revenue <- paste0("$", f2si2(YTD.revenue, rounding=TRUE))

YTD.conversion <- sum(filter(t_experienceType, name=="Commerce")$orders)/sum(filter(t_experienceType, name=="Commerce")$visits)
YTD.conversion <- percent(YTD.conversion)

YTD.visitors <- sum(filter(t_experienceType, name=="Content")$uniquevisitors)
YTD.visitors <- f2si2(YTD.visitors, TRUE)


