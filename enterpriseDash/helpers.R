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

FY <- function(date){
      end.date <- Sys.Date()
      current.year <- c(beginFY(end.date), end.date)
      last.year <- c(beginFY(end.date-365), current.year[1]-1)
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
      if ("Current" %in% x$FY){
            df <- arrange(x, FY)
      } else {
            df <- filter(x, FY != "Year NA") %>% 
                  arrange(desc(FY))      
      }
      
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
FYTD <- function(df, experience, ...){
      end.date <- max(df$datetime)
      if (!is.null(df$Year)){
            begin <- min(df$datetime[df$Year == "This Year"])
            current.year <- c(begin, end.date)
            last.year <- c(begin-365, end.date-365)
            df$FY <- df$Year
      } else {
            current.year <- c(beginFY(end.date), end.date)
            last.year <- c(beginFY(end.date-365), end.date-365)
            
      }
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

## Filter with custom date range input
getDates <- function(inputDate, today=NULL){
      if (is.null(today)) {today <- today()-1}
      if (length(inputDate) == 2) {
            dateRange <- inputDate
      } else if (inputDate == "FYTD") {
            dateRange <- c(beginFY(today), today)
      } else if (inputDate == "MTD") {
            dateRange <- c(floor_date(today, "month"), today)
      } else if (as.numeric(inputDate) > 0 & !is.na(as.numeric(inputDate))) {
            dateRange <- c(today - as.numeric(inputDate), today)
      }
      as.Date(dateRange)
}


dateFilter <- function(df, inputDate=NULL, onlyCY = FALSE){
      if (is.null(inputDate)) df
      else {
            cy <- inputDate
            py <- c(cy[1]-365, cy[2]-365)
            if (onlyCY == FALSE) {
                  df <- dplyr::filter(df, between(datetime, cy[1], cy[2]) |
                                            between(datetime, py[1], py[2]))
                  if (length(unique(df$FY)) > 2){
                        df$Year[between(df$datetime, cy[1], cy[2])] <- "This Year"
                        df$Year[between(df$datetime, py[1], py[2])] <- "Previous Year"
                        df$FY <- df$Year
                  }      
            } else {
                  df <- dplyr::filter(df, between(datetime, cy[1], cy[2]))
            }
            df
      }
}
### Delta charts functions
signR <- function(x){
      if (sum(x < 0) == length(x)) return("All Neg")
      else if (sum(x > 0) == length(x)) return("All Pos")
      else return(FALSE)
}

yLims <- function(x) {
      if (signR(x) == "All Neg"){
            ylim(min(x) - .1, 0)
      } else if (signR(x) == "All Pos") {
            ylim(0, max(x) + .1)
      } else {
            ylim(min(x) - .1, max(x) + .1)
      }
}

deltaChart <- function(fytdData){
      data <- yoyR2(fytdData)
      data %>% ggplot(aes(Metrics, YOY, label=Metrics, fill = color)) +
            geom_bar(stat="identity", position="identity") +
            geom_text(aes(label = paste0(round(YOY * 100, 1), "%"),
                          hjust = ifelse(YOY >= 0, 0, 1))) +
            coord_flip() +
            labs(x="", y="") +
            yLims(data$YOY) +
            scale_color_fivethirtyeight() + 
            theme_fivethirtyeight() + 
            scale_fill_manual(values = c("green" = "chartreuse3", "red" = "firebrick")) +
            theme(legend.position = "none",
                  plot.title = element_text(size=20, lineheight=.8, vjust=1, family = "Garamond"),
                  axis.text.y=element_text(size = 12, colour="darkblue"))
}

d3LineChart <- function(df, dom) {
      linePlot <- nPlot(visits ~ weekStart, group = 'Channel', data = df, 
                        type = "lineWithFocusChart", dom = dom, width = 800)
      linePlot$xAxis( tickFormat="#!function(d) {return d3.time.format('%x')(new Date( d * 86400000 ));}!#" )
      linePlot$yAxis(tickFormat = "#! function(d) {return d3.format(',0f')(d)} !#")
      
      return(linePlot)
}

d3Loading <- function(...){
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Making plot", value = 0)
      
      # Number of times we'll go through the loop
      n <- 100
      
      for (i in 1:n) {
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            
            # Increment the progress bar, and update the detail text.
            progress$inc(1/n, detail = paste("Loading...", i))
            
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.01)
      }
      
}