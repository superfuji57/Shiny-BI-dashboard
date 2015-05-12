library(scales)
library(dplyr)
load("./data/enterprise.Rda")
source("helpers.R")


# add this to munging file

FYTD.commerce <- t_experienceType %>% 
      filter(name == "Commerce",
             between(datetime, last.year[1], last.year[2]) | 
                   between(datetime, current.year[1], current.year[2])) %>%
      group_by(FY) %>%
      summarize(Revenue = sum(revenue),
                Conversion = sum(orders) / sum(visits),
                Visits = sum(visits),
                PageViews = sum(pageviews),
                Orders = sum(orders),
                UniqueVisitors = sum(uniquevisitors),
                "Page Views per Visit" = round(sum(pageviews) / sum(visits), 2)) %>%
      arrange(desc(FY))

FYTD.content <- t_experienceType %>% 
      filter(name == "Content",
             between(datetime, last.year[1], last.year[2]) | 
                   between(datetime, current.year[1], current.year[2])) %>%
      group_by(FY) %>%
      summarize(Revenue = sum(revenue),
                Conversion = sum(orders) / sum(visits),
                Visits = sum(visits),
                PageViews = sum(pageviews),
                Orders = sum(orders),
                UniqueVisitors = sum(uniquevisitors),
                "Page Views per Visit" = round(sum(pageviews) / sum(visits), 2)) %>%
      arrange(desc(FY))

YOY.commerce <- yoyR2(FYTD.commerce)
YOY.content <- yoyR2(FYTD.content)

YOY.commerce$color <- ifelse(YOY.commerce$YOY < 0, "red", "green")
YOY.content$color <- ifelse(YOY.content$YOY < 0, "red", "green")
