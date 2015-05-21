library(shinydashboard)
library(shinythemes)
library(dygraphs)
library(ggplot2)
library(ggthemes)
library(htmlwidgets)
library(xts)
library(scales)

source("helpers.R")
# source("dashData.R")
load("./data/enterprise.Rda")

header <- dashboardHeader(
      title = "BI and Analytics")

sidebar <- dashboardSidebar(
      
      sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")
                     ),
            
            menuItem("Trended Metrics", icon = icon("th"), tabName = "trends", 
                     menuSubItem("Commerce", tabName = "comTab"),
                     menuSubItem("Content", tabName = "conTab")),
            
            menuItem("Charts", icon = icon("bar-chart-o"),
                     menuSubItem("Sub-item 1", tabName = "subitem1"),
                     menuSubItem("Sub-item 2", tabName = "subitem2")
            )
      ),
      selectInput("dateRange", label = h5("Date Range"),
                  choices = c("Fiscal Year-to-Date" = "FYTD", 
                              "Last 7 days" = 7,
                              "Last 30 Days" = 30,
                              "Last 6 months" = 180,
                              "Custom Date Range" = "Custom")),
      conditionalPanel(condition = "input.dateRange == 'Custom'",
                       p('Select Date Range'),
                       dateRangeInput("customDateRange",
                                      label=NULL,
                                      start = beginFY(today()-1),
                                      end = today()-1,
                                      min ='2014-06-01',
                                      max = today()-1                                                              
                       ))
)

body <- dashboardBody(
      
      tabItems(
            tabItem("dashboard",
                      fluidRow(
                          column(width=6,
                                 h1("Commerce"),
                                 selectInput("commerceSite", "Business Unit",
                                             choices = c("Overall Commerce" = "Commerce", unique(t_commerce$name)),
                                             selected = "Commerce"),
                                 infoBoxOutput("conversionRate", width=12)),
                          column(width=6,
                                 h1("Content"),
                                 selectInput("contentSite", "Business Unit",
                                             choices = c("Overall Content" = "Content", unique(t_content$name)),
                                             selected = "Content"),
                                 infoBoxOutput("engagement", width=12))
                    ),
                    # YoY
                    
                    
                    fluidRow(
                          column(width=6,
                                 
                                 box(
                                       title = "Commerce YOY Metrics Performance", width = NULL, solidHeader = TRUE, status = "danger",
                                       plotOutput("conversion")
                                 ) 
                          ),
                          
                          column(6,
                                 
                                 box(
                                       title = "Content YOY Metrics Performance", width = NULL, solidHeader = TRUE, status = "danger",
                                       plotOutput("visitors")
                                 ) 
                          )
                          
                    ),
                    fluidRow(
                          box(
                                title = textOutput("daterange"), width = 12, solidHeader=TRUE, status="warning",
                                h4("Commerce Site Totals"),
                                dataTableOutput("fytdCommerce"),
                                h4("Content Site Totals"),
                                dataTableOutput("fytdContent")
                          )
                    )
            ),
            
            tabItem("comTab",
                    fluidRow(
                          box(title = "Toggle Options", width = 3,
                              solidHeader = TRUE, collapsible = TRUE, status = "info",
                              
                              selectInput("CommerceBU", "Business Unit",
                                          choices = unique(t_commerce$name),
                                          selected = "SSO"),
                              selectInput("dateGran", "Date Granularity",
                                          choices = c("Day", "Week", "Month"),
                                          selected = "Week"),
                              checkboxInput("CommerceSync", label="Sync Charts", value = TRUE)
                          )
                    ),
                    column(12,
                           fluidRow(
                                 column(6,
                                        box(status = "primary", width = 12, dygraphOutput("comVisits", height = 250)),
                                        box(status = "primary", width = 12, dygraphOutput("comPageviews", height = 250))
                                        
                                 ),
                                 column(6,
                                        box(status = "primary", width = 12, dygraphOutput("comOrders", height = 250)),
                                        box(status = "primary", width = 12, dygraphOutput("comRevenue", height = 250))
                                 )
                           )
                    )
            ),
            tabItem("conTab",
                    fluidRow(
                          box(title = "Select Content Business Unit", background = "red",
                              selectInput("ContentBU", "Business Unit",
                                          choices = unique(t_content$name),
                                          selected = "Parents")
                          ),
                          box(title = "Select Date Granularity", background = "blue",
                              selectInput("dateGran2", "Date Granularity",
                                          choices = c("Day", "Week", "Month"),
                                          selected = "Week")
                              
                          )
                    ),
                    
                    column(12,
                           fluidRow(
                                 column(6,
                                        box(status = "primary", width = 12, dygraphOutput("conVisits", height = 250)),
                                        box(status = "primary", width = 12, dygraphOutput("conPageviews", height = 250))
                                        
                                 ),
                                 column(6,
                                        box(status = "primary", width = 12, dygraphOutput("conTime", height = 250)),
                                        box(status = "primary", width = 12, dygraphOutput("conPVV", height = 250))
                                        
                                 )
                           )
                    )
            ),
            tabItem("subitem1",
                    "stuff can go here"
            ),
            
            tabItem("subitem2",
                    verbatimTextOutput("dateTest"),
                    verbatimTextOutput("dateTest2")
                    
            )
      )
)

############### SERVER ###############
######################################

server <- function(input, output, session) {
      reactiveFileReader(10000, session, "./data/enterprise.Rda", load)
      dataList <- list(t_commerce=t_commerce, t_content=t_content, t_experienceType=t_experienceType)

      dates <- reactive({
            if (input$dateRange != "Custom") getDates(input$dateRange)
            else getDates(input$customDateRange)
                })
      
            
      output$dateTest <- renderPrint({dates()})
      output$dateTest2 <- renderPrint({class(dates())})
      
      #expType <- data()$t_experienceType
      commerceDash <- reactive({
            if (input$commerceSite == "Commerce") {
                  df <- dateFilter(t_experienceType, dates())
                  FYTD(df, "Commerce", end.date=today()-1)
            } else {
                  df <- dateFilter(t_commerce, dates())
                  FYTD(df, input$commerceSite, end.date=today()-1)
            }
      })
      
      contentDash <- reactive({
            if (input$contentSite == "Content") {
                  df <- dateFilter(t_experienceType, dates())
                  FYTD(df, input$contentSite, end.date=today()-1)
            } else {
                  df <- dateFilter(t_content, dates())
                  FYTD(df, input$contentSite, end.date=today()-1)
            }
      })
      
      output$daterange <- renderText({
            from <- dates()[1]
            to <- dates()[2]
            a <- paste(month(from, label = TRUE, abbr = FALSE), day(from))
            b <- paste(month(to, label = TRUE, abbr = FALSE), day(to))
            paste("Date Range: ", a, "through", b)
      })
      
      output$conversionRate <- renderInfoBox({
            conversion <- percent(commerceDash()$Conversion[1])
            infoBox("Conversion Rate",
                    value = conversion,
                    subtitle = "Orders over visits (Commerce sites, FYTD)",
                    icon = icon("credit-card"),
                    color = "yellow"
            )
      })
      
      
      
      output$engagement <- renderInfoBox({
            engagement <- percent(contentDash()$Engagement[1])
            infoBox("Engagement",
                    value = engagement,
                    subtitle = "Rate of monthly users active in the last week (Content sites)",
                    icon = icon("users"),
                    color = "blue"
            )
      })
      
      # Progress value
      output$progress <- renderUI({
            tagList(input$progress, tags$sup(style="font-size: 20px", "%"))
      })
      
      # Icon to show with progress
      output$progressIcon <- renderUI({
            iconName <- switch(input$progress,
                               "100" = "ok",
                               "0" = "remove",
                               "road"
            )
            icon(iconName, lib = "glyphicon")
      })
      
      
      
      # Conversion Bar Plot 
      output$conversion <- renderPlot({
            yoyR2(commerceDash()) %>% ggplot(aes(Metrics, YOY, label=Metrics, fill= color)) +
                  geom_bar(stat = "identity", position="identity") + 
                  geom_text(aes(label = paste0( round(YOY * 100,1), "%"),
                                hjust = ifelse(YOY >= 0, 0, 1))) +
                  coord_flip() +
                  labs(x="", y="") +
                  ylim(min(yoyR2(commerceDash())$YOY-.1), max(yoyR2(commerceDash())$YOY)+.1) +
                  scale_color_fivethirtyeight() + 
                  theme_fivethirtyeight() + 
                  scale_fill_manual(values = c("green" = "chartreuse3", "red" = "firebrick")) +
                  theme(legend.position = "none",
                        plot.title = element_text(size=20, lineheight=.8, vjust=1, family = "Garamond"),
                        axis.text.y=element_text(size = 12, colour="darkblue"))
            
            
      })
      
      # Visitors  Bar Plot 
      output$visitors <- renderPlot({
            yoyR2(contentDash()) %>% ggplot(aes(Metrics, YOY, label=Metrics, fill = color)) +
                  geom_bar(stat = "identity", position="identity") + 
                  geom_text(aes(label = paste0( round(YOY * 100,1), "%"),
                                hjust = ifelse(YOY >= 0, 0, 1))) +
                  coord_flip() +
                  labs(x="", y="") +
                  ylim(min(yoyR2(contentDash())$YOY)-.1, max(yoyR2(contentDash())$YOY)+.1) +
                  scale_color_fivethirtyeight() + 
                  theme_fivethirtyeight() + 
                  scale_fill_manual(values = c("green" = "chartreuse3", "red" = "firebrick")) +
                  theme(legend.position = "none",
                        plot.title = element_text(size=20, lineheight=.8, vjust=1, family = "Garamond"),
                        axis.text.y=element_text(size = 12, colour="darkblue"))
            
      })
      
      ## Raw data tables on first tab
      output$fytdCommerce <- renderDataTable(prettyR(commerceDash()),
                                             options = list(
                                                   autoWidth=TRUE, paging=FALSE,
                                                   searching = FALSE,
                                                   info = FALSE
                                             ))
      
      output$fytdContent <- renderDataTable(prettyR(contentDash()),
                                            options = list(
                                                  autoWidth=TRUE, paging=FALSE,
                                                  searching = FALSE,
                                                  info = FALSE
                                            ))
      #### NEXT TAB, trended engagement
      #### dygraphs
      
      commerceData <- reactive({
            businessUnit <- input$CommerceBU
            t_commerce %>% filter(name == businessUnit)            
      })
      
      contentData <- reactive({
            businessUnit <- input$ContentBU
            t_content %>% filter(name == businessUnit)            
      })
      
      tsGran <- function(ts, gran="Week") {
            if (gran == "Week") {
                  return(apply.weekly(ts, sum))
            } else if (gran == "Day") {
                  return(ts) 
            } else if (gran == "Month") {
                  return(apply.monthly(ts, sum))
            }
      }
      
      #### Commerce dygraphs
      comSync <- reactive({
            ifelse(input$CommerceSync == TRUE, "commerce", "")
      })
      
      output$comVisits <- renderDygraph({
            ts <- xts(commerceData()$visits, order.by=commerceData()$datetime)
            ts <- tsGran(ts, input$dateGran)
            dygraph(ts, main = "Visits", group = comSync()) %>%
                  dySeries("V1", label = "Visits")
      })
      
      
      output$comPageviews <- renderDygraph({
            ts <- xts(commerceData()$pageviews, order.by=commerceData()$datetime)
            ts <- tsGran(ts, input$dateGran)
            dygraph(ts, main = "Page Views", group = comSync()) %>%
                  dySeries("V1", label = "Page Views")
      })
      
      output$comOrders <- renderDygraph({
            ts <- xts(commerceData()$orders, order.by=commerceData()$datetime)
            ts <- tsGran(ts, input$dateGran)
            dygraph(ts, main = "Orders", group = comSync()) %>%
                  dySeries("V1", label = "Orders")
      })
      
      output$comRevenue <- renderDygraph({
            ts <- xts(commerceData()$revenue, order.by=commerceData()$datetime)
            ts <- tsGran(ts, input$dateGran)
            dygraph(ts, main = "Revenue", group = comSync()) %>%
                  dySeries("V1", label = "Revenue")
      })
      
      #### Content dygraphs
      output$conVisits <- renderDygraph({
            ts <- xts(contentData()$visits, order.by=contentData()$datetime)
            ts <- tsGran(ts, input$dateGran2)
            dygraph(ts, main = "Visits") %>%
                  dySeries("V1", label = "Visits")
      })
      
      
      output$conPageviews <- renderDygraph({
            ts <- xts(contentData()$pageviews, order.by=contentData()$datetime)
            ts <- tsGran(ts, input$dateGran2)
            dygraph(ts, main = "Page Views") %>%
                  dySeries("V1", label = "Page Views")
      })
      
      output$conTime <- renderDygraph({
            ts <- xts(contentData()$totaltimespent/60, order.by=contentData()$datetime)
            ts <- tsGran(ts, input$dateGran2)
            dygraph(ts, main = "Total Time Spent") %>%
                  dySeries("V1", label = "Minutes")
      })
      
      output$conPVV <- renderDygraph({
            ts <- xts(contentData()$pageviews / contentData()$visits, order.by=contentData()$datetime)
            ts <- tsGran(ts, input$dateGran2)
            dygraph(ts, main = "Page Views per Visit") %>%
                  dySeries("V1", label = "PVs/Visit")
      })
      
      # Status text
      output$status <- renderText({
            paste0("The number of months is ", input$months,
                   ", and the interval is ", input$interval, ".")
      })
      
      # Status with uiOutput
      output$status2 <- renderUI({
            total <- round(sum(predicted()[,"fit"]))
            if(total < 75000)
                  iconClass <- "smile-o"
            else if (total < 150000)
                  iconClass <- "meh-o"
            else
                  iconClass <- "frown-o"
            
            div(
                  "Total predicted orders in range: ",
                  div(total, style = "font-size: 30px"),
                  div(icon(iconClass), style = "font-size: 50px; text-align: right;")
            )
      })
}

shinyApp(
      ui = dashboardPage(header, sidebar, body, skin="red"),
      server = server
)