library(shinydashboard)
library(shinythemes)
library(dygraphs)
library(ggplot2)
library(ggthemes)
library(htmlwidgets)
library(xts)
library(scales)
library(rCharts)

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
                     menuSubItem("Content", tabName = "conTab")
            ),
            menuItem("Marketing Channels", tabName = "ltc",
                     menuSubItem("Key Experience", "ltcExperienceType"),
                     menuSubItem("Commerce", "ltcCommerce"),
                     menuSubItem("Content", "ltcContent")
            )
      ),
      selectInput("dateRange", label = h5("Dashboard Date Range"),
                  choices = c("Fiscal Year-to-Date" = "FYTD",
                              "Month-to-Date" = "MTD",
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
                          box(title = "Select Commerce Business Unit", background = "red",
                              selectInput("CommerceBU", "Business Unit",
                                          choices = unique(t_commerce$name),
                                          selected = "SSO")
                          ),
                          box(title = "Select Date Granularity", background = "blue",
                              selectInput("dateGran", "Date Granularity",
                                          choices = c("Day", "Week", "Month"),
                                          selected = "Week")
                              
                          )
                    ),
                    column(12,
                           checkboxInput("comTabSync",
                                         label = "Sync with dashboard date range (from left)",
                                         value = FALSE
                           ),
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
                           
                           checkboxInput("conTabSync",
                                         label = "Sync with dashboard date range (from left)",
                                         value = FALSE
                           ),
                           
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
            
            ## Marketing Channels Tabs
            tabItem("ltcExperienceType",
                    selectInput(inputId = "ltcExperienceType",
                                label = "Choose Site",
                                choices = unique(ltc.experienceType$Site),
                                selected = unique(ltc.experienceType$Site)[1]
                    ),
                    
                    checkboxInput("ltcExperienceTypeDates",
                                  label = "Sync with dashboard date range (from left)",
                                  value = FALSE
                    ),
                    
                    showOutput("ltcExperienceTypeLineChart", "nvd3")
                    
            ),
            
            tabItem("ltcCommerce",
                    selectInput(inputId = "ltcCommerceSite",
                                label = "Choose Site",
                                choices = unique(ltc.commerce$Site),
                                selected = unique(ltc.commerce$Site)[1]
                    ),
                    
                    checkboxInput("ltcCommerceDates",
                                  label = "Sync with dashboard date range (from left)",
                                  value = FALSE
                    ),
                    
                    showOutput("ltcCommerceLineChart", "nvd3")
                    
            ),
            
            tabItem("ltcContent",
                    selectInput(inputId = "ltcContentSite",
                                label = "Choose Site",
                                choices = unique(ltc.content$Site),
                                selected = unique(ltc.content$Site)[1]
                    ),
                    
                    checkboxInput("ltcContentDates",
                                  label = "Sync with dashboard date range (from left)",
                                  value = FALSE
                    ),
                    
                    showOutput("ltcContentLineChart", "nvd3")
                    
            )
      )
)

############### SERVER ###############
######################################

server <- function(input, output, session) {
      reactiveFileReader(10000, session, "./data/enterprise.Rda", load)
      
      # Dates from selected input
      dates <- reactive({
            if (input$dateRange != "Custom") getDates(input$dateRange)
            else getDates(input$customDateRange)
      })
      
      # FYTD totals for current and previous year
      commerceDash <- reactive({
            if (input$commerceSite == "Commerce") {
                  df <- dateFilter(t_experienceType, dates())
                  FYTD(df, "Commerce", end.date=max(dates()$datetime))
            } else {
                  df <- dateFilter(t_commerce, dates())
                  FYTD(df, input$commerceSite, end.date=max(dates()$datetime))
            }
      }) 
      contentDash <- reactive({
            if (input$contentSite == "Content") {
                  df <- dateFilter(t_experienceType, dates())
                  FYTD(df, input$contentSite, end.date=max(dates()$datetime))
            } else {
                  df <- dateFilter(t_content, dates())
                  FYTD(df, input$contentSite, end.date=max(dates()$datetime))
            }
      })
      
      # Text to print date range used
      output$daterange <- renderText({
            from <- dates()[1]
            to <- dates()[2]
            a <- paste(month(from, label = TRUE, abbr = FALSE), day(from))
            b <- paste(month(to, label = TRUE, abbr = FALSE), day(to))
            paste("Date Range: ", a, "through", b)
      })
      
      # Main info box on commerce/left side with conversion rate
      output$conversionRate <- renderInfoBox({
            conversion <- percent(commerceDash()$Conversion[1])
            infoBox("Conversion Rate",
                    value = conversion,
                    subtitle = "Orders over visits (Commerce sites, FYTD)",
                    icon = icon("credit-card"),
                    color = "yellow"
            )
      })
      
      # Main info box on content/right side with engagement rate
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
            deltaChart(commerceDash())
      })
      
      # Visitors  Bar Plot 
      output$visitors <- renderPlot({
            deltaChart(contentDash())
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
            if (input$comTabSync == TRUE){
                  comTabDates <- dates()
            } else {
                  comTabDates <- NULL
            }
            dateFilter(t_commerce, comTabDates, onlyCY=TRUE) %>%
                  filter(name == input$CommerceBU)
      })
      
      contentData <- reactive({
            if (input$conTabSync == TRUE){
                  conTabDates <- dates()
            } else {
                  conTabDates <- NULL
            }
            dateFilter(t_content, conTabDates, onlyCY=TRUE) %>%
                  filter(name == input$ContentBU)            
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
      output$comVisits <- renderDygraph({
            ts <- xts(commerceData()$visits, order.by=commerceData()$datetime)
            ts <- tsGran(ts, input$dateGran)
            dygraph(ts, main = "Visits") %>%
                  dySeries("V1", label = "Visits")
      })
      
      
      output$comPageviews <- renderDygraph({
            ts <- xts(commerceData()$pageviews, order.by=commerceData()$datetime)
            ts <- tsGran(ts, input$dateGran)
            dygraph(ts, main = "Page Views") %>%
                  dySeries("V1", label = "Page Views")
      })
      
      output$comOrders <- renderDygraph({
            ts <- xts(commerceData()$orders, order.by=commerceData()$datetime)
            ts <- tsGran(ts, input$dateGran)
            dygraph(ts, main = "Orders") %>%
                  dySeries("V1", label = "Orders")
      })
      
      output$comRevenue <- renderDygraph({
            ts <- xts(commerceData()$revenue, order.by=commerceData()$datetime)
            ts <- tsGran(ts, input$dateGran)
            dygraph(ts, main = "Revenue") %>%
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
      
      ### LAST TOUCH CHANNEL
      chartDataCommerce <- reactive({
            if (input$ltcCommerceDates == TRUE){
                  ltcCommerceDates <- dates()
            } else { 
                  ltcCommerceDates <- NULL}
            dateFilter(ltc.commerce, ltcCommerceDates, onlyCY=TRUE) %>%
                  filter(Site == input$ltcCommerceSite) %>% 
                  mutate(weekStart = floor_date(datetime, "week")) %>% 
                  group_by(weekStart, Channel) %>% 
                  summarize(visits = sum(visits))
      })
      
      output$ltcCommerceLineChart <- renderChart({
            linePlot <- nPlot(visits ~ weekStart, group = 'Channel', data = chartDataCommerce(), 
                              type = "lineWithFocusChart", dom = 'ltcCommerceLineChart', width = 800)
            linePlot$xAxis( tickFormat="#!function(d) {return d3.time.format('%x')(new Date( d * 86400000 ));}!#" )
            linePlot$yAxis(tickFormat = "#! function(d) {return d3.format(',0f')(d)} !#")
            #linePlot$y2Axis(tickFormat = "#! function(d) {return d3.format(',0f')(d)} !#")
            dat <- data.frame(x = numeric(0), y = numeric(0))
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            progress$set(message = "Making plot", value = 0)
            
            # Number of times we'll go through the loop
            n <- 10
            
            for (i in 1:n) {
                  # Each time through the loop, add another row of data. This is
                  # a stand-in for a long-running computation.
                  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  
                  # Increment the progress bar, and update the detail text.
                  progress$inc(1/n, detail = paste("Loading...", i))
                  
                  # Pause for 0.1 seconds to simulate a long computation.
                  Sys.sleep(0.1)
            }
            
            plot(dat$x, dat$y)
            return(linePlot)
      })
}

shinyApp(
      ui = dashboardPage(header, sidebar, body, skin="red"),
      server = server
)