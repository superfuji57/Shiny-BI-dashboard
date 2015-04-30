library(shinydashboard)
library(shinythemes)
library(dygraphs)
library(ggplot2)
library(ggthemes)
library(htmlwidgets)
library(googleVis)
library(ShinyDash)

source("dashData.R")

header <- dashboardHeader(
      title = "Enterprise Demo")

sidebar <- dashboardSidebar(
      sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            
            menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
                     badgeColor = "green"),
            
            menuItem("Charts", icon = icon("bar-chart-o"),
                     menuSubItem("Sub-item 1", tabName = "subitem1"),
                     menuSubItem("Sub-item 2", tabName = "subitem2")
            )
      )
)

body <- dashboardBody(
      
      tabItems(
            tabItem("dashboard",
                    fluidRow(
                          column(width=6,
                                 h1("Commerce")),
                          column(width=6,
                                 h2("Content"))
                    ),
                    fluidRow(
                          infoBoxOutput("conversionRate", width=6),
                          infoBoxOutput("uniqueVisitors", width=6)
                    ),
                    # YoY
                    fluidRow(
                          column(width=6,
                                 p("+ 10% YOY")),
                          column(width=6,
                                 p("+ 5% YOY"))
                    ),
                    
                    fluidRow(
                          column(width=6,
                                 plotOutput("conversion"),
                                 box(
                                       status = "warning", width = NULL,
                                       "YoY performance"
                                 ),
                                 box(
                                       title = "Performance to Forecast", width = NULL, solidHeader = TRUE, status = "warning",
                                       "Box content"
                                 ),
                                 box(
                                 ) 
                          ),
                          
                          column(6,
                                 
                                 plotOutput("visitors"),
                                 
                                 box(
                                       status = "warning", width = NULL,
                                       "YoY performanc"
                                 ),
                                 box(
                                       title = "Performance to Forecast", width = NULL, solidHeader = TRUE, status = "warning",
                                       "Box content"
                                 ),
                                 box(
                                       title = "Title 5", width = NULL, background = "light-blue",
                                       "A box with a solid light-blue background"
                                 ) 
                          )
                          
                    )
            ),
            
            tabItem("widgets",
                    fluidRow(
                          box(status = "primary", width = 8, dygraphOutput("dygraph", height = 250)),
                          
                          box(title = "Controls for dygraph", background = "teal",
                              sliderInput("months", label = "Months to Predict",
                                          value = 72, min = 12, max = 144, step = 12, ticks = FALSE),
                              selectInput("interval", label = "Prediction Interval",
                                          choices = c("0.80", "0.90", "0.95", "0.99"),
                                          selected = "0.95", selectize = TRUE),
                              checkboxInput("showgrid", label = "Show Grid", value = TRUE)
                          )
                    ),
                    
                    fluidRow(
                          # Box with textOutput
                          box(title = "Status summary", background = "red", textOutput("status")),
                          # Box with HTML output, when finer control over appearance is needed
                          box(
                                title = "Status summary 2",
                                uiOutput("status2"),
                                background = "blue"
                          )
                    )
            ),
            
            tabItem("subitem1",
                    "Test"
            ),
            
            tabItem("subitem2",
                    "content"
     
            )
      )
)


server <- function(input, output) {
      output$conversionRate <- renderInfoBox({
            conversion <- percent(FYTD.commerce$Conversion[1])
            infoBox("Conversion Rate",
                  value = conversion,
                  icon = icon("credit-card"),
                  color = "yellow"
            )
      })
      
      
      
      output$uniqueVisitors <- renderInfoBox({
            visitors <- f2si2(FYTD.content$UniqueVisitors[1], rounding = TRUE)
            infoBox("Unique Visitors",
                  value = visitors,
                  subtitle = "(content sites)",
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
            YOY.commerce %>% ggplot(aes(Metrics, YOY, label=Metrics, fill = color)) +
                  geom_bar(stat = "identity", position="identity") + 
                  geom_text(aes(label = paste0( round(YOY * 100,1), "%"),
                                hjust = ifelse(YOY >= 0, 0, 1))) +
                  coord_flip() +
                  ggtitle("Year Over Year Metrics") +
                  labs(x="", y="") +
                  ylim(-.5, .5) +
                  scale_color_fivethirtyeight() + 
                  theme_fivethirtyeight() + 
                  theme(legend.position = "none",
                        plot.title = element_text(size=20, lineheight=.8, vjust=1, family = "Garamond"),
                        axis.text.y=element_text(size = 12, colour="blue", face="bold"))
            
            
      })
      
      # Visitors  Bar Plot 
      output$visitors <- renderPlot({
            YOY.content %>% ggplot(aes(Metrics, YOY, label=Metrics, fill = color)) +
                  geom_bar(stat = "identity", position="identity") + 
                  geom_text(aes(label = paste0( round(YOY * 100,1), "%"),
                                hjust = ifelse(YOY >= 0, 0, 1))) +
                  coord_flip() +
                  ggtitle("Year Over Year Metrics") +
                  labs(x="", y="") +
                  ylim(-.5, .5) +
                  scale_color_fivethirtyeight() + 
                  theme_fivethirtyeight() + 
                  theme(legend.position = "none",
                        plot.title = element_text(size=20, lineheight=.8, vjust=1, family = "Garamond"),
                        axis.text.y=element_text(size = 12, colour="blue", face="bold"))
            
      })
      
      # Predicted values for dygraph
      predicted <- reactive({
            hw <- HoltWinters(ldeaths)
            predict(hw, n.ahead = input$months,
                    prediction.interval = TRUE,
                    level = as.numeric(input$interval))
      })
      
      output$dygraph <- renderDygraph({
            dygraph(predicted(), main = "Predicted Orders Volume/Month") %>%
                  dySeries(c("lwr", "fit", "upr"), label = "Orders") %>%
                  dyOptions(drawGrid = input$showgrid)
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
      ui = dashboardPage(header, sidebar, body),
      server = server
)