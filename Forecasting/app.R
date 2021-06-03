## app.R ##
install.packages("shinydashboard")
install.packages("forecast")
install.packages("nnfor")
library(shinydashboard)

ui <- dashboardPage(
    header <- dashboardHeader(title = "O.S Forecasting"),
    
    ## Sidebar content
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Rice Production", tabName = "dashboard1", icon = icon("chart-bar")),
            menuItem("Rice Price", tabName = "dashboard2", icon = icon("chart-bar")),
            menuItem("Controller", tabName = "controller", icon = icon("th"))
        )
    ),
    ## Body content
    body <- dashboardBody(
        tabItems(
            # First Tab Content
            tabItem(tabName = "dashboard1",
                    fluidRow(
                        box(title = "Forecast Rice Produce Holt-Winter Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot1", height = 400),
                            verbatimTextOutput("detail")
                            ),
                        box(title = "Forecast Rice Produce MLP Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot2", height = 400),
                            verbatimTextOutput("detail2")
                            ),
                        box(title = "Forecast Rice Produce Auto-Arima Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot3", height = 400),
                            verbatimTextOutput("detail3")
                        )
                    )
            ),
            # Second Tab Content
            tabItem(tabName = "dashboard2",
                    fluidRow(
                       box(title = "Forecast Rice Price Holt-Winter Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot4", height = 200),
                           verbatimTextOutput("detail4")
                           ),
                       box(title = "Forecast Rice Price MLP Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot5", height = 200),
                           verbatimTextOutput("detail5")
                       ),
                       box(title = "Forecast Rice Price Auto-Arima Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot6", height = 200),
                           verbatimTextOutput("detail6")
                       )
                    )
            ),
            # Third tab content
            tabItem(tabName = "controller",
                    fluidRow(
                        box(title = "Choose Regional Zone of Rice", status = "danger", solidHeader = TRUE, collapsible = TRUE,
                            selectInput("reg",
                                        label = "Choose Regional",
                                        choices = c("West Java",
                                                    "Central Java",
                                                    "East Java",
                                                    "South Sulawesi"),
                                        selected = "East Java")
                        )
                        
                    )
            )
        )
    ),
    dashboardPage(
        header,
        sidebar,
        body
    )
)

server <- function(input, output) {
    #Rice Produce
    supplyBeras = read.csv("D:/supply_beras.csv",sep = ",")
    cb <- supplyBeras$Karawang
    supply <- ts(cb, start = c(2015,5), frequency = 12)
    plot(supply)
    library(forecast)
    library(nnfor)
    #HW Produce
    hw <- HoltWinters(supply)
    plot(hw)
    fhwp <- forecast(hw, h=30)
    detail <- accuracy(fhwp)
    #MLP Produce
    ml <- mlp(supply)
    plot(ml)
    fmlpp = forecast(ml, h=30)
    detail2 <- accuracy(fmlpp)
    #ARIMA Produce
    ar <- auto.arima(supply)
    farp <- forecast(ar, h = 30)
    plot(farp)
    detail3 <- accuracy(farp)
    
    #Rice Price
    hargaBeras = read.csv("D:/harga_beras.csv", sep = ";")
    cb2 <- hargaBeras$Jatim
    supply2 <- ts(cb2, frequency = 12, start = c(2017,1))
    plot(supply2)
    #HW Produce
    hw2 <- HoltWinters(supply2)
    plot(hw2)
    fhwp2 <- forecast(hw2, h=30)
    plot(fhwp2)
    detail4 <- accuracy(fhwp2)
    #MLP Produce
    ml2 <- mlp(supply2)
    plot(ml2)
    fmlpp2 <- forecast(ml2, h=30)
    plot(fmlpp2)
    detail5 <- accuracy(fmlpp2)
    #ARIMA Produce
    ar2 <- auto.arima(supply2)
    farp2 <- forecast(ar2, h = 30)
    plot(farp2)
    detail6 <- accuracy(farp2)
    output$detail <- renderText({ detail[,"MAPE"] })
    output$detail2 <- renderText({ detail2[,"MAPE"]})
    output$detail3 <- renderText({ detail3[,"MAPE"]})
    output$detail4 <- renderText({ detail4[,"MAPE"] })
    output$detail5 <- renderText({ detail5[,"MAPE"]})
    output$detail6 <- renderText({ detail6[,"MAPE"]})
    output$plot1 <- renderPlot({
        plot(fhwp)
    })
    output$plot2 <- renderPlot({
        plot(fmlpp)
    })
    output$plot3 <- renderPlot({
        plot(farp)
    })
    output$plot4 <- renderPlot({
        plot(fhwp2)
    })
    output$plot5 <- renderPlot({
        plot(fmlpp2)
    })
    output$plot6 <- renderPlot({
        plot(farp2)
    })
}

shinyApp(ui, server)