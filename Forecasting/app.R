## app.R ##
library(shinydashboard)

ui <- dashboardPage(
    header <- dashboardHeader(title = "O.S Forecasting"),
    
    ## Sidebar content
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Rice Production", tabName = "dashboard1", icon = icon("dashboard")),
            menuItem("Rice Price", tabName = "dashboard2", icon = icon("dashboard")),
            menuItem("Controller", tabName = "controller", icon = icon("th"))
        )
    ),
    ## Body content
    body <- dashboardBody(
        tabItems(
            # First Tab Content
            tabItem(tabName = "dashboard1",
                    fluidRow(
                        box(title = "Forecast Rice Produce Holt-Winter Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot1", height = 200),
                            verbatimTextOutput("detail")
                            ),
                        box(title = "Forecast Rice Produce MLP Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot2", height = 200),
                            verbatimTextOutput("detail2")
                            ),
                        box(title = "Forecast Rice Produce Auto-Arima Method", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot3", height = 200),
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
        )
    ),
    dashboardPage(
        header,
        sidebar,
        body
    )
    mainPanel({
        box(title = "Forecast")
    })
)

server <- function(input, output) {
    #Rice Produce
    hangga = read.csv("D:/supply_beras.csv",sep = ",")
    cb <- hangga$Karawang
    supply <- ts(cb, start (2012,1), frequency = 12)
    plot(supply)
    library(forecast)
    library(nnfor)
    #HW Produce
    hw <- HoltWinters(supply)
    plot(hw)
    fhwp <- forecast(hw, h=90)
    detail <- accuracy(fhwp)
    #MLP Produce
    ml <- mlp(supply)
    plot(ml)
    fmlpp = forecast(ml, h=90)
    detail2 <- accuracy(fmlpp)
    #ARIMA Produce
    ar <- auto.arima(supply)
    farp <- forecast(ar, h = 140)
    plot(farp)
    detail3 <- accuracy(farp)
    
    #Rice Price
    hangga2 = read.csv("D:/harga_beras.csv", sep = ";")
    cb2 <- hangga2$Jatim
    supply2 <- ts(cb2, start (2017,1), frequency = 12)
    plot(supply2)
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