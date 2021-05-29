## app.R ##
library(shinydashboard)

ui <- dashboardPage(
    header <- dashboardHeader(title = "O.S Forecasting"),
    
    ## Sidebar content
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Rice Production", tabName = "dashboard1", icon = icon("dashboard")),
            menuItem("Rice Price", tabName = "dashboard2", icon = icon("dashboard"))
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
    
    output$detail <- renderText({ detail[,"MAPE"] })
    output$detail2 <- renderText({ detail2[,"MAPE"]})
    output$plot1 <- renderPlot({
        plot(fhwp)
    })
    output$plot2 <- renderPlot({
        plot(fmlpp)
    })
}

shinyApp(ui, server)