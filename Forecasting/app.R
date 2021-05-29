## app.R ##
library(shinydashboard)

ui <- dashboardPage(
    header <- dashboardHeader(title = "Forecasting"),
    
    ## Sidebar content
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
    ),
    ## Body content
    body <- dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(title = "Forecast Rice Price", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot1", height = 215),
                            verbatimTextOutput("detail")
                            ),
                        box(title = "Forecast Rice Produce", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot2", height = 215),
                            verbatimTextOutput("detail2")
                            ),
                        box(
                            title = "Controls", status = "danger", solidHeader = TRUE, collapsible = TRUE, 
                            sliderInput("slider", "Number of observations:", 1, 100, 50),
                            textInput("text", "Text Input:")
                        ),
                        box(
                            title = "Blabla", status = "warning", solidHeader = TRUE, collapsible = TRUE
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "widgets",
                    h2("Widgets tab content")
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
    hangga = read.csv("D:/supply_beras.csv",sep = ",")
    head(hangga)
    cb <- hangga$Karawang
    supply <- ts(cb, start (2012,1), frequency = 12)
    plot(supply)
    library(forecast)
    library(nnfor)
    #HW
    hw <- HoltWinters(supply)
    plot(hw)
    f1 <- forecast(hw, h=90)
    detail2 <- accuracy(f1)
    #MLP
    ml <- mlp(supply)
    plot(ml)
    f2 = forecast(ml, h=90)
    detail <- accuracy(f2)
    output$detail <- renderText({ detail[,"MAPE"] })
    output$detail2 <- renderText({ detail2[,"MAPE"]})
    output$plot1 <- renderPlot({
        plot(f1)
    })
    output$plot2 <- renderPlot({
        plot(f2)
    })
}

shinyApp(ui, server)