library(shiny)
library(shinydashboard)
library(quantmod)
library(highcharter)

ui <- dashboardPage(
  dashboardHeader(title = "Awesome Stock Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stock Data", tabName = "stock_data")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "stock_data",
        h2("Select a Stock"),
        selectInput("stock", "Stock Symbol:",
                    choices = c("AAPL", "MSFT", "GOOGL", "GME", "QCOM"),
                    selected = "AAPL"),
        dateRangeInput("date_range", "Date Range:",
                       start = "2023-01-01",
                       end = Sys.Date()),
        actionButton("toggle_scale", "Mudar escala (log)"),
        highchartOutput("stock_plot"),
        verbatimTextOutput("summary"),
        plotOutput("histogram"),
        plotOutput("boxplot")
      )
    )
  )
)

# logica serv
server <- function(input, output, session) {
  toggle_state <- reactiveVal(FALSE)
  selection <- reactiveVal(NULL)
  observeEvent(input$toggle_scale, {
    toggle_state(!toggle_state())
  })
  observeEvent(input$select_range, {
    selection(input$stock_plot_xaxis_categories)
  })
  output$stock_plot <- renderHighchart({
    stock_data <- getSymbols(input$stock, from = input$date_range[1], to = input$date_range[2], auto.assign = FALSE)
    chart <- highchart() %>%
      hc_title(text = paste("Preço do ticket: ", input$stock)) %>%
      hc_add_series(type = "line", name = "Close Price", data = Cl(stock_data)) %>%
      hc_navigator(enabled = TRUE) %>%
      hc_tooltip(valueDecimals = 2) 
    
    if (toggle_state()) {
      chart <- chart %>%
        hc_yAxis(type = "logarithmic")
    }
    chart %>%
      hc_chart(events = list(selection = JS("function(event) { Shiny.onInputChange('selected_points', event.xAxis[0].categories); }")))
  })
  observeEvent(input$selected_points, {
    selection(input$selected_points)
  })
  output$summary <- renderPrint({
    selected_data <- getSymbols(input$stock, from = input$date_range[1], to = input$date_range[2], auto.assign = FALSE)
    print(paste('Data selecionada:',paste(paste(input$date_range[1], 'até'), input$date_range[2])))
    stats <- data.frame(
      "Número de observações" = nrow(selected_data),
      "Média" = mean(Cl(selected_data), na.rm = TRUE),
      "Moda" = as.numeric(names(sort(-table(Cl(selected_data))))[1]),
      "Mediana" = median(Cl(selected_data), na.rm = TRUE),
      "Desvio padrão (volatidade)" = sd(Cl(selected_data), na.rm = TRUE),
      "Min" = min(Cl(selected_data), na.rm = TRUE),
      "Max" = max(Cl(selected_data), na.rm = TRUE)
    )

    names(stats) <- gsub("\\.", " ", names(stats))
    return(knitr::kable(stats, align = "c"))
  
  })
  output$boxplot <- renderPlot({
    stock_data <- getSymbols(input$stock, from = input$date_range[1], to = input$date_range[2], auto.assign = FALSE)
    close_prices <- Cl(stock_data)
    dates <- as.Date(index(stock_data))
    weekly_intervals <- cut(dates, "week")
    boxplot(split(close_prices, weekly_intervals), 
            main = "Boxplot semanal do preço de fechamento", ylab = "Close Price")
  })
  output$histogram <- renderPlot({
    stock_data <- getSymbols(input$stock, from = input$date_range[1], to = input$date_range[2], auto.assign = FALSE)
    hist(Cl(stock_data), main = "Histograma da frequência do preço de fechamento", xlab = "Close Price", col = "lightblue")
  })
  
}
shinyApp(ui = ui, server = server)
