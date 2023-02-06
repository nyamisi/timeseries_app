require(tidyverse)
require(shiny)
require(shinythemes)
require(shinyWidgets)
require(DT)
require(lubridate)
require(plotly)



data = read_csv("timeseries.csv") %>% 
    mutate(month = month(date, label = TRUE))


data %>%
    filter(variable == "tmax")%>% 
    plot_ly(x = ~month, y = ~data, split = ~site) %>%
    add_boxplot()

fig = data %>%
    filter(variable == "tmax")%>% 
    ggplot(aes(x = month, y = data, fill = site)) +
    geom_boxplot()

fig %>% ggplotly()

data %>%
    filter(variable == "tmax")%>% 
    plot_ly(x = ~date, y = ~data, split = ~site) %>%
    add_lines()

site = data %>% distinct(site) %>% pull()

ui = fluidPage(
    navbarPage(title = "Climatology", theme = shinytheme("flatly"),
               tabPanel(title = "Mie",
               sidebarLayout(
                   sidebarPanel(width = 2,
                       selectInput(inputId = "i_site", label = "Choose a Site", choices = site, selected = "Lindi")
                   ),
                   mainPanel(
                       fluidRow(
                           column(width = 6, plotlyOutput(outputId = "i_sst")),
                           column(width = 6, plotlyOutput(outputId = "i_tmax")),
                           column(width = 6, plotlyOutput(outputId = "i_chloro")),
                           column(width = 6, plotlyOutput(outputId = "i_rain"))
                       )
                   )
                )
               )
               
               )
    
)

server = function(input,output){
    
    output$i_sst = renderPlotly({
        data %>%
            filter(variable == "sst" & site == input$i_site)%>% 
            plot_ly(x = ~month, y = ~data, split = ~site) %>%
            add_boxplot()
    })
    
    output$i_tmax = renderPlotly({
        data %>%
            filter(variable == "tmax" & site == input$i_site & data > 28)%>% 
            plot_ly(x = ~month, y = ~data, split = ~site) %>%
            add_boxplot()
    })
    
    output$i_chloro = renderPlotly({
        data %>%
            filter(variable == "chlorophyll" & site == input$i_site & data < 0.5)%>% 
            plot_ly(x = ~month, y = ~data, split = ~site) %>%
            add_boxplot()
    })
    
    output$i_rain = renderPlotly({
        data %>%
            filter(variable == "rainfall" & site == input$i_site & data < 400)%>% 
            plot_ly(x = ~month, y = ~data, split = ~site) %>%
            add_boxplot() %>%
            layout(title = "Rainfall")
    })
    
}


shinyApp(ui = ui, server = server)
