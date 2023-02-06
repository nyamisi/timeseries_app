require(tidyverse)
require(shiny)
require(shinythemes)
require(shinyWidgets)
require(DT)
require(lubridate)
require(patchwork)
require(plotly)


data = read_csv("timeseries.csv") %>%
    mutate(season = month(date)%>%as.integer()) %>%
    mutate(season = replace(season, season %in% c(1:4, 11:12), "NE")) %>%
    mutate(season = replace(season, season %in% c(5:10), "SE"))

tz.data = data %>% 
    pivot_wider(names_from = variable, values_from = data) 



fig1 = tz.data %>%
    filter(site == "Mafia Channel" & chlorophyll < 0.5) %>%
    ggplot(aes(x = date, y = chlorophyll)) +
    geom_line() +
    theme(panel.background = element_rect(fill = NA, colour = "black"),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title = element_text(size = 11, colour = "black")) +
    labs(x = "", y = expression(Chlorophyll-a~(mgm^{-3})))


fig2 = tz.data %>% 
    filter(site == "Mafia Channel" & chlorophyll < 0.5) %>%
    ggplot(aes(x = as.factor(month(date, label = TRUE, abbr = TRUE)), y = chlorophyll)) +
    geom_boxplot(fill = "green") +
    theme(panel.background = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title = element_text(size = 11, colour = "black")) +
    labs(x = "", y = expression(Chlorophyll-a~(mgm^{-3})))

fig1 / fig2 + plot_layout(ncol = 1, widths = 4, heights = 5)

site = tz.data %>%distinct(site) %>% pull

variable = data %>%
    filter(variable != "tmin") %>% distinct(variable) %>% pull

# 1st define ui
ui = fluidPage(
    navbarPage(title = "Oceanographic variables", theme = shinytheme("united"),
               tabPanel(title = "Chlorophyll",
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectInput(inputId = "tz_site", label = "Choose site", choices = site, selected = "Zanzibar Channel")
                                         ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel(title = "plot",
                                             plotOutput(outputId = "timeseries_plot", width = "100%", height = 600)),
                                    tabPanel(title = "data",
                                             DTOutput(outputId = "table"))
                                )
                            )
                        )
                        ),
               tabPanel(title = "SST",
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectInput(inputId = "tz_sst", label = "Choose site", choices = site, selected = "Zanzibar Channel")
                                         ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel(title = "plot",
                                             plotOutput(outputId = "sst_plot", width = "100%", height = 600)),
                                    tabPanel(title = "data",
                                             DTOutput(outputId = "ssttable"))
                                )
                            )
                        )
                        ),
               tabPanel(title = "Rainfall",
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectInput(inputId = "tz_rain", label = "Choose site", choices = site, selected = "Zanzibar Channel")
                                
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel(title = "plot",
                                             plotOutput(outputId = "rain_plot", width = "100%", height = 600)),
                                    tabPanel(title = "data",
                                             DTOutput(outputId = "raintable"))
                                )
                            )
                        )
                   
               ),
               tabPanel(title = "Tmax",
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectInput(inputId = "tz_tmax", label = "Choose site", choices = site, selected = "Zanzibar Channel")
                                
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel(title = "plot", 
                                             plotOutput(outputId = "tmax_plot", width = "100%", height = 600)),
                                    tabPanel(title = "data",
                                             DTOutput(outputId = "tmaxtable"))
                                )
                            )
                        )
                   
               ),
               tabPanel(title = "Tmin",
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectInput(inputId = "tz_tmin", label = "Choose site", choices = site, selected = "Zanzibar Channel")
                                
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel(title = "plot",
                                             plotOutput(outputId = "tmin_plot", width = "100%", height = 600)),
                                    tabPanel(title = "data",
                                             DTOutput(outputId = "tmintable"))
                                )
                            )
                        )
                   
               ),
               tabPanel(title = "Seasonal",
                        sidebarLayout(
                            sidebarPanel(width = 2
                                
                            ),
                            mainPanel(
                                fluidRow(column(width = 6, plotlyOutput(outputId = "season_chl")),
                                         column(width = 6, plotlyOutput(outputId = "season_sst")),
                                         column(width = 6, plotlyOutput(outputId = "season_rain")),
                                         column(width = 6, plotlyOutput(outputId = "season_tmax"))
                                    
                                )
                            )
                        )
                   
               ),
               tabPanel(title = "About Oceanography",
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectInput(inputId = "tz_tmin", label = "Choose site", choices = site, selected = "Zanzibar Channel")
                                         ),
                            mainPanel(
                                fluidRow(
                                    column(width = 11,
                                           tags$div(
                                               tags$h4("Abstract"),
                                               "The change of chlorophyll-a (chl-a) concentration was analysed along the territorial waters in Tanzania at Pemba Channel, Zanzibar Channel, Mafia Channel, Lindi and Mtwara from 1997 to 2019. Chlorophyll-a and Sea surface temperature (SST) data were obtained from MODerate resolution Imaging Spectrometer on board the platform Aqua (MODIS-Aqua). The chlorophyll-a data before July 2002 were obtained from Sea-viewing Wide Field-of-view Sensor (SeaWIFS). Rainfall, maximum and minimum air temperature data were obtained from Tanzania Meteorological Agency (TMA). There was a significant decline in annual chlorophyll-a concentration between 1997 and 2019 at Pemba Channel (tau = -0.26, z = -6.44, p = 1.16 x 10-10), Mafia Channel (tau = -0.25, z = -6.16, p = 7.41 x 10-10), Lindi (tau = -0.41, z = -9.90, p = 2.2 x 10-16) and Mtwara (tau = -0.15, z = -3.64, p = 0.0003). However, at Zanzibar Channel, there was a significant annual increase in chl-a concentration (tau = 0.12, z = 3.01, p = 0.003). At Pemba Channel, Zanzibar Channel, Mafia Channel, Lindi and Mtwara there were significant increase in SST and maximum and minimum air temperature between 1997 and 2019. The decline in chl-a concentration is linked with increase in temperature during the study period. The southeast monsoon had significant higher chlorophyll-a concentration than northeast monsoon season at both Pemba Channel (W = 3594, p < 0.05), Zanzibar Channel (W = 3119, p < 0.05), Mafia Channel (W = 4912, p < 0.05), Lindi (W = 4089, p < 0.05) and Mtwara site (W = 7100, p < 0.05). This seasonal variation in chl-a is associated with oscillations of monsoon winds. The peak chl-a concentrations were recorded on July at Pemba Channel and Lindi site, September at Zanzibar Channel, May at Mafia Channel and on April at Mtwara site. Lower chl-a was recorded on February at Pemba and Mafia Channel and on December at Zanzibar Channel, Lindi and Mtwara sites. Higher chl-a concentration coincide well with low SST and low air temperature months and lower chl-a on months with high SST and air temperature.",
                                               tags$br(),
                                               
                                               tags$h4("Background"),
                                               "Changes of physical and chemical properties of the ocean affect the temporal and spatial abundance of phytoplankton biomass (Chl-a). Previous studies show that there is a decline in phytoplankton biomass in the world ocean. These studies indicated a rapid decline in open oceans than shelf areas. The average rate of decline in phytoplankton biomass was projected up to 2 % per year by 2100 in many areas of the world ocean. In Western Indian Ocean (WIO) region, studies show the decline in phytoplankton biomass at the rate of 1.2 % per year around the Rufiji-Mafia Channel.  These studies show the increase in sea surface temperature, intensifying vertical stratification and reduced vertical mixing as the major cause for that decline. As a consequence of ocean warming, there is the increase in phytoplankton abundance in cooler regions and decrease phytoplankton abundance in warmer ones. Thus, long-term studies of the ocean is very important in order to determine changes of phytoplankton biomass and ocean temperature as a result of global warming. However, there is a limited information on long-term studies on phytoplankton biomass along Tanzanian waters.",
                                               tags$br(),
                                               
                                               tags$h4("Objectives"),
                                               tags$h5("Specific Objectives"),
                                               "1. To determine inter-annual, seasonal and monthly changes of phytoplankton biomass from 1997 to 2019",tags$br(),
                                               "2. To determine factors governing those changes",
                                               tags$br(),
                                               
                                               tags$h4("Summary"),
                                               "Chlorophyll-a exhibit both monthly, seasonal and inter-annual differences at both Pemba Channel, Zanzibar Channel, Mafia Channel, Lindi and Mtwara site. The monthly difference in chl-a at these sites might be attributed by the seasonal cycle of environmental variables. Higher chl-a concentration on July at Pemba Channel and Lindi site and on September at Zanzibar Channel coincide with lower SST months at these sites. This result is similar to other studies in Tanzania which showed that, phytoplankton biomass (chlorophyll-a) is very high during the colder months of the year. Sea surface temperature play an important role on the growth and reproduction of phytoplankton. Mixing processes in the ocean are generally influenced by temperature conditions of the sea surface water. When surface waters are cold, it is easier for deeper water to rise to the surface, bringing nutrients to sunlit areas where phytoplankton can use them. Low amount of sea surface temperature is the result of low amount of air temperature during the colder months.",
                                               tags$br(),
                                               "Unlike Pemba Channel, Zanzibar Channel and Lindi site, Mafia Channel and Mtwara site had higher chl-a on May and April consecutively. This might be due to heavy rain which peak on April at Mafia Channel and on March at Mtwara site. Because of higher influence of Rufiji River to the channel and Ruvuma River at Mtwara, during rain period the land runoff from upstream areas are washed to these areas resulting in nutrient enrichment as a result higher phytoplankton and chl-a.",
                                               tags$br(),
                                               "Apart from monthly variation, the seasonal difference showed significant higher chl-a concentration during the southeast monsoon season than northeast monsoon at all study sites. This is similar to other studies in Tanzania which found that, chl-a was higher in SE than NE monsoon season. The southeast monsoon season is mainly associated with heavy rain, low SST and strong winds. These allow nutrient supply to euphotic zone through runoff input, vertical mixing of the water column and turbulent mixing.",
                                               tags$br(),
                                               "Looking on the inter-annual trend at Pemba Channel, Mafia Channel, Lindi and Mtwara site there were a significant decline in chl-a concentration between 1997 and 2019. This can be linked with an increase in SST resulted from increase in air temperature during the study period.",
                                               tags$br(),
                                               "On the other hand, the decline in chlorophyll-a concentration might be associated with the decline in amount of rainfall at Pemba Channel, Mafia Channel and Lindi site. We observed that, the amount of rainfall was declining at both Pemba Channel, Mafia Channel and Lindi during the study period. Rainfall play a major role in phytoplankton production and chlorophyll-a concentration through nutrient supply to the ocean. Riverine input resulting from rainfall (precipitation) cause increase in nutrients leading to higher chlorophyll-a concentration. Thus, decrease in amount of rainfall will reduce riverine input and land runoff to the ocean which will result in low nutrient and in long-run, low chlorophyll-a concentration.",
                                               tags$br(),
                                               "Contrary, the Zanzibar Channel showed a significant increase in annual chl-a concentration between 1997 and 2019. However, the reason for this is not well known as environmental variables at this channel shows similar characteristics as other study sites. While inter-annual trend of SST, maximum and minimum air temperature showed significant increase, the amount of rainfall was significant decreasing at Zanzibar Channel."
                                           )
                                        
                                    )
                                )
                            )
                        )
                        )
               )
)


server = function(input, output){
    
    output$timeseries_plot = renderPlot({
        
        fig1 = tz.data %>%
            filter(site == input$tz_site & chlorophyll < 0.5) %>%
            ggplot(aes(x = date, y = chlorophyll)) +
            geom_line(color = "green") +
            theme(panel.background = element_rect(fill = NA, colour = "black"),
                  axis.text = element_text(size = 11, colour = "black"),
                  axis.title = element_text(size = 11, colour = "black")) +
            labs(x = "", y = expression(Chlorophyll-a~(mgm^{-3})))
        
        
        fig2 = tz.data %>% 
            filter(site == input$tz_site & chlorophyll < 0.5) %>%
            ggplot(aes(x = as.factor(month(date, label = TRUE, abbr = TRUE)), y = chlorophyll)) +
            geom_boxplot(fill = "green") +
            theme(panel.background = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(size = 11, colour = "black"),
                  axis.title = element_text(size = 11, colour = "black")) +
            labs(x = "", y = expression(Chlorophyll-a~(mgm^{-3})))
        
        fig1 / fig2 + plot_layout(ncol = 1, widths = 4, heights = 5)
        
        
    })
    output$table = renderDataTable({
        tz.data %>% 
            filter(site == input$tz_site & chlorophyll < 0.5) %>%
            select(date, site, chlorophyll) %>%
            mutate_if(is.numeric, round, digits = 2)
        
    })
    output$sst_plot = renderPlot({
        
        fig1 = tz.data %>%
            filter(site == input$tz_sst) %>%
            ggplot(aes(x = date, y = sst)) +
            geom_line(color = "blue") +
            theme(panel.background = element_rect(fill = NA, colour = "black"),
                  axis.text = element_text(size = 11, colour = "black"),
                  axis.title = element_text(size = 11, colour = "black")) +
            labs(x = "", y = "SST (oC)")
        
        
        fig2 = tz.data %>% 
            filter(site == input$tz_sst) %>%
            ggplot(aes(x = as.factor(month(date, label = TRUE, abbr = TRUE)), y = sst)) +
            geom_boxplot(fill = "blue") +
            theme(panel.background = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(size = 11, colour = "black"),
                  axis.title = element_text(size = 11, colour = "black")) +
            labs(x = "", y = "SST (oC)")
        
        fig1 / fig2 + plot_layout(ncol = 1, widths = 4, heights = 5)
    })
    output$ssttable = renderDataTable({
        tz.data %>% 
            filter(site == input$tz_sst) %>%
            select(date, site, sst)%>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$rain_plot = renderPlot({
        
        fig1 = tz.data %>%
            filter(site == input$tz_rain) %>%
            ggplot(aes(x = date, y = rainfall)) +
            geom_line(color = "purple") +
            theme(panel.background = element_rect(fill = NA, colour = "black"),
                  axis.text = element_text(size = 11, colour = "black"),
                  axis.title = element_text(size = 11, colour = "black")) +
            labs(x = "", y = "Rainfall (mm)")
        
        
        fig2 = tz.data %>% 
            filter(site == input$tz_rain) %>%
            ggplot(aes(x = as.factor(month(date, label = TRUE, abbr = TRUE)), y = rainfall)) +
            geom_boxplot(fill = "purple") +
            theme(panel.background = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(size = 11, colour = "black"),
                  axis.title = element_text(size = 11, colour = "black")) +
            labs(x = "", y = "Rainfall (mm)")
        
        fig1 / fig2 + plot_layout(ncol = 1, widths = 4, heights = 5)
    })
    
    output$raintable = renderDataTable({
        tz.data %>% 
            filter(site == input$tz_rain) %>%
            select(date, site, rainfall)
    })
    
    output$tmax_plot = renderPlot({
        
        fig1 = tz.data %>%
            filter(site == input$tz_tmax & tmax > 28) %>%
            ggplot(aes(x = date, y = tmax)) +
            geom_line(color = "red") +
            theme(panel.background = element_rect(fill = NA, colour = "black"),
                  axis.text = element_text(size = 11, colour = "black"),
                  axis.title = element_text(size = 11, colour = "black")) +
            labs(x = "", y = "Maximum air temp (oC)")
        
        
        fig2 = tz.data %>% 
            filter(site == input$tz_tmax & tmax > 28) %>%
            ggplot(aes(x = as.factor(month(date, label = TRUE, abbr = TRUE)), y = tmax)) +
            geom_boxplot(fill = "red") +
            theme(panel.background = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(size = 11, colour = "black"),
                  axis.title = element_text(size = 11, colour = "black")) +
            labs(x = "", y = "Maximum air temp (oC)")
        
        fig1 / fig2 + plot_layout(ncol = 1, widths = 4, heights = 5)  
        
    })
    
    output$tmaxtable = renderDataTable({
        tz.data %>%
            filter(site == input$tz_tmax & tmax > 28) %>%
            select(date, site, tmax)
        
    })
    
    output$tmin_plot = renderPlot({
        
        fig1 = tz.data %>%
            filter(site == input$tz_tmin) %>%
            ggplot(aes(x = date, y = tmin)) +
            geom_line(color = "pink") +
            theme(panel.background = element_rect(fill = NA, colour = "black"),
                  axis.text = element_text(size = 11, colour = "black"),
                  axis.title = element_text(size = 11, colour = "black")) +
            labs(x = "", y = "Minimum air temp (oC)")
        
        
        fig2 = tz.data %>% 
            filter(site == input$tz_tmin) %>%
            ggplot(aes(x = as.factor(month(date, label = TRUE, abbr = TRUE)), y = tmin)) +
            geom_boxplot(fill = "pink") +
            theme(panel.background = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(size = 11, colour = "black"),
                  axis.title = element_text(size = 11, colour = "black")) +
            labs(x = "", y = "Minimum air temp (oC)")
        
        fig1 / fig2 + plot_layout(ncol = 1, widths = 4, heights = 5)   
        
    })
    
    output$tmintable = renderDataTable({
        tz.data %>%
            filter(site == input$tz_tmin) %>%
            select(date, site, tmin)
    })
    
    output$season_chl = renderPlotly({
        data %>%
            filter(variable == "chlorophyll" & data < 0.5) %>%
            plot_ly(x = ~site, y = ~data, split = ~season) %>%
            add_boxplot() %>%
            layout(title = "Chlorophyll", boxmode = "group")
    })
    
    output$season_sst = renderPlotly({
        data %>%
            filter(variable == "sst") %>%
            plot_ly(x = ~site, y = ~data, split = ~season) %>%
            add_boxplot() %>%
            layout(title = "SST", boxmode = "group")
    })
    

    output$season_rain = renderPlotly({
        data %>% 
            filter(variable == "rainfall" & data < 500) %>%
            plot_ly(x = ~site, y = ~data, split = ~season) %>%
            add_boxplot() %>%
            layout(title = "Rainfall", boxmode = "group")
    })
    
    output$season_tmax = renderPlotly({
        data %>%
            filter(variable == "tmax") %>%
            plot_ly(x = ~site, y = ~data, split = ~season) %>%
            add_boxplot() %>%
            layout(title = "Tmax", boxmode = "group")
    })
    
    
}


shinyApp(ui = ui, server = server)
    