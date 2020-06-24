library(shiny)
library(tidyverse)
library(here)
library(shinyjs)

theme_set(theme_bw())

# load data
# load(here::here("risk-premia-app", "data", "assetclass_prices.RData"))  # to run locally
load("data/assetclass_prices.RData")  # for deployment

# source(here::here("risk-premia-app", "R", "utils.R")) # to run locally
source("R/utils.R")  # for deployment

# TODO:
    # load from BQ: do we want the unwashed masses hitting our cloud?
    # maybe load data from BQ on the first startup of the day so it's always up to date without always hitting BQ
    # needs application service token
    # Apply colour scheme such that colours of already selected assets don't change - 
    # rolling correlation: implement with roll not slider
    # spinners on plot load
    # tour
    # rolling corrplot - should use different colours as they don't represent the things above....
    # numbers over tiles on corrplot
    



# calculate total returns
rp_tickers <- c("VTI", "TLT", 'GLD')

prices <- prices %>% 
    filter(ticker %in% rp_tickers) %>% 
    add_total_returns_col()

ui <- fluidPage(
    shinyjs::useShinyjs(),
    
    titlePanel("Risk Premia Harvesting"),
    
    sidebarLayout(
        sidebarPanel(
            selectizeInput("assets", "Select Assets", choices = c("VTI", "TLT", "GLD"), selected = c("VTI", "TLT", "GLD"), multiple = TRUE),
            sliderInput("estWdwSize", "Select Estimation Window Length", min = 10, max = 100, step = 10, value = 30),
            sliderInput("fwdWdwSize", "Select Forward Window Length", min = 10, max = 100, step = 10, value = 30),
            checkboxInput("removeOverlapping", label = "Show Non-Overlapping Periods Only", value = TRUE)
        ),
        
        mainPanel(
            tabsetPanel(
                id = "tab",
                tabPanel(
                    "Performance",
                    value = "perfTab",
                    plotOutput("cumReturnsPlot"),
                    plotOutput("rollingPerfPlot"),
                    plotOutput("rollCorrPlot"),
                    plotOutput("corrMatPlot")
                ),
            
                tabPanel(
                    "Scatterplots",
                    value = "autcorrTab",
                    fluidRow(
                        column(6, plotOutput("laggedReturnsPlot")),
                        column(6, plotOutput("laggedVolPlot"))
                    )
                )
            )
        )
    )
    
)

server <- function(input, output) {
    
    observe({
        if(input$"tab" == "perfTab") {
            shinyjs::disable(id = "estWdwSize")
            shinyjs::disable(id = "fwdWdwSize")
            shinyjs::disable(id = "removeOverlapping")
        } else {
            shinyjs::enable(id = "estWdwSize")
            shinyjs::enable(id = "fwdWdwSize")
            shinyjs::enable(id = "removeOverlapping")
        }
    })
    
    df <- reactive({
        prices %>% 
            filter(ticker %in% input$assets)
    })

    output$cumReturnsPlot <- renderPlot({
        df() %>% 
            total_returns_plot()
    })
    
    output$rollingPerfPlot <- renderPlot({
        df() %>% 
            rolling_ann_perf() %>% 
            rolling_ann_perf_plot()
    })
    
    output$rollCorrPlot <- renderPlot({
        df() %>% 
            roll_pairwise_corrs() %>% 
            roll_pairwise_corrs_plot(facet = FALSE)
    })
    
    output$corrMatPlot <- renderPlot({
        df() %>% 
            cormat() %>% 
            cormat_plot()
    })
    
    output$laggedReturnsPlot <- renderPlot({
        df() %>% 
            lagged_returns_scatterplot(estimation_wdw = input$estWdwSize, forward_wdw = input$fwdWdwSize, remove_overlapping = input$removeOverlapping)
    })
    
    output$laggedVolPlot <- renderPlot({
        df() %>% 
            lagged_vol_scatterplot(estimation_wdw = input$estWdwSize, forward_wdw = input$fwdWdwSize, remove_overlapping = input$removeOverlapping)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
