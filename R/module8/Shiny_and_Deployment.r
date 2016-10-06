
library(dplyr)
library(tidyr)
library(magrittr)

library(shiny)
library(ggplot2)

## make data
df0 <- readRDS("small_data/inpatient_charges_2014_clean_1000.RDS")
names(df0) %<>% gsub("\\.", "", .)

df <- df0 %>% select(AverageCoveredCharges, AverageTotalPayments,
                     AverageMedicarePayments, TotalDischarges)
variables <- names(df)[1:3]
colors <- names(df)[4]

# the shiny code
server <- function(input, output){
    
    output$my_output_plot <- renderPlot({

        p <- ggplot(df, aes_string(input$x, input$y, colour=input$color)) +
            geom_point(alpha=0.3)
        ##p
        print(p)
    })
}

ui <- fluidPage(

    headerPanel("My Data Explorer"),

    sidebarPanel(
        selectInput(inputId="y",
                    label="Y Variable",
                    choices=variables
                    ),
        selectInput(inputId="x",
                    label="X Variable",
                    choices=variables

                    ),
        selectInput(inputId="color",
                    label="Color by Variable",
                    choices=colors
                    )
    ),

    mainPanel(
        plotOutput("my_output_plot")
    )
    
)

# deploy
# shinyApp(ui=ui, server=server)

## add a subset to the above scatterplot - filter by code
df0$DRGcode %<>% as.character

(codes <- df0$DRGcode %>% unique %>% sort)

server <- function(input, output){
    
    dataset <- reactive({
        df0
        ## if (input$code != 'None')
        ##     df_filter <- df %>% filter(DRG.code==input$code)
        ## df_filter
    })

    output$my_output_plot <- renderPlot({

        p <- ggplot(dataset, aes_string(input$x, input$y, colour=input$color)) +
            geom_point(alpha=0.3)
        ##p
        print(p)

    })
}

ui <- fluidPage(

    headerPanel("My Data Explorer"),

    sidebarPanel(
        selectInput(inputId="y", label="Y Variable", choices=variables),
        selectInput(inputId="x", label="X Variable", choices=variables),
        selectInput(inputId="color", label="Color by Variable", choices=colors),
        selectInput(inputId="code", label="Select DRG code", c('None', codes))
    ),

    mainPanel( plotOutput("my_output_plot") )
)

# shinyApp(ui=ui, server=server)
