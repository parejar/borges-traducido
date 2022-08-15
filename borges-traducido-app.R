library(shiny)
library(ggplot2)
library(shinyjs)
library(dplyr)
library(DT)

# Define UI for ggplot visualization app ----
ui <- fluidPage(
    br(),
    useShinyjs(),
    # App title ----
    titlePanel("Borges Traducido / Borges Translated"),
    h5("Con datos del Index Translationum de la UNESCO / With data from UNESCO's Index Translationum"),
    br(),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # br() element to introduce extra vertical spacing ----
            br(),


            sliderInput("range", "Rango de años/Year range:",
                        min(borges_language_year$año), max(borges_language_year$año),
                        c(1970, 2009)),
            selectInput("lengua", "Lengua/Language:", 
                        c(Todas=" ", borges_language_year$lengua))
        ),
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Gráfico/Plot", plotOutput("plot")),
                        tabPanel("Resumen/Summary", verbatimTextOutput("summary")),
                        tabPanel("Datos/Data", DTOutput("table"))
            )
            
        )
    )
)

# Define server logic ----
server <- function(input, output, session) {
    data <- reactive({
        if(input$lengua == " ") {
            return (borges_language_year %>% subset(año >= input$range[1] & año <= input$range[2]))
            } else {
                return (borges_language_year %>% subset(año >= input$range[1] & año <= input$range[2]) %>% 
                            filter(lengua == input$lengua))
            }
    })
    
    output$plot <- renderPlot({
        ggplot(data=data(), aes(año, n)) + 
            geom_line(color = "steelblue", size = 1) +
            geom_bar(stat = "identity") +
            labs(title = "Traducciones por lengua y por año / Translations by language and year", y = "Número de traducciones / Number of translations", x = "") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        })
    
    # Generate a summary of the data ----    
    output$summary <- renderPrint({
        summary(borges_language_year)
    })
    
    # Generate an HTML table view of the data ----
    output$table <- renderDT({
        borges_language_year
    })
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)