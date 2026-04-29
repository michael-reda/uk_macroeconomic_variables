library(tidyverse)
library(afcharts)
library(shiny)
library(plotly)
###################   Data  ###################

macroeconomic_variables_df <- read_csv("data/macroeconomic_variables.csv") %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

variable_choices <- sort(unique(macroeconomic_variables_df$variable))

###################  APP ######################

ui <- fluidPage(
  
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      body {
        background: white;
      }
      
      .page-container {
        text-align: center;
        border: 4px solid #b50d3f;
        padding: 40px;
        background-color: #ffffff;
        width: 90%;
        margin: 50px auto;
        border-radius: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      }
      
      .page-title {
        font-size: 2em;
        color: #666666;
        margin-bottom: 20px;
      }
      
      .barchart-description {
        font-size: 1.1em;
        color: #666666;
        margin-bottom: 30px;
        line-height: 1.5;
      }
      
      /* Title page styles */
      .title-main {
        text-align: center;
        color: #089e21;
        font-size: 2.2em;
        margin: 30px 0;
        font-weight: bold;
      }
      
      .title-section {
        margin: 30px 0;
        padding: 15px;
        border-left: 4px solid #089e21;
      }
      
      .title-subhead {
        color: #666666;
        font-size: 1.4em;
        margin-bottom: 10px;
        font-weight: 600;
      }
      
      .title-text {
        color: #555;
        line-height: 1.5;
      }
      
            /* Footer section styles */
      .footer-section {
        margin-top: 50px;
        padding-top: 30px;
        border-top: 3px solid #089e21;
      }
      
      .footer-content {
        color: #666;
        font-size: 0.9em;
        line-height: 1.6;
      }
      
      .footer-heading {
        color: #089e21;
        font-weight: 600;
        margin-bottom: 15px;
      }
      "))
  ),
  
  tabsetPanel(
    tabPanel("UK Macroeconomics",
             div(style = "padding: 40px;",
                 h1("UK Macroeconomics", class = "title-main"),
                 
                 div(class = "title-section",
                     h3("Institue of fiscal Studies", class = "title-subhead"),
                     p("Poverty data", class = "title-text")
                 ),
                 
                 div(class = "title-section",
                     h3("ONS", class = "title-subhead"),
                     p("Office for National statistics", class = "title-text")
                 ),
                 
                 # Footer section
                 div(class = "footer-section",
                     h4("Data sources & caveats", class = "footer-heading"),
                     div(class = "footer-content",
                         p(strong("Data sources:"), "All the data presented is taken from the Bank of England, ONS and Institue for fiscal studies"),
                         p(strong("Caveats:"), "Think of something"),
                         p(em("Created by Michael Reda and Aadam Akbar 10/03/2026 | For any further questions please get in contact."))
                     )
                 )
             )
    ), # close tab panel
    tabPanel("UK graphs",
             fluidRow(
               div(class = "page-container",
               h2(class = "page-title", "UK Macroeconomic Graphs"),
               p(class = "barchart-description", 
                 "This tab allows you to select any key UK macroeconometic indicator from the dropdown and see how trends have changed over time."),
               
                 selectInput(
                   inputId = "Variable",
                   label = "Choose a variable:",
                   choices = variable_choices
                 ),
               
                 h3("Graphs:"),
                 
                 plotlyOutput("chart", width = "100%", height = "600px")
                 
               )# close chart container
              ) # close fluid
             ) # close tabPanel
  ) # close tabset panel 
  
) #close UI

server <- function(input, output, session) {
  
  output$chart <- renderPlotly({
    
    req(input$Variable)
    
    d <- macroeconomic_variables_df[
      macroeconomic_variables_df$variable == input$Variable, 
    ]
    
    req(nrow(d) > 0)
    
    ggplotly(
      ggplot(d, aes(year, value)) +
        geom_line(color = "#F46A25")
    )
    
  })
  
}

shinyApp(ui = ui, server = server)