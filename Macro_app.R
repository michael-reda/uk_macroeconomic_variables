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
        color: #b50d3f;
        font-size: 2.2em;
        margin: 30px 0;
        font-weight: bold;
      }
      
      .title-section {
        margin: 30px 0;
        padding: 15px;
        border-left: 4px solid #b50d3f;
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
        border-top: 3px solid #b50d3f;
      }
      
      .footer-content {
        color: #666;
        font-size: 0.9em;
        line-height: 1.6;
      }
      
      .footer-heading {
        color: #b50d3f;
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
             ), # close tabPanel
    tabPanel( "Correlation Matrix"
      
    )# close tabPanel
  ) # close tabset panel 
  
) #close UI

server <- function(input, output, session) {
  
  output$chart <- renderPlotly({
    
    req(input$Variable)
    
    d <- macroeconomic_variables_df[
      macroeconomic_variables_df$variable == input$Variable, 
    ]
    
    req(nrow(d) > 0)
    
    p <- ggplot(d, aes(year, value)) +
      geom_line(color = "#F46A25") +
      
      geom_vline(xintercept = 2020, linetype = "dashed", color = "#E6A8A1") +
      geom_vline(xintercept = 2021, linetype = "dashed", color = "#A9C1D9") +
      geom_vline(xintercept = 2016, linetype = "dashed", color = "#A4C6D2") +
      geom_vline(xintercept = 1979, linetype = "dashed", color = "#C8B6D9") +
      geom_vline(xintercept = 1990, linetype = "dashed", color = "#C8B6D9") +
      geom_vline(xintercept = 1973, linetype = "dashed", color = "#BFD8C0")
    
    ggplotly(p) %>%
      layout(
        annotations = list(
          list(x = 2020, y = 0.95, xref = "x", yref = "paper",
               text = "COVID-19", textangle = -45, showarrow = FALSE,
               font = list(color = "#666")),
          
          list(x = 2021, y = 0.9, xref = "x", yref = "paper",
               text = "Brexit (TCA)", textangle = -45, showarrow = FALSE,
               font = list(color = "#666")),
          
          list(x = 2016, y = 0.95, xref = "x", yref = "paper",
               text = "Brexit Vote", textangle = -45, showarrow = FALSE,
               font = list(color = "#666")),
          
          list(x = 1979, y = 0.98, xref = "x", yref = "paper",
               text = "Thatcher Start", textangle = -45, showarrow = FALSE,
               font = list(color = "#666")),
          
          list(x = 1990, y = 0.95, xref = "x", yref = "paper",
               text = "Thatcher End", textangle = -45, showarrow = FALSE,
               font = list(color = "#666")),
          
          list(x = 1973, y = 0.95, xref = "x", yref = "paper",
               text = "Joined EEC", textangle = -45, showarrow = FALSE,
               font = list(color = "#666"))
        )
      )
    
  })
  
}

shinyApp(ui = ui, server = server)