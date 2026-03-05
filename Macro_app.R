library(tidyverse)
library(afcharts)
library(shiny)

###################   Data  ###################





###################  APP ######################

ui <- fluidPage(
  
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      body {
        background: white;
      }
      
      .chart-container {
        text-align: center;
        border: 3px solid #ae08c4;
        padding: 40px;
        background-color: #ffffff;
        width: 90%;
        margin: 50px auto;
        border-radius: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      }
      
      .chart-title {
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
    tabPanel("UK impact of JOB/AG/255",
             div(style = "padding: 40px;",
                 h1("WTO analysis: JOB/AG/255", class = "title-main"),
                 
                 div(class = "title-section",
                     h3("Tariff simplification", class = "title-subhead"),
                     p("This refers to converting complex and compound (non-ad valorem) tariffs into ad valorem tariffs", class = "title-text")
                 ),
                 
                 div(class = "title-section",
                     h3("Tariff peaks", class = "title-subhead"),
                     p("For industrialised countries like the UK, the WTO defines tariff peaks as above 15% across the entire tariff universe.", class = "title-text")
                 ),
                 
                 div(class = "title-section",
                     h3("Tariff escalation", class = "title-subhead"),
                     p("This refers to tackling what some members perceive as the “unfair” structure of tariff regimes, in which tariffs are either zero or low on primary products and increase, or escalate, as products undergo processing.", class = "title-text")
                 ),
                 # Footer section
                 div(class = "footer-section",
                     h4("Data sources & caveats", class = "footer-heading"),
                     div(class = "footer-content",
                         p(strong("Data sources:"), "All the data presented is taken from the 2025 XWH AVE estimates using HMRC data from 2023 and 2024."),
                         p(strong("Caveats:"), "All calculations and analysis excludes all preferential trade agreements."),
                         p(em("Created by Aadam Akbar 01/09/2025 | For any further questions please get in contact."))
                     )
                 )
             )
    ) # close tab panel
  ) # close tabset panel 
  
) #close UI


server <- function(input, output, session) {
  
  
} # close server