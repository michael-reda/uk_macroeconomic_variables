library(tidyverse)
library(afcharts)
library(shiny)
library(plotly)
library(corrplot)
library(rsconnect)
###################   Data  ###################

macroeconomic_variables_df <- read_csv("data/macroeconomic_variables.csv") %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

macroeconomic_variables_df <- macroeconomic_variables_df |>
  mutate(title_i = case_when(variable == "interest_rate" ~ "Interest Rate",
                             variable == "cpi" ~ "CPI growth rate",
                             variable == "gdp_deflator_growth" ~ "GDP Deflator Growth Rate",
                             variable == "pub_sec_deficit_m" ~ "Public Sector Deficit, £m",
                             variable == "pub_sec_receipts_m" ~ "Public Sector Receipts, £m",
                             variable == "pub_sec_expenditure_m" ~ "Public Sector Expenditure, £m",
                             variable == "gdp_deflator_index" ~ "GDP Deflator Index",
                             variable == "gdp_m" ~ "GDP",
                             variable == "gdp_growth_rate" ~ "GDP Growth Rate",
                             variable == "gdp_per_capita" ~ "GDP per Capita",
                             variable == "capital_account_balance" ~ "Capital Account Balance",
                             variable == "urban_pop_pct" ~ "Urban Population, % of total",
                             variable == "life_expectancy" ~ "Life Expectancy",
                             variable == "mean_household_income" ~ "Mean Household Income",
                             variable == "median_household_income" ~ "Median Household Income",
                             variable == "gini_coefficient" ~ "Gini Coefficient",
                             variable == "ab_poverty_rate" ~ "Absolute Poverty Rate",
                             variable == "rel_poverty_rate" ~ "Relative Poverty Rate",
                             variable == "ab_child_poverty_rate" ~ "Absolute Child Poverty Rate",
                             variable == "rel_child_poverty_rate" ~ "Relative Child Poverty Rate",
                             variable == "trade_pct_of_gdp" ~ "Trade Intensity",
                             variable == "unemployment_rate" ~ "Unemployment Rate",
                             variable == "population" ~ "Population",
                             variable == "exchg_gbp_into_usd" ~ "Exchange Rate: GBP into USD",
                             variable == "exchg_gbp_into_eur" ~ "Exchange Rate: GBP into EUR",
                             variable == "started_dwellings" ~ "Started Dwellings",
                             variable == "completed_dwellings" ~ "Completed Dwellings",
                             variable == "exchg_rate_index" ~ "Exchange Rate Index",
                             variable == "manuf_val_added_pct_of_gdp" ~ "Manufacturing Value Added as a % of GDP",
                             variable == "pub_sec_net_debt_m" ~ "Public Sector Net Debt, £m",
                             variable == "pub_sec_net_debt_pct_of_gdp" ~ "Public Sector Net Debt as a % of GDP",
                             variable == "randd_exp_pct_of_gdp" ~ "R&D expenditure as a % of GDP",
                             variable == "gfcf" ~ "Gross Fixed Capital Formation, £m",
                             variable == "exports_total" ~ "Total Exports, £m",
                             variable == "imports_total" ~ "Total Imports, £m",
                             variable == "balance_total" ~ "Total Trade Balance, £m",
                             variable == "goods_imports_total" ~ "Goods Imports, £m",
                             variable == "goods_exports_total" ~ "Goods Exports, £m",
                             variable == "goods_balance_total" ~ "Goods Trade Balance, £m",
                             variable == "goods_balance_eu" ~ "Goods Trade Balance with the EU, £m",
                             variable == "goods_balance_non_eu" ~ "Goods Trade Balance, non-EU, £m",
                             variable == "goods_exports_eu" ~ "Goods Exports to the EU, £m",
                             variable == "goods_imports_eu" ~ "Goods Imports from the EU, £m",
                             variable == "goods_exports_non_eu" ~ "Goods Exports to non-EU, £m",
                             variable == "goods_imports_non_eu" ~ "Goods Imports from non-EU, £m",
                             variable == "services_exports_total" ~ "Services Exports, £m",
                             variable == "services_imports_total" ~ "Services Imports, £m",
                             variable == "services_balance_total" ~ "Services Trade Balance, £m",
                             .default = NA),
         var_info = case_when(variable == "interest_rate" ~ "The yearly average of official Bank rate",
                              variable == "cpi" ~ "annual growth rate of the consumer price index",
                              variable == "gdp_deflator_growth" ~ "",
                              variable == "pub_sec_deficit_m" ~ "Public sector debt is real 2023 prices",
                              variable == "pub_sec_receipts_m" ~ "real 2023 prices",
                              variable == "pub_sec_expenditure_m" ~ "real 2023 prices",
                              variable == "gdp_deflator_index" ~ "2023 = 100",
                              variable == "gdp_m" ~ "£m, real 2023 prices",
                              variable == "gdp_growth_rate" ~ "",
                              variable == "gdp_per_capita" ~ "GDP per capita",
                              variable == "capital_account_balance" ~ "",
                              variable == "urban_pop_pct" ~ "",
                              variable == "life_expectancy" ~ "",
                              variable == "mean_household_income" ~ "net of taxes and inclusive of benefits, equivalised household income, before housing costs, inflation adjusted",
                              variable == "median_household_income" ~ "net of taxes and inclusive of benefits, equivalised household income, before housing costs, inflation adjusted",
                              variable == "gini_coefficient" ~ "0 = perfect equality; 1 = perfect inequality",
                              variable == "ab_poverty_rate" ~ "Absolute poverty is the proportion of people whose household incomes are below 60% of the inflation adjusted 2010 median income. This is the official absolute poverty rate.",
                              variable == "rel_poverty_rate" ~ "Relative poverty is proportion of people whose household incomes are below 60% of the contemporary median. This is a poverty line which changes over time to reflect poverty in relation to average living standards.",
                              variable == "ab_child_poverty_rate" ~ "Absolute child poverty is the proportion of children whose household incomes are below 60% of the inflation adjusted 2010 median income. This is the official absolute poverty rate.",
                              variable == "rel_child_poverty_rate" ~ "Relative child poverty is proportion of children whose household incomes are below 60% of the contemporary median. This is a poverty line which changes over time to reflect poverty in relation to average living standards.",
                              variable == "trade_pct_of_gdp" ~ "",
                              variable == "unemployment_rate" ~ "aged 16 and over",
                              variable == "population" ~ "",
                              variable == "exchg_gbp_into_usd" ~ "",
                              variable == "exchg_gbp_into_eur" ~ "",
                              variable == "started_dwellings" ~ "",
                              variable == "completed_dwellings" ~ "",
                              variable == "exchg_rate_index" ~ "",
                              variable == "manuf_val_added_pct_of_gdp" ~ "",
                              variable == "pub_sec_net_debt_m" ~ "real 2023 prices",
                              variable == "pub_sec_net_debt_pct_of_gdp" ~ "",
                              variable == "randd_exp_pct_of_gdp" ~ "",
                              variable == "gfcf" ~ "",
                              variable == "exports_total" ~ "",
                              variable == "imports_total" ~ "",
                              variable == "balance_total" ~ "",
                              variable == "goods_imports_total" ~ "",
                              variable == "goods_exports_total" ~ "",
                              variable == "goods_balance_total" ~ "",
                              variable == "goods_balance_eu" ~ "",
                              variable == "goods_balance_non_eu" ~ "",
                              variable == "goods_exports_eu" ~ "",
                              variable == "goods_imports_eu" ~ "",
                              variable == "goods_exports_non_eu" ~ "",
                              variable == "goods_imports_non_eu" ~ "",
                              variable == "services_exports_total" ~ "",
                              variable == "services_imports_total" ~ "",
                              variable == "services_balance_total" ~ "",
                              .default = NA),
         var_units = case_when(variable == "interest_rate" ~ "%",
                               variable ==  "cpi" ~ "%",
                               variable == "gdp_deflator_growth" ~ "",
                               variable == "pub_sec_deficit_m" ~ "",
                               variable == "pub_sec_receipts_m" ~ "",
                               variable == "pub_sec_expenditure_m" ~ "",
                               variable == "gdp_deflator_index" ~ "",
                               variable == "gdp_m" ~ "",
                               variable == "gdp_growth_rate" ~ "GDP Growth Rate",
                               variable == "gdp_per_capita" ~ "GDP per Capita",
                               variable == "capital_account_balance" ~ "",
                               variable == "urban_pop_pct" ~ "",
                               variable == "life_expectancy" ~ "",
                               variable == "mean_household_income" ~ "",
                               variable == "median_household_income" ~ "",
                               variable == "gini_coefficient" ~ "",
                               variable == "poverty_rate" ~ "",
                               variable == "child_poverty_rate" ~ "",
                               variable == "trade_pct_of_gdp" ~ "",
                               variable == "unemployment_rate" ~ "",
                               variable == "population" ~ "",
                               variable == "exchg_gbp_into_usd" ~ "",
                               variable == "exchg_gbp_into_eur" ~ "",
                               variable == "started_dwellings" ~ "",
                               variable == "completed_dwellings" ~ "",
                               variable == "exchg_rate_index" ~ "",
                               variable == "manuf_val_added_pct_of_gdp" ~ "",
                               variable == "pub_sec_net_debt_m" ~ "",
                               variable == "pub_sec_net_debt_pct_of_gdp" ~ "",
                               variable == "randd_exp_pct_of_gdp" ~ "",
                               variable == "gfcf" ~ "",
                               variable == "exports_total" ~ "",
                               variable == "imports_total" ~ "",
                               variable == "balance_total" ~ "",
                               variable == "goods_imports_total" ~ "",
                               variable == "goods_exports_total" ~ "",
                               variable == "goods_balance_total" ~ "",
                               variable == "goods_balance_eu" ~ "",
                               variable == "goods_balance_non_eu" ~ "",
                               variable == "goods_exports_eu" ~ "",
                               variable == "goods_imports_eu" ~ "",
                               variable == "goods_exports_non_eu" ~ "",
                               variable == "goods_imports_non_eu" ~ "",
                               variable == "services_exports_total" ~ "",
                               variable == "services_imports_total" ~ "",
                               variable == "services_balance_total" ~ "",
                               .default = NA),
         var_source = case_when(variable == "interest_rate" ~ "Bank of England",
                                variable == "cpi" ~ "ONS",
                                variable == "gdp_deflator_growth" ~ "",
                                variable == "pub_sec_deficit_m" ~ "",
                                variable == "pub_sec_receipts_m" ~ "",
                                variable == "pub_sec_expenditure_m" ~ "",
                                variable == "gdp_deflator_index" ~ "",
                                variable == "gdp_m" ~ "",
                                variable == "gdp_growth_rate" ~ "GDP Growth Rate",
                                variable == "gdp_per_capita" ~ "GDP per Capita",
                                variable == "capital_account_balance" ~ "",
                                variable == "urban_pop_pct" ~ "",
                                variable == "life_expectancy" ~ "",
                                variable == "mean_household_income" ~ "",
                                variable == "median_household_income" ~ "",
                                variable == "gini_coefficient" ~ "",
                                variable == "ab_poverty_rate" ~ "",
                                variable == "rel_poverty_rate" ~ "",
                                variable == "ab_child_poverty_rate" ~ "",
                                variable == "rel_child_poverty_rate" ~ "",
                                variable == "trade_pct_of_gdp" ~ "",
                                variable == "unemployment_rate" ~ "",
                                variable == "population" ~ "",
                                variable == "exchg_gbp_into_usd" ~ "",
                                variable == "exchg_gbp_into_eur" ~ "",
                                variable == "started_dwellings" ~ "",
                                variable == "completed_dwellings" ~ "",
                                variable == "exchg_rate_index" ~ "",
                                variable == "manuf_val_added_pct_of_gdp" ~ "",
                                variable == "pub_sec_net_debt_m" ~ "",
                                variable == "pub_sec_net_debt_pct_of_gdp" ~ "",
                                variable == "randd_exp_pct_of_gdp" ~ "",
                                variable == "gfcf" ~ "",
                                variable == "exports_total" ~ "",
                                variable == "imports_total" ~ "",
                                variable == "balance_total" ~ "",
                                variable == "goods_imports_total" ~ "",
                                variable == "goods_exports_total" ~ "",
                                variable == "goods_balance_total" ~ "",
                                variable == "goods_balance_eu" ~ "",
                                variable == "goods_balance_non_eu" ~ "",
                                variable == "goods_exports_eu" ~ "",
                                variable == "goods_imports_eu" ~ "",
                                variable == "goods_exports_non_eu" ~ "",
                                variable == "goods_imports_non_eu" ~ "",
                                variable == "services_exports_total" ~ "",
                                variable == "services_imports_total" ~ "",
                                variable == "services_balance_total" ~ "",
                                .default = NA),
         var_url = case_when(variable == "interest_rate" ~ "url",
                             variable == "cpi" ~ "url",
                             variable == "gdp_deflator_growth" ~ "",
                             variable == "pub_sec_deficit_m" ~ "",
                             variable == "pub_sec_receipts_m" ~ "",
                             variable == "pub_sec_expenditure_m" ~ "",
                             variable == "gdp_deflator_index" ~ "",
                             variable == "gdp_m" ~ "",
                             variable == "gdp_growth_rate" ~ "GDP Growth Rate",
                             variable == "gdp_per_capita" ~ "GDP per Capita",
                             variable == "capital_account_balance" ~ "",
                             variable == "urban_pop_pct" ~ "",
                             variable == "life_expectancy" ~ "",
                             variable == "mean_household_income" ~ "",
                             variable == "median_household_income" ~ "",
                             variable == "gini_coefficient" ~ "",
                             variable == "ab_poverty_rate" ~ "",
                             variable == "rel_poverty_rate" ~ "",
                             variable == "ab_child_poverty_rate" ~ "",
                             variable == "rel_child_poverty_rate" ~ "",
                             variable == "trade_pct_of_gdp" ~ "",
                             variable == "unemployment_rate" ~ "",
                             variable == "population" ~ "",
                             variable == "exchg_gbp_into_usd" ~ "",
                             variable == "exchg_gbp_into_eur" ~ "",
                             variable == "started_dwellings" ~ "",
                             variable == "completed_dwellings" ~ "",
                             variable == "exchg_rate_index" ~ "",
                             variable == "manuf_val_added_pct_of_gdp" ~ "",
                             variable == "pub_sec_net_debt_m" ~ "",
                             variable == "pub_sec_net_debt_pct_of_gdp" ~ "",
                             variable == "randd_exp_pct_of_gdp" ~ "",
                             variable == "gfcf" ~ "",
                             variable == "exports_total" ~ "",
                             variable == "imports_total" ~ "",
                             variable == "balance_total" ~ "",
                             variable == "goods_imports_total" ~ "",
                             variable == "goods_exports_total" ~ "",
                             variable == "goods_balance_total" ~ "",
                             variable == "goods_balance_eu" ~ "",
                             variable == "goods_balance_non_eu" ~ "",
                             variable == "goods_exports_eu" ~ "",
                             variable == "goods_imports_eu" ~ "",
                             variable == "goods_exports_non_eu" ~ "",
                             variable == "goods_imports_non_eu" ~ "",
                             variable == "services_exports_total" ~ "",
                             variable == "services_imports_total" ~ "",
                             variable == "services_balance_total" ~ "",
                             .default = NA)
         
  )

title_choices <- unique(na.omit(macroeconomic_variables_df$title_i))

# recession
recessions_df <- tibble::tibble(
  start = c(1975.25, 1980, 1990.75, 2008.25, 2020),
  end   = c(1975.75, 1981.25, 1993.25, 2009.5, 2020.5)
)

#CORRELATION MATRIX

# Example structure
corr_data <- macroeconomic_variables_df%>%
  select(year, value, title_i)%>%
  pivot_wider(names_from = title_i,
              values_from = value)%>%
  select(-year)

cor_mat <- cor(corr_data,
               method = "pearson", 
               use = "pairwise.complete.obs")

corrplot(cor_mat, 
         method = "circle", 
         type = "lower", 
         tl.col = "black", 
         tl.cex = 0.8)

corrplot(cor_mat, method = 'color', order = 'alphabet')

corrplot(cor_mat)

n_obs <- function(x, y) sum(complete.cases(x, y))
obs_matrix <- outer(
  colnames(corr_data),
  colnames(corr_data),
  Vectorize(function(i, j) n_obs(corr_data[[i]], corr_data[[j]]))
)


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
             div(class = "page-container",
                 h2(class = "page-title", "UK Macroeconomic Graphs"),
                 p(class = "barchart-description", 
                   "This tab allows you to select any key UK macroeconomic indicator from the dropdown and see how trends have changed over time."),
                 fluidRow(
                   tabsetPanel(
                     
                     tabPanel(
                       title = "Solo Graph",
                       value = "solo",
                       br(),
                       p(style = "font-size: 18px;", "Select a variable to view historic data"),
                       br(),
                       column(12,
                              selectInput("Variable", "Choose a variable:", choices = title_choices),
                              p(textOutput("var_description")),
                              plotlyOutput("chart", width = "100%", height = "600px")
                       )
                     ), #close tab panel 1
                     
                     tabPanel(
                       title = "Comparison Index",
                       value = "comparison",
                       br(),
                       p(style = "font-size: 18px;", "Select multiple variables to compare indexed to 100 at a base year"),
                       br(),
                       
                       helpText(
                         "All selected variables are rebased to 100 in the chosen base year. Values above 100 indicate growth since the base year, while values below 100 indicate a decline. This allows variables measured in different units to be compared on the same chart.
                          Indexing removes differences in units and scale, making trends easier to compare."
                       ),
                       
                       column(
                         12,
                         checkboxInput(
                           "log_scale",
                           "Use log scale",
                           value = FALSE
                         ),
                         helpText(
                           "Log scale makes it easier to compare growth paths when variables have very different growth rates. Equal vertical distances represent equal percentage changes rather than equal absolute index changes."
                         )
                       ),
                       helpText(
                         "Recommended when comparing variables such as GDP, debt, exports and population over long periods. A log scale highlights relative (%) growth rather than absolute increases."
                       ),
                       
                       column(12,
                              selectInput("CompareVars", "Choose variables to compare:",
                                          choices  = title_choices,
                                          multiple = TRUE),
                              numericInput("BaseYear", "Index base year:", 
                                           value = 2000, min = 1970, max = 2023, step = 1),
                              
                              plotlyOutput("chart_comparison", width = "100%", height = "600px")
                       )
                     ), #close tab panel 2
                     
                     tabPanel(
                       title = "Versus",
                       value = "vs",
                       fluidRow(
                         column(6,
                                selectInput(
                                  inputId = "x_var",
                                  label = "Choose x axis variable:",
                                  choices = title_choices
                                )
                         ),
                         column(6,
                                selectInput(
                                  inputId = "y_var",
                                  label = "Choose y axis variable:",
                                  choices = title_choices
                                )
                         ),
                         
                         plotlyOutput("chart2", width = "100%", height = "600px")
                         
                       )
                     ) #close tab panel 3
                   ) # close tabset panel
                 ) #close fluid row
             )# close div
             
    ) #close tabpanel
  ) # close tabset panel 
  
) #close UI

server <- function(input, output, session) {

  output$chart <- renderPlotly({
    
    req(input$Variable)
    
    d <- macroeconomic_variables_df[
      macroeconomic_variables_df$title_i == input$Variable,
    ]
    
    req(nrow(d) > 0)
    
    units <- d$var_units[1]
    y_label <- if (!is.na(units) && units != "") units else "Value"
    
    events <- data.frame(
      x     = c(1973,         1979,             1990,            2016,          2020,       2021),
      label = c("Joined EEC", "Thatcher Start", "Thatcher End", "Brexit Vote", "COVID-19", "Brexit (TCA)"),
      color = c("#BFD8C0",    "#C8B6D9",        "#C8B6D9",      "#A4C6D2",    "#E6A8A1",  "#A9C1D9")
    )
    
    p <- ggplot(d, aes(year, value)) +
      geom_vline(
        data        = events,
        aes(xintercept = x, color = label),
        linetype    = "dashed",
        show.legend = FALSE
      ) +
      scale_color_manual(values = setNames(events$color, events$label)) +
      geom_line(color = "#F46A25") +
      ylab(y_label)
    
    annotations <- lapply(seq_len(nrow(events)), function(i) {
      list(
        x         = events$x[i],
        y         = 0.97,
        xref      = "x", yref = "paper",
        text      = events$label[i],
        textangle = -45,
        showarrow = FALSE,
        font      = list(color = "#666666", size = 11)
      )
    })
    
    ggplotly(p) %>%
      layout(annotations = annotations)
    
  })
  
  # --- Comparison index chart -------------------------------------------------
  output$chart_comparison <- renderPlotly({
    
    req(input$CompareVars)
    req(length(input$CompareVars) >= 2)
    req(input$BaseYear)
    
    # Filter to selected variables
    d <- macroeconomic_variables_df[
      macroeconomic_variables_df$title_i %in% input$CompareVars,
    ]
    req(nrow(d) > 0)
    
    # Index each variable to 100 at base year
    d <- d %>%
      group_by(title_i) %>%
      mutate(
        base_value = value[year == input$BaseYear],
        indexed    = (value / base_value) * 100
      ) %>%
      filter(!is.na(indexed)) %>%
      ungroup()
    
    p <- ggplot(d, aes(x = year, y = indexed, color = title_i)) +
      geom_line() +
      geom_hline(yintercept = 100,
                 linetype = "dashed",
                 color = "#999999") +
      ylab(paste0("Index (", input$BaseYear, " = 100)")) +
      labs(color = NULL) +
      theme(legend.position = "bottom")
    
    if (isTRUE(input$log_scale)) {
      p <- p +
        scale_y_log10()
    }
    
    ggplotly(p) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.2)
      )
  })
  
  #############################################################################
  
  output$var_description <- renderText({
    req(input$Variable)
    
    info <- macroeconomic_variables_df$var_info[
      macroeconomic_variables_df$title_i == input$Variable
    ][1]  # [1] to avoid duplicates
    
    if (!is.na(info) && info != "") {
      paste0(info)
    } else {
      ""
    }
  })
  
  output$chart2 <- renderPlotly({
    
    req(input$x_var, input$y_var)
    
    x_var_clean <- macroeconomic_variables_df$variable[
      macroeconomic_variables_df$title_i == input$x_var
    ][1]
    
    y_var_clean <- macroeconomic_variables_df$variable[
      macroeconomic_variables_df$title_i == input$y_var
    ][1]
    
    d <- macroeconomic_variables_df[
      macroeconomic_variables_df$variable %in% c(x_var_clean, y_var_clean),
    ]
    
    req(nrow(d) > 0)
    
    d_wide <- pivot_wider(
      d,
      id_cols = year,
      names_from = variable,
      values_from = value
    )
    
    ggplotly(
      ggplot(d_wide, aes(x = .data[[x_var_clean]], y = .data[[y_var_clean]])) +
        geom_point(color = "#F46A25") +
        geom_smooth(method = "lm", se = FALSE, color = "#555555") +
        xlab(input$x_var) +
        ylab(input$y_var)
    )
    
  })

}

shinyApp(ui = ui, server = server)