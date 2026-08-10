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
        background: #f8f9fb;
      }
      
      /* Hero section */
      
      .hero {
      text-align: center;
      padding: 60px 40px;
      margin: 30px auto 40px auto;
      background: white;
      border: 3px solid #b50d3f;
      border-radius: 24px;
      max-width: 1200px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.06);
      }
      
      .hero-title {
      font-size: 3rem;
      font-weight: 700;
      color: #1f2937;
      margin-bottom: 15px;
      }
      
      .hero-subtitle {
      font-size: 1.15rem;
      line-height: 1.7;
      color: #6b7280;
      max-width: 900px;
      margin: 0 auto;
      }
      
      
      /* Analysis containers */
      
      .analysis-card {
      background: white;
      border: 3px solid #b50d3f;
      border-radius: 24px;
      padding: 30px;
      margin: 0 auto 30px auto;
      max-width: 1200px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.06);
      }
      
      .section-title {
      font-size: 1.8rem;
      font-weight: 600;
      color: #1f2937;
      margin-bottom: 10px;
      padding-left: 15px;
      border-left: 5px solid #b50d3f;
      }
      
      .section-description {
      color: #6b7280;
      font-size: 1rem;
      line-height: 1.6;
      margin-bottom: 25px;
      }
      
      /* Inputs */
      
      .control-section {
      margin-bottom: 25px;
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
  
  div(
    class = "hero",
    h1(class = "hero-title",
      "Historic UK Macroeconometic data"
    ),
    
    p(
      class = "hero-subtitle",
      "Use the data visualisations below to investigate historic trends in UK macroeconomic indicators,
      compare variables using indexed series, and investigate
      relationships between economic measures."
    )
  ),
  
  div( ### Title and description ####
    class = "analysis-card",
    h2(class = "section-title",
      "Single Variable Analysis"),
    p(class = "section-description",
      "Select an economic indicator and explore its historical trend."),
    
    selectInput(
      "Variable",
      "Choose a variable:",
      choices = title_choices
    ),
    
    textOutput("var_description"),
    plotlyOutput(
      "chart",
      height = "600px"
    )
  ),

  ######################   INDEXED COMPARISON  ############################  
  
  div( ### Title and description ####
    class = "analysis-card",
    h2(class = "section-title",
      "Indexed Comparison"),
    p(class = "section-description",
      "Compare multiple economic indicators after rebasing them to a common base year."),
    
    p(h4("Using the drop downs below you can compare between multiple varables overtime, choosing a year of comparison to index the variables. " )),
    
      checkboxInput(
        "log_scale",
        "Use log scale",
        value = FALSE
      ),
      helpText(
        "Log scale makes it easier to compare growth paths when variables have very different growth rates. Equal vertical distances represent equal percentage changes rather than equal absolute index changes."
      ),
    helpText(
      "This is recommended when comparing variables such as GDP, debt, exports and population over long periods. A log scale highlights relative (%) growth rather than absolute increases."
    ),
    selectInput("CompareVars", "Choose variables to compare:",
                choices  = title_choices,
                multiple = TRUE),
    numericInput("BaseYear", "Index base year:",
                 value = 2000, min = 1970, max = 2023, step = 1),
    plotlyOutput("chart_comparison", width = "100%", height = "600px")

  ), # close div
  
  ######################   CORRELATION COMPARISON  ############################   
  div(
    class = "analysis-card",
    h2(
      class = "section-title",
      "Relationship Analysis"
    ),
    p(
      class = "section-description",
      "Investigate the relationship between two variables using a
    dynamic scatter plot and correlation statistics."
    ),
    
           p(style = "font-size: 18px;",
             "Compare two variables directly against each other over time."),
           helpText(
             "Each point is one year. The line connecting points follows chronological order, so you can see the path the relationship has taken over time rather than just a static scatter. The grey trend line is a simple linear fit (OLS) and the correlation coefficient below the chart summarises how closely the two variables move together on a scale from -1 (perfectly opposite) to +1 (perfectly aligned)."
           ),
    
           checkboxInput(
             "standardise_vs",
             "Standardise variables (z-scores)",
             value = FALSE),
           helpText(
             "Rescales both variables to the same units - standard deviations from their own mean - so a variable measured in billions and one measured in percentage points can be compared on the same axes without one visually dominating the other. This changes the scale of the chart but does not change the correlation coefficient itself."
           ),

           checkboxInput(
             "diff_vs",
             "Use year-on-year changes (removes spurious trend correlation)",
             value = FALSE),
           helpText(
             "Two series that both trend over time (e.g. GDP and population) can show a high correlation purely because they share a common upward trend, not because they're actually related. Ticking this box correlates year-on-year changes instead of raw levels, which strips out shared trends and reveals whether the variables genuinely move together from one year to the next. This answers a different question to the levels correlation ('do they move together year to year?' vs 'do they trend together over time?') - it isn't strictly more correct, just a different, trend-robust lens, and it will typically show a lower r than the levels version."
           ),

           selectInput(
             inputId = "x_var",
             label = "Choose x axis variable:",
             choices = title_choices),

           selectInput(
             inputId = "y_var",
             label = "Choose y axis variable:",
             choices = title_choices),

    br(),
    
           plotlyOutput("chart2", width = "100%", height = "600px"),
           verbatimTextOutput("corr_text")
  ), # close div
  
             div(class = "footer-section",
                 h4("Data sources & caveats", class = "footer-heading"),
                 div(class = "footer-content",
                     p(strong("Data sources:"), "All the data presented is taken from the Bank of England, ONS and Institue for fiscal studies"),
                     p(strong("Caveats:"), "Think of something"),
                     p(em("Created by Michael Reda and Aadam Akbar 10/03/2026 | For any further questions please get in contact."))
                 )
             )

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
  
  # Shared reactive for the Versus tab
  vs_data <- reactive({
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
      d, id_cols = year, names_from = variable, values_from = value
    ) %>%
      filter(!is.na(.data[[x_var_clean]]), !is.na(.data[[y_var_clean]])) %>%
      arrange(year)
    
    x_lab <- input$x_var
    y_lab <- input$y_var
    
    # Year-on-year differencing to strip out shared time trends before
    # correlating - guards against spurious correlation between two series
    # that are each just trending over time.
    if (isTRUE(input$diff_vs)) {
      d_wide <- d_wide %>%
        mutate(
          !!x_var_clean := .data[[x_var_clean]] - lag(.data[[x_var_clean]]),
          !!y_var_clean := .data[[y_var_clean]] - lag(.data[[y_var_clean]])
        ) %>%
        filter(!is.na(.data[[x_var_clean]]), !is.na(.data[[y_var_clean]]))
      
      x_lab <- paste0(x_lab, " (year-on-year change)")
      y_lab <- paste0(y_lab, " (year-on-year change)")
    }
    
    if (isTRUE(input$standardise_vs)) {
      d_wide[[x_var_clean]] <- as.numeric(scale(d_wide[[x_var_clean]]))
      d_wide[[y_var_clean]] <- as.numeric(scale(d_wide[[y_var_clean]]))
      x_lab <- paste0(x_lab, " (z-score)")
      y_lab <- paste0(y_lab, " (z-score)")
    }
    
    corr <- cor(
      d_wide[[x_var_clean]], d_wide[[y_var_clean]],
      use = "complete.obs", method = "pearson"
    )
    
    list(
      d_wide = d_wide, x_var_clean = x_var_clean, y_var_clean = y_var_clean,
      x_lab = x_lab, y_lab = y_lab, corr = corr
    )
  })
  
  output$chart2 <- renderPlotly({
    vd <- vs_data()
    
    p <- ggplot(
      vd$d_wide,
      aes(x = .data[[vd$x_var_clean]], y = .data[[vd$y_var_clean]])
    ) +
      geom_path(colour = "#999999", linewidth = 0.6) +
      geom_point(aes(color = year)) +
      scale_color_viridis_c(name = "Year") +
      geom_smooth(method = "lm", se = FALSE, color = "#555555") +
      xlab(vd$x_lab) +
      ylab(vd$y_lab)
    
    # When plotting year-on-year changes, one or two shock years (e.g. 2008,
    # 2020) can be so large they stretch the axes and compress every other
    # year into a tiny cluster. Zoom the view to a robust range (1st-99th
    # percentile with a margin) instead of the full data range - outlier
    # points still exist and are still included in the correlation, they're
    # just allowed to sit outside the visible window if extreme.
    if (isTRUE(input$diff_vs)) {
      
      x_vals <- vd$d_wide[[vd$x_var_clean]]
      y_vals <- vd$d_wide[[vd$y_var_clean]]
      
      x_range <- quantile(x_vals, probs = c(0.01, 0.99), na.rm = TRUE)
      y_range <- quantile(y_vals, probs = c(0.01, 0.99), na.rm = TRUE)
      
      x_pad <- diff(x_range) * 0.15
      y_pad <- diff(y_range) * 0.15
      
      p <- p + coord_cartesian(
        xlim = x_range + c(-x_pad, x_pad),
        ylim = y_range + c(-y_pad, y_pad)
      )
    }
    
    ggplotly(p)
  })
  
  output$corr_text <- renderText({
    vd <- vs_data()
    basis <- if (isTRUE(input$diff_vs)) "year-on-year changes" else "levels"
    paste0(
      "Pearson correlation (r), based on ", basis, ": ",
      round(vd$corr, 3), "  |  n = ", nrow(vd$d_wide)
    )
  })

}

shinyApp(ui = ui, server = server)