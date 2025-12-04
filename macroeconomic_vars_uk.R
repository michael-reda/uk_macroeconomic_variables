#install.packages("tidyverse")
library(tidyverse)
library(afcharts)

## read in data

# Bank of England data: interest rates and exchange rates
# https://www.bankofengland.co.uk/boeapps/database/default.asp
# IUMABEDR Monthly average of official Bank Rate
# XUMABK67 Monthly average Effective exchange rate index, Sterling (Jan 2005 = 100)
# XUMAGBD Monthly average Spot exchange rate, Sterling into US$
# XUMASER Monthly average Spot exchange rates, Sterling into Euro
boe_exchg_interest_df <- read.csv("data/boe_df.csv")

boe_exchg_interest_df <- boe_exchg_interest_df |>
  rename(date = Date,
         interest_rate = Monthly.average.of.official.Bank.Rate...............a...b..............IUMABEDR,
         exchg_rate_index = Monthly.average.Effective.exchange.rate.index..Sterling..Jan.2005...100................c...c...c...c...c...c...d...c..............XUMABK67,
         exchg_gbp_into_usd = Monthly.average.Spot.exchange.rate..Sterling.into.US................d..............XUMAGBD,
         exchg_gbp_into_eur = Monthly.average.Spot.exchange.rates..Sterling.into.Euro...............d..............XUMASER
  )

boe_exchg_interest_df$date <- lubridate::dmy(boe_exchg_interest_df$date)

str(boe_exchg_interest_df)
boe_exchg_interest_df$exchg_rate_index <- as.numeric(boe_exchg_interest_df$exchg_rate_index)

boe_exchg_interest_df <- mutate(boe_exchg_interest_df, year = lubridate::year(date))

boe_annual_df <- boe_exchg_interest_df |>
  group_by(year)|>
  summarise(across(2:5, mean, na.rm = TRUE))

rm(boe_exchg_interest_df)

# Institute for Fiscal Studies living standards, poverty and inequality data
# https://ifs.org.uk/living-standards-poverty-and-inequality-uk
# net [of taxes and inclusive of benefits] equivalised household income, before housing costs, inflation adjusted.
ifs_income_bhc_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Income (BHC)", range = "a3:f66")|>
  select(2, 4, 6)
    
    #mean weekly: col D; median weekly: col F
ifs_inequality_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Inequality", range = "a3:d66")|>
  select(2,4)#gini coefficient: col D

# use col M: absolute poverty: proportion below 60% of the inflation adjusted 2010 median income. This is the official absolute poverty rate.
ifs_poverty_bhc_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Poverty (BHC)", range = "a3:m66")|>
  select(2, 13)|>
  rename(poverty_rate = `60pc...13`)
  
ifs_child_poverty_bhc_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Child Poverty (BHC)", range = "a3:m66")|>
  select(2, 13)|>
  rename(child_poverty_rate = `60pc...13`)

ifs_df <- dplyr::left_join(ifs_income_bhc_df, ifs_inequality_df, by = "Year")%>%
  left_join(., ifs_poverty_bhc_df, by = "Year")%>%
  left_join(., ifs_child_poverty_bhc_df, by = "Year")
  
# for the joined dataframe

ifs_df <- ifs_df |> mutate(Year = stringr::str_sub(Year, 1, 4))|>
  rename(year = Year,
         mean_household_income = `Mean income`,
         median_household_income = `Median income`,
         gini_coefficient = `Gini coefficient`
  )|>
  mutate(year = as.numeric(year))
  
rm(ifs_child_poverty_bhc_df,
   ifs_income_bhc_df,
   ifs_inequality_df,
   ifs_poverty_bhc_df)

# ONS public sector finance data
# https://www.ons.gov.uk/economy/governmentpublicsectorandtaxes/publicsectorfinance/bulletins/publicsectorfinances/september2025
# quarterly_public_sector_net_borrowing : J5II
# quarterly_public_sector_spending : KX5Q
# quarterly_public_sector_receipts : JW2O
# public_sector_net_debt £bn: HF6W
# public_sector_net_debt as % of GDP: HF6X
# all are nominal.
public_sector_finances_df <- readxl::read_xlsx("data/ons_public_sector_finances_quarterly.xlsx")

public_sector_finances_df <- public_sector_finances_df[public_sector_finances_df[1, ]%in% c("CDID", "J5II", "KX5Q", "JW2O", "HF6W", "HF6X")]

names(public_sector_finances_df) <- c("year",
                                             "pub_sec_net_debt_m", 
                                             "pub_sec_net_debt_pct_of_gdp",
                                             "pub_sec_deficit_m",
                                             "pub_sec_receipts_m",
                                             "pub_sec_expenditure_m")


public_sector_finances_df <- public_sector_finances_df |>
  slice(15:93)

public_sector_finances_df[] <- lapply(public_sector_finances_df[], as.numeric)

public_sector_finances_df$pub_sec_net_debt_m <- public_sector_finances_df$pub_sec_net_debt_m * 1000

# ONS gross fixed capital formation
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/grossfixedcapitalformationbysectorandasset
# sheet = "G1_CVM_SA_Q_levels" array = "a6:m121"
gfcf_df <- readxl::read_xlsx("data/ons_grossfixedcapitalformationbysectorandasset.xlsx", sheet = "G1_CVM_SA_Q_levels", range = "a6:m121")|>
  select(1, 13)|>
  rename(date_qtrly = `Time period column and title row`,
         gfcf = `Total sector (S.1) Total asset (GFCF)`)|>
  slice(2:n())|>
  mutate(gfcf = as.numeric(gfcf),
         date_qtrly = lubridate::yq(date_qtrly),
         year = lubridate::year(date_qtrly)
  )|> 
  group_by(year)|>
  summarise(gfcf = sum(gfcf))|>
  ungroup()
  

# ONS UK housebuilding
# https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/ukhousebuildingpermanentdwellingsstartedandcompleted
 #"1a" "6a:195j"
housebuilding_df <- readxl::read_xlsx(path = "data/ons_ukhousebuilding.xlsx", sheet = "1a", range = "b6:j195")|>
  select("Period", "Started - All Dwellings", "Completed - All Dwellings")|>
  mutate(Period = lubridate::my(Period),
         year = lubridate::year(Period),
         `Started - All Dwellings` = as.numeric(`Started - All Dwellings`),
         `Completed - All Dwellings` = as.numeric(`Completed - All Dwellings`)
  )|>
  group_by(year)|>
  summarise(started_dwellings = sum(`Started - All Dwellings`),
            completed_dwellings = sum(`Completed - All Dwellings`)
            )|>
  ungroup()


# ONS Capital Account: Balance: CP NSA
# https://www.ons.gov.uk/economy/nationalaccounts/balanceofpayments/timeseries/fkmj/pnbp
capital_account_balance_df <- readr::read_csv(file = "data/ons_capital_account.csv")|>
  slice(22:86)|>
  rename(year = Title,
          capital_account_balance_cp = `Capital Account: Balance: CP NSA`)|>
  mutate(year = as.numeric(year),
         capital_account_balance_cp = as.numeric(capital_account_balance_cp)
  )

# ONS trade stats, 1997 - 2025, Chain Volume Measured, EU, non-EU, total, goods, services, imports, exports
# https://www.ons.gov.uk/economy/nationalaccounts/balanceofpayments/datasets/tradeingoodsmretsallbopeu2013timeseriesspreadsheet
# YW: Total Trade (TT): WW: Exports: BOP: CVM: SA
# YX: Total Trade (TT): WW: Imports: BOP: CVM: SA
# YY: Total Trade (TT): WW: Balance: BOP: CVM: SA

# EK: Trade in Goods (T): WW: Imports: BOP: CVM: SA
# EL: Trade in Goods (T): WW: Exports: BOP: CVM: SA
# JG: Trade in Goods (T): WW: Balance: BOP: CVM: SA
# JH:Trade in Goods (T): EU: Balance: BOP: CVM: SA
# JI: Trade in Goods (T): Non-EU: Balance: BOP: CVM: SA
# AER: Trade in Goods (T): EU: Exports: BOP: CVM: SA
# AES: Trade in Goods (T): EU: Imports: BOP: CVM: SA
# AET: Trade in Goods (T): Non-EU: Exports: BOP: CVM: SA
# AEU: Trade in Goods (T): Non-EU: Imports: BOP: CVM: SA

# Trade in services is not disaggregated by EU / non-EU.
# YQ: Trade in Services (TS): WW: Exports: BOP: CVM: SA
# YR: Trade in Services (TS): WW: Imports: BOP: CVM: SA
# YS: Trade in Services (TS): WW: Balance: BOP: CVM: SA

trade_balance_df <- readxl::read_xlsx("data/ons_uk_trade_stats.xlsx", sheet = "data", range = "a1:aeu1161")|>
  select(c("Title",
           "Total Trade (TT): WW: Exports: BOP: CVM: SA",
           "Total Trade (TT): WW: Imports: BOP: CVM: SA",
           "Total Trade (TT): WW: Balance: BOP: CVM: SA",
           "Trade in Goods (T): WW: Imports: BOP: CVM: SA",
           "Trade in Goods (T): WW: Exports: BOP: CVM: SA", 
           "Trade in Goods (T): WW: Balance: BOP: CVM: SA", 
           "Trade in Goods (T): EU: Balance: BOP: CVM: SA", 
           "Trade in Goods (T): Non-EU: Balance: BOP: CVM: SA", 
           "Trade in Goods (T): EU: Exports: BOP: CVM: SA", 
           "Trade in Goods (T): EU: Imports: BOP: CVM: SA", 
           "Trade in Goods (T): Non-EU: Exports: BOP: CVM: SA", 
           "Trade in Goods (T): Non-EU: Imports: BOP: CVM: SA", 
           "Trade in Services (TS): WW: Exports: BOP: CVM: SA", 
           "Trade in Services (TS): WW: Imports: BOP: CVM: SA", 
           "Trade in Services (TS): WW: Balance: BOP: CVM: SA"))|>
  slice(59:86)

trade_balance_variable_names <- c("year",
                                  "exports_total", 
                                  "imports_total", 
                                  "balance_total", 
                                  "goods_imports_total", 
                                  "goods_exports_total",
                                  "goods_balance_total",
                                  "goods_balance_eu",
                                  "goods_balance_non_eu",
                                  "goods_exports_eu",
                                  "goods_imports_eu",
                                  "goods_exports_non_eu",
                                  "goods_imports_non_eu",
                                  "services_exports_total",
                                  "services_imports_total",
                                  "services_balance_total"
)

names(trade_balance_df) <- trade_balance_variable_names

trade_balance_df[] <- lapply(trade_balance_df[], as.numeric)

# ONS quarterly GDP in chained volume measures (2023 prices)
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/realtimedatabaseforukgdpabmi
gdp_df <- readxl::read_xlsx("data/ons_uk_quarterly_GDP_real.xlsx", sheet = "2018 - ", range = "A4:BH286")|>
  select(1, 60)|>
  mutate(year = substr(`Publication date and time period`, 4, 7))|>
  mutate(year = as.numeric(year))|>
  select(year, gdp = `Sep-25 [2023 prices]\r\nQNA`)|>
  group_by(year)|>
  summarise(gdp = sum(gdp))|>
  ungroup()|>
  filter(year != 2025)


# ONS CPI
# https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindices
cpi_df <- readr::read_csv("data/ons_uk_cpi_89_25.csv")|>
  slice(8:43)|>
  mutate(year = as.numeric(Title),
         cpi = as.numeric(`CPI ANNUAL RATE 00: ALL ITEMS 2015=100`)
  )|>
  select(year, cpi)

# GDP deflator
#https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/ihys/qna
gdp_deflator_df <- readr::read_csv("data/ons_gdp_deflator_yoy_growth.csv")|>
  slice(8:83)|>
  mutate(year = as.numeric(Title),
         gdp_deflator_growth = as.numeric(`GDP Deflator: Year on Year growth: SA %`)
  )|>
  select(year, gdp_deflator_growth)


# ONS Unemployment rate (aged 16 and over, seasonally adjusted): %
# https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms
unemployment_df <- readr::read_csv("data/ons_uk_unemployment_71_24.csv")|>
  slice(8:61)|>
  mutate(year = as.numeric(Title),
         unemployment_rate = as.numeric(`Unemployment rate (aged 16 and over, seasonally adjusted): %`)
  )|>
  select(year, unemployment_rate)

# ONS population
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/ukpop/pop
population_df <- readr::read_csv("data/ons_uk_population_71_24.csv")|>
  slice(8:61)|>
  mutate(year = as.numeric(Title),
         population = as.numeric(`United Kingdom population mid-year estimate`)
  )|>
  select(year, population)


# trade intensity (% of GDP), World Bank
# https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS
trade_intensity_df <- readr::read_csv("data/wb_trade_intensity.csv", skip = 3)|>
  filter(`Country Code` == "GBR")


# Research and development expenditure (% of GDP), World Bank
# https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS
randd_expenditure_df <- readr::read_csv("data/wb_gerd_annual.csv", skip = 3)|>
  filter(`Country Code` == "GBR")

# Manufacturing value added (% of GDP), World Bank
# https://data.worldbank.org/indicator/NV.IND.MANF.ZS?end=2024&start=1960&view=chart
manuf_val_added_df <- readr::read_csv("data/wb_manuf_val_added.csv", skip = 3)|>
  filter(`Country Code` == "GBR")

# Urban population (% of total population) - United Kingdom
urban_pop_pct_df <- readr::read_csv("data/wb_urban_pop_pct.csv", skip = 3)|>
  filter(`Country Code` == "GBR")

wb_df <- rbind(trade_intensity_df, randd_expenditure_df, manuf_val_added_df, urban_pop_pct_df)%>%
  select(c(3, 5:(ncol(.) - 1)))%>%
  pivot_longer(cols = 2:ncol(.), names_to = "year", values_to = "value")

wb_df$year <- as.numeric(wb_df$year)

wb_df <- pivot_wider(wb_df, names_from = `Indicator Name`, values_from = value)

new_names_for_wb_df <- c("year", "trade_pct_of_gdp", "randd_exp_pct_of_gdp", "manuf_val_added_pct_of_gdp", "urban_pop_pct")

names(wb_df) <- new_names_for_wb_df

# need to add 2022: 2.69, and 2023: 2.64 to R&D expenditure (source: 2023 edition of https://www.ons.gov.uk/economy/governmentpublicsectorandtaxes/researchanddevelopmentexpenditure )
wb_df$randd_exp_pct_of_gdp[wb_df$year == 2022] <- 2.69
wb_df$randd_exp_pct_of_gdp[wb_df$year == 2023] <- 2.64

rm(trade_intensity_df,
   randd_expenditure_df,
   manuf_val_added_df,
   urban_pop_pct_df
)

# Life expectancy, Our World in Data
# Data source: Riley (2005); Zijdeman et al. (2015); HMD (2025); UN WPP (2024) – Learn more about this data
# OurWorldinData.org/life-expectancy | CC BY
# https://ourworldindata.org/life-expectancy
life_expectancy <- readr::read_csv("data/owid_life_expectancy_uk_61_23.csv")|>
  select(year = Year,
         life_expectancy = `Period life expectancy at birth`)

# join all the data together
joined_df <- full_join(boe_annual_df, cpi_df, by = "year")%>%
             full_join(., gdp_deflator_df, by = "year")%>%
             full_join(., gdp_df, by = "year")%>%
             full_join(., unemployment_df, by = "year")%>%
             full_join(., gfcf_df, by = "year")%>%
             full_join(., housebuilding_df, by = "year")%>%
             full_join(., capital_account_balance_df, by = "year")%>%
             full_join(., wb_df, by = "year")%>%
             full_join(., trade_balance_df, by = "year")%>%
             full_join(., public_sector_finances_df, by = "year")%>%
             full_join(., population_df, by = "year")%>%
             full_join(., life_expectancy, by = "year")%>%
             full_join(., ifs_df, by = "year"
             )
            

# reorder by year and remove 2025 and pre- 1948
joined_df <- joined_df[order(joined_df$year), ]|>
  filter(year != 2025 & year >= 1949)

# make a note of which variables are nominal, which need summarising by averaging or summing up to annual, which need dividing or multiplying by an index
# Boe: all indices or rates so all fine
# IFS: already inflation adjusted
# ONS public sector finances: all are nominal!
# GFCF is CVM (real) so all fine
# housebuilding: all fine
# capital account balance: current prices, need to adjust for inflation!
# all others are fine

# create the GDP deflator index. 1948 = 100
joined_df <- joined_df |>
    mutate(gdp_deflator_index_48 = cumprod(1 + (gdp_deflator_growth /  100)))

# rebase to 2023. Treasury guidance: "divide all the deflators by the value of the deflator in the new reference year, then multiply by 100."
index_value_in_2023 <- joined_df |>
  filter(year == 2023)|>
  select(gdp_deflator_index_48)|>
  pull()
  
joined_df <- joined_df |>
    mutate(gdp_deflator_index = gdp_deflator_index_48 / index_value_in_2023)

joined_df <- select(joined_df, -gdp_deflator_index_48)

# multiply the public sector finances and capital account balance by this.
# before: pub_sec_expenditure_m is 4655 in 1949.
joined_df <- joined_df %>%
  mutate(across(.cols = c(capital_account_balance_cp,
                          pub_sec_net_debt_m,
                          pub_sec_deficit_m,
                          pub_sec_receipts_m,
                          pub_sec_expenditure_m),
                ~ (.x / gdp_deflator_index) * 100
  ))%>%
  rename(capital_account_balance = capital_account_balance_cp)

# save as a csv
readr::write_csv(x = joined_df, file = "data/macroeconomic_variables.csv")

#in future start from here by reading in the .csv file
macroeconomic_variables_df <- readr::read_csv("data/macroeconomic_variables.csv")|>
    tidyr::pivot_longer(cols = -year, names_to = "variable", values_to = "value")|>
  filter(!is.na(value))

# add columns for the labels and data sources that will be in the plots
macroeconomic_variables_df <- macroeconomic_variables_df |>
    mutate(title_i = case_when(variable == "interest_rate" ~ "Interest rate",
                               variable == "cpi" ~ "CPI",
                                            .default = NA),
         subtitle_i = case_when(variable == "interest_rate" ~ "yearly average of official Bank rate",
                                variable == "cpi" ~ "annual growth rate of the consumer price index",
                                .default = NA),
         var_units = case_when(variable == "interest_rate" ~ "%",
                               variable ==  "cpi" ~ "%",
                               .default = NA),
         var_source = case_when(variable == "interest_rate" ~ "Bank of England",
                               variable == "cpi" ~ "ONS",
                               .default = NA),
         var_url = case_when(variable == "interest_rate" ~ "url",
                             variable == "cpi" ~ "url",
                             .default = NA)
         )

# create a function for a dynamic plot
macro_vars_plot_function<- function(variable_x, 
                                            recessions = FALSE,
                                            plot_title = paste(unique(macroeconomic_variables_df$title_i[macroeconomic_variables_df$variable == variable_x]), collapse = "; "),
                                            plot_subtitle = paste(unique(macroeconomic_variables_df$subtitle_i[macroeconomic_variables_df$variable == variable_x]), collapse = "; "),
                                            plot_y_label = paste(unique(macroeconomic_variables_df$var_units[macroeconomic_variables_df$variable == variable_x]), collapse = "; "),
                                            plot_caption = paste(unique(macroeconomic_variables_df$var_source[macroeconomic_variables_df$variable == variable_x]), collapse = "; ")
){

annotate_recession_function <- function(start_year, end_year){
  annotate("rect", 
           xmin = start_year, 
           xmax = end_year, 
           ymin = 0, 
           ymax = max(macroeconomic_variables_df$value[macroeconomic_variables_df$variable == variable_x]),
           fill = "#F46A25",
           alpha = .2)
}

macroeconomic_variables_df |>
  filter(variable == variable_x)|>
ggplot(mapping = aes(x = year, y = value))+
  scale_colour_discrete_af()+
  geom_line(linewidth = 1, aes(colour = variable), show.legend = (length(unique(variable_x)) >1))+
    {if(recessions)annotate_recession_function(1975.25, 1975.75)}+
    {if(recessions)annotate_recession_function(1980, 1981.25)}+
    {if(recessions)annotate_recession_function(1990.75, 1993.25)}+
    {if(recessions)annotate_recession_function(2008.25, 2009.5)}+
    {if(recessions)annotate_recession_function(2020, 2020.5)}+
  scale_x_continuous(breaks = seq(round(min(macroeconomic_variables_df$year), -1), max(macroeconomic_variables_df$year), by = 5))+
  scale_y_continuous(labels = scales::label_comma())+ #limits = c(0, NA),
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "year",
    y = plot_y_label,
    caption = stringr::str_c("Source: ", plot_caption, if_else(recessions, ". Shaded areas indicate economic recessions.", ""), sep = "")
  )+  
  theme_af()
}

macro_vars_plot_function(variable_x = c("gdp_deflator_growth", "cpi", "interest_rate", "unemployment_rate"),  recessions = TRUE)

  
        