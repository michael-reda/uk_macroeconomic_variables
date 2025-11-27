#install.packages("tidyverse")
library(tidyverse)

## read in data

# Bank of England data: interest rates and exchange rates
# https://www.bankofengland.co.uk/boeapps/database/default.asp
# IUMABEDR Monthly average of official Bank Rate
# XUMABK67 Monthly average Effective exchange rate index, Sterling (Jan 2005 = 100)
# XUMAGBD Monthly average Spot exchange rate, Sterling into US$
# XUMASER Monthly average Spot exchange rates, Sterling into Euro
boe_exchg_interest_df <- read.csv("data/boe_df.csv")

boe_exchg_interest_df <- boe_exchg_interest_df %>%
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

boe_annual_df <- boe_exchg_interest_df %>%
  group_by(year)%>%
  summarise(across(2:5, mean, na.rm = TRUE))

rm(boe_exchg_interest_df)

# Institute for Fiscal Studies living standards, poverty and inequality data
# https://ifs.org.uk/living-standards-poverty-and-inequality-uk
# net [of taxes and inclusive of benefits] equivalised household income, before housing costs, inflation adjusted.
ifs_income_bhc_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Income (BHC)", range = "a3:f66")%>%
  select(2, 4, 6)
    
    #mean weekly: col D; median weekly: col F
ifs_inequality_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Inequality", range = "a3:d66")%>%
  select(2,4)#gini coefficient: col D

# use col M: absolute poverty: proportion below 60% of the inflation adjusted 2010 median income. This is the official absolute poverty rate.
ifs_poverty_bhc_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Poverty (BHC)", range = "a3:m66")%>%
  select(2, 13)%>%
  rename(poverty_rate = `60pc...13`)
  
ifs_child_poverty_bhc_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Child Poverty (BHC)", range = "a3:m66")%>%
  select(2, 13)%>%
  rename(child_poverty_rate = `60pc...13`)

ifs_df <- dplyr::left_join(ifs_income_bhc_df, ifs_inequality_df, by = "Year")%>%
  left_join(., ifs_poverty_bhc_df, by = "Year")%>%
  left_join(., ifs_child_poverty_bhc_df, by = "Year")
  
# for the joined dataframe

ifs_df <- ifs_df %>% mutate(Year = stringr::str_sub(Year, 1, 4))%>%
  rename(year = Year,
         mean_household_income = `Mean income`,
         median_household_income = `Median income`,
         gini_coefficient = `Gini coefficient`
  )%>%
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
                                             "pub_sec_net_borrowing_m",
                                             "pub_sec_receipts_m",
                                             "pub_sec_expenditure_m")


public_sector_finances_df <- public_sector_finances_df %>%
  slice(15:93)

public_sector_finances_df[] <- lapply(public_sector_finances_df[], as.numeric)

public_sector_finances_df$pub_sec_net_debt_m <- public_sector_finances_df$pub_sec_net_debt_m * 1000

# ONS gross fixed capital formation
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/grossfixedcapitalformationbysectorandasset
# sheet = "G1_CVM_SA_Q_levels" array = "a6:m121"
gfcf_df <- readxl::read_xlsx("data/ons_grossfixedcapitalformationbysectorandasset.xlsx", sheet = "G1_CVM_SA_Q_levels", range = "a6:m121")%>%
  select(1, 13)%>%
  rename(date_qtrly = `Time period column and title row`,
         gfcf = `Total sector (S.1) Total asset (GFCF)`)%>%
  slice(2:n())%>%
  mutate(gfcf = as.numeric(gfcf),
         date_qtrly = lubridate::yq(date_qtrly),
         year = lubridate::year(date_qtrly)
  )%>% 
  group_by(year)%>%
  summarise(gfcf = sum(gfcf))%>%
  ungroup()
  

# ONS UK housebuilding
# https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/ukhousebuildingpermanentdwellingsstartedandcompleted
 #"1a" "6a:195j"
housebuilding_df <- readxl::read_xlsx(path = "data/ons_ukhousebuilding.xlsx", sheet = "1a", range = "b6:j195")%>%
  select("Period", "Started - All Dwellings", "Completed - All Dwellings")%>%
  mutate(Period = lubridate::my(Period),
         year = lubridate::year(Period),
         `Started - All Dwellings` = as.numeric(`Started - All Dwellings`),
         `Completed - All Dwellings` = as.numeric(`Completed - All Dwellings`)
  )%>%
  group_by(year)%>%
  summarise(started_dwellings = sum(`Started - All Dwellings`),
            completed_dwellings = sum(`Completed - All Dwellings`)
            )%>%
  ungroup()


# ONS Capital Account: Balance: CP NSA
# https://www.ons.gov.uk/economy/nationalaccounts/balanceofpayments/timeseries/fkmj/pnbp
capital_account_balance_df <- readr::read_csv(file = "data/ons_capital_account.csv")%>%
  slice(22:86)%>%
  rename(year = Title,
          capital_account_balance_cp = `Capital Account: Balance: CP NSA`)%>%
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

trade_balance_df <- readxl::read_xlsx("data/ons_uk_trade_stats.xlsx", sheet = "data", range = "a1:aeu1161")%>%
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
           "Trade in Services (TS): WW: Balance: BOP: CVM: SA"))%>%
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
gdp_df <- readxl::read_xlsx("data/ons_uk_quarterly_GDP_real.xlsx", sheet = "2018 - ", range = "A4:BH286")%>%
  select(1, 60)%>%
  mutate(year = substr(`Publication date and time period`, 4, 7))%>%
  mutate(year = as.numeric(year))%>%
  select(year, gdp = `Sep-25 [2023 prices]\r\nQNA`)%>%
  group_by(year)%>%
  summarise(gdp = sum(gdp))%>%
  ungroup()%>%
  filter(year != 2025)


# ONS CPI
# https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindices
cpi_df <- readr::read_csv("data/ons_uk_cpi_89_25.csv")%>%
  slice(8:43)%>%
  mutate(year = as.numeric(Title),
         cpi = as.numeric(`CPI ANNUAL RATE 00: ALL ITEMS 2015=100`)
  )%>%
  select(year, cpi)

# GDP deflator
#https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/ihys/qna
gdp_deflator_df <- readr::read_csv("data/ons_gdp_deflator_yoy_growth.csv")%>%
  slice(8:83)%>%
  mutate(year = as.numeric(Title),
         gdp_deflator_growth = as.numeric(`GDP Deflator: Year on Year growth: SA %`)
  )%>%
  select(year, gdp_deflator_growth)


# ONS Unemployment rate (aged 16 and over, seasonally adjusted): %
# https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms
unemployment_df <- readr::read_csv("data/ons_uk_unemployment_71_24.csv")%>%
  slice(8:61)%>%
  mutate(year = as.numeric(Title),
         unemployment_rate = as.numeric(`Unemployment rate (aged 16 and over, seasonally adjusted): %`)
  )%>%
  select(year, unemployment_rate)

# ONS population
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/ukpop/pop
population_df <- readr::read_csv("data/ons_uk_population_71_24.csv")%>%
  slice(8:61)%>%
  mutate(year = as.numeric(Title),
         population = as.numeric(`United Kingdom population mid-year estimate`)
  )%>%
  select(year, population)


# trade intensity (% of GDP), World Bank
# https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS
trade_intensity_df <- readr::read_csv("data/wb_trade_intensity.csv", skip = 3)%>%
  filter(`Country Code` == "GBR")


# Research and development expenditure (% of GDP), World Bank
# https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS
randd_expenditure_df <- readr::read_csv("data/wb_gerd_annual.csv", skip = 3)%>%
  filter(`Country Code` == "GBR")

# Manufacturing value added (% of GDP), World Bank
# https://data.worldbank.org/indicator/NV.IND.MANF.ZS?end=2024&start=1960&view=chart
manuf_val_added_df <- readr::read_csv("data/wb_manuf_val_added.csv", skip = 3)%>%
  filter(`Country Code` == "GBR")

# Urban population (% of total population) - United Kingdom
urban_pop_pct_df <- readr::read_csv("data/wb_urban_pop_pct.csv", skip = 3)%>%
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
life_expectancy <- readr::read_csv("data/owid_life_expectancy_uk_61_23.csv")%>%
  select(year = Year,
         life_expectancy = `Period life expectancy at birth`)



# make a note of which variables are nominal, which need summarising by averaging or summing up to annual, which need dividing or multiplying by an index
# Boe: all indices or rates so all fine
# IFS: already inflation adjusted
# ONS public sector finances: all are nominal!
# GFCF is CVM (real) so all fine
# housebuilding: all fine
# capital account balance: current prices, need to adjust for inflation!
# all others are fine


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
            
# create the GDP deflator index, multiply the public sector finances and capital account balance by this
joined_df_test <- joined_df

#add a new variable: 1948 'gdp' of 100 grown annually using dplyr::lag() and the GDP deflator growth rate up to the present day.
# Then rebase to a recent year.

# save as a csv
readr::write_csv(x = joined_df, file = "data/macroeconomic_variables.csv")

#in future start from here by reading in the .csv file
macroeconomic_variables_df <- readr::read_csv("data/macroeconomic_variables.csv")

#begin creating the time series graphs