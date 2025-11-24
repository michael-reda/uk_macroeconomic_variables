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

boe_exchg_interest_df <- mutate(boe_exchg_interest_df, year = lubridate::year(date))

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
         median_household_income = `Median income`
  )

# ONS public sector finance data
# https://www.ons.gov.uk/economy/governmentpublicsectorandtaxes/publicsectorfinance/bulletins/publicsectorfinances/september2025
# quarterly_public_sector_net_borrowing : J5II
# quarterly_public_sector_spending : KX5Q
# quarterly_public_sector_receipts : JW2O
# public_sector_net_debt £bn: HF6W
# public_sector_net_debt as % of GDP: HF6X
public_sector_finances_df <- readxl::read_xlsx("data/ons_public_sector_finances_quarterly.xlsx")

public_sector_finances_subset_df <- public_sector_finances_df[public_sector_finances_df[1, ]%in% c("CDID", "J5II", "KX5Q", "JW2O", "HF6W", "HF6X")]

# rename the columns, subset the rows


# ONS gross fixed capital formation
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/grossfixedcapitalformationbysectorandasset
# sheet = "G1_CVM_SA_Q_levels" array = "a6:m121"
gfcf_df <- readxl::read_xlsx("data/ons_grossfixedcapitalformationbysectorandasset.xlsx", sheet = "G1_CVM_SA_Q_levels", range = "a6:m121")


# ONS UK housebuilding
# https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/ukhousebuildingpermanentdwellingsstartedandcompleted
 #"1a" "6a:195j"
housebuilding_df <- readxl::read_xlsx(path = "data/ons_ukhousebuilding.xlsx", sheet = "1a", range = "b6:j195")


# ONS Capital Account: Balance: CP NSA
# https://www.ons.gov.uk/economy/nationalaccounts/balanceofpayments/timeseries/fkmj/pnbp
capital_account_balance_df <- readr::read_csv(file = "data/ons_capital_account.csv")


# ONS trade stats, 1997 - 2025, Chain Volume Measured, EU, non-EU, total, goods, services, imports, exports
# https://www.ons.gov.uk/economy/nationalaccounts/balanceofpayments/datasets/tradeingoodsmretsallbopeu2013timeseriesspreadsheet
# row 817 - 1161
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
  slice(817:1161)


# ONS quarterly GDP in chained volume measures (2023 prices)
# sheet= "2018 -" array= "BH165:B286"
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/realtimedatabaseforukgdpabmi
gdp_df <- readxl::read_xlsx("data/ons_uk_quarterly_GDP_real.xlsx", sheet = "2018 - ", range = "A4:BH286")%>%
  select(1, 60)

# ONS CPI
# https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindices
cpi_df <- readr::read_csv("data/ons_uk_cpi_95_25.csv")

# ONS Unemployment rate (aged 16 and over, seasonally adjusted): %
# https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms
unemployment_df <- readr::read_csv("data/ons_uk_unemployment_95_25.csv")

# ONS population
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/ukpop/pop
population_df <- readr::read_csv("data/ons_uk_population_95_25.csv")


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

# Life expectancy, Our World in Data
# Data source: Riley (2005); Zijdeman et al. (2015); HMD (2025); UN WPP (2024) – Learn more about this data
# OurWorldinData.org/life-expectancy | CC BY
# https://ourworldindata.org/life-expectancy
life_expectancy <- readr::read_csv("data/owid_life-expectancy_uk_95_23.csv")
# need to add 2022: 2.69, and 2023: 2.64 (source: 2023 edition of https://www.ons.gov.uk/economy/governmentpublicsectorandtaxes/researchanddevelopmentexpenditure )

