#install.packages("tidyverse")
library(tidyverse)

# Bank of England data: interest rates and exchange rates
# https://www.bankofengland.co.uk/boeapps/database/default.asp
# IUMABEDR Monthly average of official Bank Rate
# XUMABK67 Monthly average Effective exchange rate index, Sterling (Jan 2005 = 100)
# XUMAGBD Monthly average Spot exchange rate, Sterling into US$
# XUMASER Monthly average Spot exchange rates, Sterling into Euro
boe_exchg_interest_df <- read.csv("data/boe_df.csv")


# Institute for Fiscal Studies living standards, poverty and inequality data
# https://ifs.org.uk/living-standards-poverty-and-inequality-uk
# net [of taxes and inclusive of benefits] equivalised household income, before housing costs, inflation adjusted.
ifs_income_bhc_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Income (BHC)", range = "a3:f66") #mean weekly: col D; median weekly: col F
ifs_inequality_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Inequality", range = "a3:d66") #gini coefficient: col D
ifs_poverty_bhc_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Poverty (BHC)", range = "a3:m66")
ifs_child_poverty_bhc_df <- readxl::read_xlsx("data/ifs_poverty_inequality.xlsx", sheet = "Child Poverty (BHC)", range = "a3:m66")
# use col M: absolute poverty: proportion below 60% of the inflation adjusted 2010 median income. This is the official absolute poverty rate.


# ONS public sector finance data
# https://www.ons.gov.uk/economy/governmentpublicsectorandtaxes/publicsectorfinance/bulletins/publicsectorfinances/september2025
# quarterly_public_sector_net_borrowing : J5II
# quarterly_public_sector_spending : KX5Q
# quarterly_public_sector_receipts : JW2O
# public_sector_net_debt : HF6XX
public_sector_finances_df <- readxl::read_xlsx("data/ons_public_sector_finances_quarterly.xlsx")

# ONS gross fixed capital formation
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/grossfixedcapitalformationbysectorandasset
# sheet = "G1_CVM_SA_Q_levels" array = "a6:m121"
gfcf_df <- readxl::read_xlsx("data/ons_grossfixedcapitalformationbysectorandasset.xlsx", sheet = "G1_CVM_SA_Q_levels", range = "a6:m121")

# ONS UK housebuilding
# https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/ukhousebuildingpermanentdwellingsstartedandcompleted
 #"1a" "6a:195j"

# ONS Capital Account: Balance: CP NSA
# https://www.ons.gov.uk/economy/nationalaccounts/balanceofpayments/timeseries/fkmj/pnbp

# ONS trade stats
# https://www.ons.gov.uk/economy/nationalaccounts/balanceofpayments/datasets/tradeingoodsmretsallbopeu2013timeseriesspreadsheet

# ONS quarterly GDP in chained volume measures (2023 prices)
# sheet= "2018 -" array= "BH165:B286"
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/realtimedatabaseforukgdpabmi

# ONS CPI
# https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindices

# ONS Unemployment rate (aged 16 and over, seasonally adjusted): %
# https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms

# ONS population
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/ukpop/pop

# trade intensity (% of GDP), World Bank
# https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS

# Research and development expenditure (% of GDP), World Bank
# https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS

# Manufacturing value added (% of GDP), World Bank
# https://data.worldbank.org/indicator/NV.IND.MANF.ZS?end=2024&start=1960&view=chart


# Life expectancy, Our World in Data
# Data source: Riley (2005); Zijdeman et al. (2015); HMD (2025); UN WPP (2024) – Learn more about this data
# OurWorldinData.org/life-expectancy | CC BY
# https://ourworldindata.org/life-expectancy


