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




