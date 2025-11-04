install.packages("tidyverse")
library(tidyverse)

# Bank of England data: interest rates and exchange rates
# https://www.bankofengland.co.uk/boeapps/database/default.asp
# IUMABEDR Monthly average of official Bank Rate
# XUMABK67 Monthly average Effective exchange rate index, Sterling (Jan 2005 = 100)
# XUMAGBD Monthly average Spot exchange rate, Sterling into US$
# XUMASER Monthly average Spot exchange rates, Sterling into Euro

# ONS public sector finance data:
# quarterly_public_sector_net_borrowing : J5II
# quarterly_public_sector_spending : KX5Q
# quarterly_public_sector_receipts : JW2O
# public_sector_net_debt : HF6XX

boe_data <- read.csv("data/boe_df.csv")

