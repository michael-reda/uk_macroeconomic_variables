import pandas as pd
%pwd
boe_df = pd.read_csv("dbfs/mnt/lab/unrestricted/michael.reda@defra.gov.uk/BoE_economic_data/BoE_bank_rate_exchgEUR-STERL_USD-STERL.csv")
boe_df.head()
boe_df.columns

boe_df.rename(columns={"Monthly average of official Bank Rate              [a] [b]             IUMABEDR": "bank_rate",
                       "Monthly average Spot exchange rate, Euro into Sterling              [c]             XUMAERS":"euro_to_sterling_exchg",
                       "Monthly average Spot exchange rate, US$ into Sterling              [c]             XUMAUSS":"usd_to_sterling_exchg"}, inplace=True)
					   
boe_df.dtypes
