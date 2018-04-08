
install.packages("hflights")
library(hflights)
library(dplyr)
head(hflights)

hflights_df <- tbl_df(hflights)
class(hflights_df)
hflights_df

f_df <- filter(hflights_df, Month == 1, UniqueCarrier == "AA")
f_df
class(f_df)

filter(hflights_df, UniqueCarrier == "AA" | UniqueCarrier == "UA")

arrange(hflights_df, Month, DayofMonth, desc(AirTime))

mutate(hflights_df, gain = ArrDelay - DepDelay, gain_per_hour = gain / (AirTime / 60))

summarise(hflights_df, delay = mean(ArrDelay, na.rm = T))

per_month <- summarize(hflights_df, number_flights = sum(number_flights))
per_month