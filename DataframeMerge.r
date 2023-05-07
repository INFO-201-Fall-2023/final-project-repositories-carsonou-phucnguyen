library(tidyverse)

df_1 <- read.csv("Alternative_Fueling_Stations.csv")
df_2 <- read.csv("Electric_Vehicle_Population_Data.csv")

cs_df <- df_1
ev_df <- df_2

cs_df <- select(cs_df, city, state, zip)
cs_df <- filter(cs_df, state == "WA")
cs_df <- data.frame(table(cs_df$zip))
colnames(cs_df)[1] = "Zip_Code"
colnames(cs_df)[2] = "Total_Charging_Stations"

ev_df <- select(ev_df, City, State, Postal.Code)
ev_df <- filter(ev_df, State == "WA")
ev_df <- data.frame(table(ev_df$Postal.Code))
colnames(ev_df)[1] = "Zip_Code"
colnames(ev_df)[2] = "Total_Electric_Vehicles"

total_df <- merge(ev_df, cs_df, by = "Zip_Code", all.x = TRUE)
total_df[is.na(total_df)] = 0
total_df <-
  mutate(total_df, CS_to_EV_Ratio_Percent = 
           round(Total_Charging_Stations / Total_Electric_Vehicles * 100, digits = 2))
total_df <-
  mutate(total_df, Does_CS_Exist =
           Total_Charging_Stations != 0)
write.csv(total_df, 
          "C:\\Users\\carso\\OneDrive\\Documents\\UW\\INFO_201\\MergedDataframe.csv", 
          row.names = TRUE)