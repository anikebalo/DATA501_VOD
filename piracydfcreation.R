install.packages("haven")
library(haven)
library(dplyr)
library(ggplot2)

piracy.raw = read_sav("PiracySVOD.sav")

piracy.df <- piracy.raw %>%
  select(-4,-15:-19,-75)

piracy.df <- piracy.df %>%
  select((-(14:28)))

piracy.df <- piracy.df %>%
  select((-(30:41)))

piracy.df$Interest_Thriller <- piracy_allfactors$Interest_Thriller

for(i in 1:nrow(piracy.df)){
  if (is.na(piracy.df$Interest_Thiller[i])){
    piracy.df$Interest_Thiller[i] <- 0
  }
}


piracy.df

piracy.df <- piracy.df %>% 
  rename(
    Sex = Gender,
    Avg_Advertising_Attitude = Advertising_attitude,
    Avg_Loyalty_Attitude = Loyalty_attitude,
    Avg_Justice_Sensibility = Justice_Sensibility,
    Avg_Fear_Punishment = Fear_punishment,
    Loyalty_attitude01 = Permanence_attitude01,
    Loyalty_attitude02 = Permanence_attitude02,
    Loyalty_attitude03 = Permanence_attitude03,
    Loyalty_attitude04 = Permanence_attitude04,
    Interest_Thriller = Interest_Thiller
  )

piracy.df <- piracy.df %>%
  select(-27)