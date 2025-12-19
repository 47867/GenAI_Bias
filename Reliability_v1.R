library(tidycomm)
library(tibble)

setwd("C:/Users/Simon/Documents/02_Master_Computational_Social_Science/GenAI_Bias/Daten/Codepläne/Github files")

data_Simon <- read.csv2("Codeplan_Reliabilität_Simon_fertig.csv")
data_Sarah <- read.csv2("Codeplan_Reliabilität_Sarah_fertig.csv")
data_Emma <- read.csv2("Codeplan_Reliabilität_Emma_fertig.csv")
data_Annalena <- read.csv2("Codeplan_Reliabilität_AnnaLena_fertig.csv")

tibble_Simon <- as_tibble(data_Simon)

df_all <- rbind(data_Simon, data_Annalena, data_Sarah, data_Emma)

reliability <- df_all %>% 
  test_icr(id, coder_id, na.omit = TRUE)

write.csv2(reliability, file = "reliability_v1.csv")


