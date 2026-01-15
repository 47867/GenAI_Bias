#this script is written in R4.5.2
#tidyverse version: 2.0.0
#tidycomm version: 0.4.2
#tibble version: 3.3.1
#chisq.posthoc.test version: 0.1.2


#Results

#load packages
library(tidyverse)
library(tidycomm)
library(tibble)
library(chisq.posthoc.test)


#read data
data <- read.csv2("Codeplan_Gesamt_cleaned.csv")
head(data)

data_tibble <- as_tibble(data)

#descriptives
data_tibble %>% tab_frequencies(version, profession)

#####what descriptives do we actually need?


#crosstabs overall
data_tibble %>% crosstab(
  machtposition1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#####continue here with crosstabs overall


#crosstabs per model -> v1.5
data_filtered_v1 <- data_tibble %>% 
  filter(version == "1_5")

data_filtered_v1 %>% crosstab(
  machtposition1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#####continue here with crosstabs per model



#post-hoc, if necessary:
#chisq.posthoc.test(data1, method = "bonferroni")

