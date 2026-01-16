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
#data_tibble %>% tab_frequencies(version, race1)
#data_tibble %>% tab_frequencies(version, race2)

#data_tibble %>% tab_frequencies(version, large1)
#data_tibble %>% tab_frequencies(version, large2)

#data_tibble %>% tab_frequencies(version, gender1)
#data_tibble %>% tab_frequencies(version, gender2)

#data_tibble %>% tab_frequencies(version, machtposition1)
#data_tibble %>% tab_frequencies(version, machtposition2)

#data_tibble %>% tab_frequencies(version)

  
data_rearranged <- read.csv2("Dataset_rearranged.csv")
View(data_rearranged)
data_tibble_re <- as_tibble(data_rearranged)

data_tibble_re %>% tab_frequencies(version, race1)
data_tibble_re %>% tab_frequencies(version, large1)
data_tibble_re %>% tab_frequencies(version, gender1)
data_tibble_re %>% tab_frequencies(version, position1)
data_tibble_re %>% tab_frequencies(version, machtposition1)

#####what descriptives do we actually need?


#H1a
crosstabs_1 <-data_tibble_re %>% crosstab(
  machtposition1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)
chisq.posthoc.test(crosstabs_1, method = "bonferroni")

#H1b
data_tibble_re %>% crosstab(
  machtposition1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#H2a
data_tibble_re_fil <- data_tibble_re %>% 
  filter(gender1!=3)
data_tibble_re_fil <- data_tibble_re_fil %>% 
  filter(race1!=3)

crosstabs_2 <- data_tibble_re_fil %>% crosstab(
  gender1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_2, method = "bonferroni", round = 12)

#H2b
crosstabs_3 <- data_tibble_re_fil %>% crosstab(
  gender1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_3, method = "bonferroni", round = 12)

#H3a
data_tibble_re_fil %>% crosstab(
  race1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#H3b
data_tibble_re_fil %>% crosstab(
  race1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#H4a
crosstabs_4 <- data_tibble_re_fil %>% crosstab(
  machtposition1,
  gender1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_4, method = "bonferroni", round = 12)

#H4b
data_tibble_re_fil %>% crosstab(
  machtposition1,
  race1,
  chi_square = TRUE,
  add_total = TRUE
)









#####continue here with crosstabs overall


#crosstabs per model -> v1.5
data_filtered_v1 <- data_tibble %>% 
  filter(version == "1_5")

#h1a (v1.5)
data_filtered_v1 %>% crosstab(
  machtposition1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v1.5)
data_filtered_v1 %>% crosstab(
  machtposition1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v1.5)
data_filtered_v1_fil <- data_filtered_v1 %>% 
  filter(gender1!=3) %>% 
  filter(race1!=3)

crosstabs_5 <- data_filtered_v1_fil %>% crosstab(
  gender1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_5, method = "bonferroni", round = 12)

#h2b (v1.5)
data_filtered_v1_fil %>% crosstab(
  gender1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v1.5)
data_filtered_v1_fil %>% crosstab(
  race1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v1.5)
data_filtered_v1_fil %>% crosstab(
  race1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v1.5)
data_filtered_v1_fil %>% crosstab(
  gender1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)

#h4b (v1.5)
data_filtered_v1_fil %>% crosstab(
  race1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)


#crosstabs per model -> v2.0
data_filtered_v2 <- data_tibble %>% 
  filter(version == "2_0")

data_filtered_v2_fil <- data_filtered_v2 %>% 
  filter(gender1!=3) %>% 
  filter(race1!=3)

#h1a (v2.0)
data_filtered_v2 %>% crosstab(
  machtposition1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v2.0)
data_filtered_v2 %>% crosstab(
  machtposition1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v2.0)

crosstabs_6 <- data_filtered_v2_fil %>% crosstab(
  gender1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_6, method = "bonferroni", round = 12)

#h2b (v2.0)
data_filtered_v2_fil %>% crosstab(
  gender1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v2.0)
data_filtered_v2_fil %>% crosstab(
  race1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v2.0)
data_filtered_v2_fil %>% crosstab(
  race1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v2.0)
data_filtered_v2_fil %>% crosstab(
  gender1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)

#h4b (v2.0)
data_filtered_v2_fil %>% crosstab(
  race1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)



#crosstabs per model -> v3.5
data_filtered_v3 <- data_tibble %>% 
  filter(version == "3_5")

data_filtered_v3_fil <- data_filtered_v3 %>% 
  filter(gender1!=3) %>% 
  filter(race1!=3)

#h1a (v3.5)
crosstabs_7 <- data_filtered_v3 %>% crosstab(
  machtposition1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_7, method = "bonferroni", round = 12)

#h1b (v3.5)
data_filtered_v3 %>% crosstab(
  machtposition1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v3.5)

crosstabs_8 <- data_filtered_v3_fil %>% crosstab(
  gender1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_8, method = "bonferroni", round = 12)

#h2b (v3.5)
crosstabs_9 <- data_filtered_v3_fil %>% crosstab(
  gender1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_9, method = "bonferroni", round = 12)

#h3a (v3.5)
data_filtered_v3_fil %>% crosstab(
  race1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v3.5)
data_filtered_v3_fil %>% crosstab(
  race1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v3.5)
crosstabs_10 <- data_filtered_v3_fil %>% crosstab(
  gender1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_10, method = "bonferroni", round = 12)

#h4b (v3.5)
data_filtered_v3_fil %>% crosstab(
  race1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)













