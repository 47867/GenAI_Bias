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

#deactivate scientific notation for faster interpretation
options(scipen = 999)

#read data with different format
#new format contains all entries in one column instead of two.
#For example race1 and race2 are now in one column and so on.
#We have an variable called "left_right" so we can still identify each entry.

data_rearranged <- read.csv2("Dataset_rearranged.csv")
View(data_rearranged)
data_tibble_re <- as_tibble(data_rearranged)


#drop empty columns
data_tibble_re <- data_tibble_re %>%
  select(
    -c(X, race2, gender2, large2, position2, machtposition2)
  )

#rename other columns to avoid confusion
data_tibble_re <- data_tibble_re %>% 
  rename(
    race = race1,
    gender = gender1,
    large = large1,
    position = position1,
    machtposition = machtposition1
  )

#dichotomise data for large and position
data_tibble_re <- data_tibble_re %>% 
  mutate(
    large_rec = case_when(
      large == 1 ~ 1,
      large == 2 ~ 2,
      large == 3 ~ 2
    ),
    position_rec = case_when(
      position == 1 ~ 1,
      position == 2 ~ 2,
      position == 3 ~ 2
    )   
  )

#drop residual category for gender and race
data_tibble_re <- data_tibble_re %>% 
  filter(
    race != 3,
    gender != 3
  )


#######################
#### descriptives #####
#######################

#group by version
data_tibble_re %>% tab_frequencies(version, race)
data_tibble_re %>% tab_frequencies(version, large_rec)
data_tibble_re %>% tab_frequencies(version, gender)
data_tibble_re %>% tab_frequencies(version, position_rec)
data_tibble_re %>% tab_frequencies(version, machtposition)

#group bvy version and left/right
data_tibble_re %>% tab_frequencies(version, left_right, race)
data_tibble_re %>% tab_frequencies(version, left_right, large_rec)
data_tibble_re %>% tab_frequencies(version, left_right, gender)
data_tibble_re %>% tab_frequencies(version, left_right, position_rec)
data_tibble_re %>% tab_frequencies(version, left_right, machtposition)



###############################
#### Hypotheses Model v1.5 ####
###############################

data_tibble_re_v1 <- data_tibble_re %>% 
  filter(version == "1_5")

#H1a - v1.5 - non-significant
data_tibble_re_v1 %>% crosstab(
  machtposition,
  large_rec,
  chi_square = TRUE
)

#H1b - v1.5 - non-significant
data_tibble_re_v1 %>% crosstab(
  machtposition,
  position_rec,
  chi_square = TRUE
)

#H1c - v1.5
#unable to collect data due to poor reliability

#H2a - v1.5 - significant
data_tibble_re_v1 %>% crosstab(
  gender,
  large_rec,
  chi_square = TRUE
)

#H2b - v1.5 - non-significant
data_tibble_re_v1 %>% crosstab(
  gender,
  position_rec,
  chi_square = TRUE
)

#H2c - v1.5
#unable to collect data due to poor reliability

#H3a - v1.5 - non-significant
data_tibble_re_v1 %>% crosstab(
  race,
  large_rec,
  chi_square = TRUE
)

#H3b - v1.5 - non-significant
data_tibble_re_v1 %>% crosstab(
  race,
  position_rec,
  chi_square = TRUE
)

#H3c - v1.5
#unable to collect data due to poor reliability

#H4a - v1.5 - non-significant
data_tibble_re_v1 %>% crosstab(
  machtposition,
  gender,
  chi_square = TRUE
)

#H4b - v1.5 - non-significant
data_tibble_re_v1 %>% crosstab(
  machtposition,
  race,
  chi_square = TRUE
)

##############################################################################

###############################
#### Hypotheses Model v2.0 ####
###############################

data_tibble_re_v2 <- data_tibble_re %>% 
  filter(version == "2_0")

#H1a - v2.0 - non-significant
data_tibble_re_v2 %>% crosstab(
  machtposition,
  large_rec,
  chi_square = TRUE
)

#H1b - v2.0 - non-significant
data_tibble_re_v2 %>% crosstab(
  machtposition,
  position_rec,
  chi_square = TRUE
)

#H1c - v2.0
#unable to collect data due to poor reliability

#H2a - v2.0 - significant
data_tibble_re_v2 %>% crosstab(
  gender,
  large_rec,
  chi_square = TRUE
)

#H2b - v2.0 - non-significant
data_tibble_re_v2 %>% crosstab(
  gender,
  position_rec,
  chi_square = TRUE
)

#H2c - v2.0
#unable to collect data due to poor reliability

#H3a - v2.0 - non-significant
data_tibble_re_v2 %>% crosstab(
  race,
  large_rec,
  chi_square = TRUE
)

#H3b - v2.0 - non-significant
data_tibble_re_v2 %>% crosstab(
  race,
  position_rec,
  chi_square = TRUE
)

#H3c - v2.0
#unable to collect data due to poor reliability

#H4a - v2.0 - non-significant
data_tibble_re_v2 %>% crosstab(
  machtposition,
  gender,
  chi_square = TRUE
)

#H4b - v2.0 - non-significant
data_tibble_re_v2 %>% crosstab(
  machtposition,
  race,
  chi_square = TRUE
)


##############################################################################

###############################
#### Hypotheses Model v3.5 ####
###############################

data_tibble_re_v3 <- data_tibble_re %>% 
  filter(version == "3_5")

#H1a -v3.5 - significant
data_tibble_re_v3 %>% crosstab(
  machtposition,
  large_rec,
  chi_square = TRUE
)

#H1b -v3.5 - non-significant
data_tibble_re_v3 %>% crosstab(
  machtposition,
  position_rec,
  chi_square = TRUE
)

#H1c -v3.5
#unable to collect data due to poor reliability

#H2a -v3.5 - significant
data_tibble_re_v3 %>% crosstab(
  gender,
  large_rec,
  chi_square = TRUE
)

#H2b -v3.5 - significant
data_tibble_re_v3 %>% crosstab(
  gender,
  position_rec,
  chi_square = TRUE
)

#H2c -v3.5
#unable to collect data due to poor reliability

#H3a -v3.5 - non-significant
data_tibble_re_v3 %>% crosstab(
  race,
  large_rec,
  chi_square = TRUE
)

#H3b -v3.5 - non-significant
data_tibble_re_v3 %>% crosstab(
  race,
  position_rec,
  chi_square = TRUE
)

#H3c -v3.5
#unable to collect data due to poor reliability

#H4a -v3.5 - significant
data_tibble_re_v3 %>% crosstab(
  machtposition,
  gender,
  chi_square = TRUE
)

#H4b -v3.5 - non-significant
data_tibble_re_v3 %>% crosstab(
  machtposition,
  race,
  chi_square = TRUE
)

##########################################
#### Post-hoc tests (where necessary) ####
##########################################

#H1a -v3.5 - significant
crosstabs_3 <- data_tibble_re_v3 %>% crosstab(
  machtposition,
  large_rec,
  chi_square = TRUE
)

crosstabs_3 <- crosstabs_3 %>% 
  select(
    -1
  )

head(crosstabs_3)

chisq.posthoc.test(crosstabs_3, method = "bonferroni", round = 12)



#H4a -v3.5 - significant
crosstabs_6 <- data_tibble_re_v3 %>% crosstab(
  machtposition,
  gender,
  chi_square = TRUE
)

crosstabs_6 <- crosstabs_6 %>% 
  select(
    -1
  )

head(crosstabs_6)

chisq.posthoc.test(crosstabs_6, method = "bonferroni", round = 12)

