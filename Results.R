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


#read data with different format
  
data_rearranged <- read.csv2("Dataset_rearranged.csv")
View(data_rearranged)
data_tibble_re <- as_tibble(data_rearranged)


#descriptives für drei ausprägungen

#links
data_tibble_re %>% tab_frequencies(version, race1)
data_tibble_re %>% tab_frequencies(version, large1)
data_tibble_re %>% tab_frequencies(version, gender1)
data_tibble_re %>% tab_frequencies(version, position1)
data_tibble_re %>% tab_frequencies(version, machtposition1)



#rechts
data_tibble_re %>% tab_frequencies(version, race2)
data_tibble_re %>% tab_frequencies(version, large2)
data_tibble_re %>% tab_frequencies(version, gender2)
data_tibble_re %>% tab_frequencies(version, position2)
data_tibble_re %>% tab_frequencies(version, machtposition2)




#descriptives für dichotomisierte Variablen
data_tibble_re_dich <- data_tibble_re %>% 
  mutate(
    large1_rec = case_when(
      large1 == 1 ~ 1,
      large1 == 2 ~ 2,
      large1 == 3 ~ 2
    ),
    large2_rec = case_when(
      large2 == 1 ~ 1,
      large2 == 2 ~ 2,
      large2 == 3 ~ 2
    ),
    position1_rec = case_when(
      position1 == 1 ~ 1,
      position1 == 2 ~ 2,
      position1 == 3 ~ 2
    ),   
    position2_rec = case_when(
      position2 == 1 ~ 1,
      position2 == 2 ~ 2,
      position2 == 3 ~ 2
    ),      
  )


#links
data_tibble_re_dich %>% tab_frequencies(version, race1)
data_tibble_re_dich %>% tab_frequencies(version, large1_rec)
data_tibble_re_dich %>% tab_frequencies(version, gender1)
data_tibble_re_dich %>% tab_frequencies(version, position1_rec)
data_tibble_re_dich %>% tab_frequencies(version, machtposition1)

#rechts
data_tibble_re_dich %>% tab_frequencies(version, race2)
data_tibble_re_dich %>% tab_frequencies(version, large2_rec)
data_tibble_re_dich %>% tab_frequencies(version, gender2)
data_tibble_re_dich %>% tab_frequencies(version, position2_rec)
data_tibble_re_dich %>% tab_frequencies(version, machtposition2)

#####what descriptives do we actually need?







#####################################
#### Hypothesen für alle Modelle ####
#####################################

#########################################
#### Alle Modelle - dichotom - links ####
#########################################

data_tibble_re_dich <- data_tibble_re %>% 
  mutate(
    large1_rec = case_when(
      large1 == 1 ~ 1,
      large1 == 2 ~ 2,
      large1 == 3 ~ 2
    ),
    large2_rec = case_when(
      large2 == 1 ~ 1,
      large2 == 2 ~ 2,
      large2 == 3 ~ 2
    ),
    position1_rec = case_when(
      position1 == 1 ~ 1,
      position1 == 2 ~ 2,
      position1 == 3 ~ 2
    ),   
    position2_rec = case_when(
      position2 == 1 ~ 1,
      position2 == 2 ~ 2,
      position2 == 3 ~ 2
    ),      
  )

data_tibble_re_dich_fil <- data_tibble_re_dich %>% 
  filter(gender2!=3)
data_tibble_re_dich_fil <- data_tibble_re_dich_fil %>% 
  filter(race2!=3)

#H1a - links
data_tibble_re_dich %>% crosstab(
  machtposition2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#H1b - links
data_tibble_re_dich %>% crosstab(
  machtposition2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#H2a - links
data_tibble_re_dich_fil %>% crosstab(
  gender2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#H2b - links
data_tibble_re_dich_fil %>% crosstab(
  gender2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#H3a - links
data_tibble_re_dich_fil %>% crosstab(
  race2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#H3b - links
data_tibble_re_dich_fil %>% crosstab(
  race2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#H4a - links
data_tibble_re_dich_fil %>% crosstab(
  machtposition2,
  gender2,
  chi_square = TRUE,
  add_total = TRUE
)

#H4b - links
data_tibble_re_dich_fil %>% crosstab(
  machtposition2,
  race2,
  chi_square = TRUE,
  add_total = TRUE
)













##########################################
#### Alle Modelle - dichotom - rechts ####
##########################################

data_tibble_re_dich_fil_right <- data_tibble_re_dich %>% 
  filter(gender2!=3)
data_tibble_re_dich_fil_right <- data_tibble_re_dich_fil_right %>% 
  filter(race2!=3)

#H1a - rechts
data_tibble_re_dich %>% crosstab(
  machtposition2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#H1b - rechts
data_tibble_re_dich %>% crosstab(
  machtposition2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#H2a - rechts
data_tibble_re_dich_fil_right %>% crosstab(
  gender2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#H2b - rechts
data_tibble_re_dich_fil_right %>% crosstab(
  gender2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#H3a - rechts
data_tibble_re_dich_fil_right %>% crosstab(
  race2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#H3b - rechts
data_tibble_re_dich_fil_right %>% crosstab(
  race2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#H4a - rechts
data_tibble_re_dich_fil_right %>% crosstab(
  machtposition2,
  gender2,
  chi_square = TRUE,
  add_total = TRUE
)

#H4b - rechts
data_tibble_re_dich_fil_right %>% crosstab(
  machtposition2,
  race2,
  chi_square = TRUE,
  add_total = TRUE
)



















##############################
#### Alle Modelle - links ####
##############################

data_tibble_re_fil <- data_tibble_re %>% 
  filter(gender1!=3)
data_tibble_re_fil <- data_tibble_re_fil %>% 
  filter(race1!=3)

#H1a - links
crosstabs_1 <-data_tibble_re %>% crosstab(
  machtposition1,
  large1,
  chi_square = TRUE,
  #add_total = TRUE
)
crosstabs_1 <- crosstabs_1 %>% select(
  c(2, 3, 4)
)
chisq.posthoc.test(crosstabs_1, method = "bonferroni")

#H1b - links
data_tibble_re %>% crosstab(
  machtposition1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#H2a - links
crosstabs_2 <- data_tibble_re_fil %>% crosstab(
  gender1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_2, method = "bonferroni", round = 12)

#H2b - links
crosstabs_3 <- data_tibble_re_fil %>% crosstab(
  gender1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_3, method = "bonferroni", round = 12)

#H3a - links
data_tibble_re_fil %>% crosstab(
  race1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#H3b - links
data_tibble_re_fil %>% crosstab(
  race1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#H4a - links
crosstabs_4 <- data_tibble_re_fil %>% crosstab(
  machtposition1,
  gender1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_4, method = "bonferroni", round = 12)

#H4b - links
data_tibble_re_fil %>% crosstab(
  machtposition1,
  race1,
  chi_square = TRUE,
  add_total = TRUE
)


##############################
#### Alle Modelle - rechts ####
##############################

data_tibble_re_fil_right <- data_tibble_re %>% 
  filter(gender2!=3)
data_tibble_re_fil_right <- data_tibble_re_fil_right %>% 
  filter(race2!=3)

#H1a - rechts
data_tibble_re %>% crosstab(
  machtposition2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#H1b - rechts
data_tibble_re %>% crosstab(
  machtposition2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#H2a - rechts
data_tibble_re_fil_right %>% crosstab(
  gender2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#H2b - rechts
data_tibble_re_fil_right %>% crosstab(
  gender2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#H3a - rechts
data_tibble_re_fil_right %>% crosstab(
  race2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#H3b - rechts
data_tibble_re_fil_right %>% crosstab(
  race2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#H4a - rechts
data_tibble_re_fil_right %>% crosstab(
  machtposition2,
  gender2,
  chi_square = TRUE,
  add_total = TRUE
)

#H4b - rechts
data_tibble_re_fil_right %>% crosstab(
  machtposition2,
  race2,
  chi_square = TRUE,
  add_total = TRUE
)








#####################################
#### crosstabs per model -> v1.5 ####
#####################################

##################################
#### Variablen dichotomisiert ####
##################################

data_filtered_v1 <- data_tibble %>% 
  filter(version == "1_5")

data_filtered_v1_dich <- data_filtered_v1 %>% 
  mutate(
    large1_rec = case_when(
      large1 == 1 ~ 1,
      large1 == 2 ~ 2,
      large1 == 3 ~ 2
    ),
    large2_rec = case_when(
      large2 == 1 ~ 1,
      large2 == 2 ~ 2,
      large2 == 3 ~ 2
    ),
    position1_rec = case_when(
      position1 == 1 ~ 1,
      position1 == 2 ~ 2,
      position1 == 3 ~ 2
    ),   
    position2_rec = case_when(
      position2 == 1 ~ 1,
      position2 == 2 ~ 2,
      position2 == 3 ~ 2
    ),      
  )

##########################################
#### Variablen dichotomisiert - links ####
##########################################

data_filtered_v1_dich_fil <- data_filtered_v1_dich %>% 
  filter(gender1!=3) %>% 
  filter(race1!=3) #Ausschluss Ausweichkategorien für Geschlecht und Hautton

#h1a (v1.5) - dichotom - links
data_filtered_v1_dich %>% crosstab(
  machtposition1,
  large1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v1.5) - dichotom - links
data_filtered_v1_dich %>% crosstab(
  machtposition1,
  position1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v1.5) - dichotom - links
data_filtered_v1_dich_fil %>% crosstab(
  gender1,
  large1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2b (v1.5) - dichotom - links
data_filtered_v1_dich_fil %>% crosstab(
  gender1,
  position1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v1.5) - dichotom - links
data_filtered_v1_dich_fil %>% crosstab(
  race1,
  large1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v1.5) - dichotom - links
data_filtered_v1_dich_fil %>% crosstab(
  race1,
  position1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v1.5) -> keine Dichotomisierung nötig -> siehe unten


#h4b (v1.5) -> keine Dichotomisierung nötig -> siehe unten


###########################################
#### Variablen dichotomisiert - rechts ####
###########################################

data_filtered_v1_dich_fil_right <- data_filtered_v1_dich %>% 
  filter(gender2!=3) %>% 
  filter(race2!=3) #Ausschluss Ausweichkategorien für Geschlecht und Hautton

#h1a (v1.5) - dichotom - rechts
data_filtered_v1_dich %>% crosstab(
  machtposition2,
  large2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v1.5) - dichotom - rechts
data_filtered_v1_dich %>% crosstab(
  machtposition2,
  position2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v1.5) - dichotom - rechts

data_filtered_v1_dich_fil_right %>% crosstab(
  gender2,
  large2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2b (v1.5) - dichotom - rechts
data_filtered_v1_dich_fil_right %>% crosstab(
  gender2,
  position2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v1.5) - dichotom - rechts
data_filtered_v1_dich_fil_right %>% crosstab(
  race2,
  large2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v1.5) - dichotom - rechts
data_filtered_v1_dich_fil_right %>% crosstab(
  race2,
  position2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v1.5) -> keine Dichotomisierung nötig -> siehe unten


#h4b (v1.5) -> keine Dichotomisierung nötig -> siehe unten










#################################################
#### ursprüngliche drei Ausprägungen - links ####
#################################################

data_filtered_v1_fil <- data_filtered_v1 %>% 
  filter(gender1!=3) %>% 
  filter(race1!=3) #Ausschluss Ausweichkategorien für Geschlecht und Hautton

#h1a (v1.5) - links
data_filtered_v1 %>% crosstab(
  machtposition1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v1.5) - links
data_filtered_v1 %>% crosstab(
  machtposition1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v1.5) - links
crosstabs_5 <- data_filtered_v1_fil %>% crosstab(
  gender1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_5, method = "bonferroni", round = 12)

#h2b (v1.5) - links
data_filtered_v1_fil %>% crosstab(
  gender1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v1.5) - links
data_filtered_v1_fil %>% crosstab(
  race1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v1.5) - links
data_filtered_v1_fil %>% crosstab(
  race1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v1.5) - links
data_filtered_v1_fil %>% crosstab(
  gender1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)

#h4b (v1.5) - links
data_filtered_v1_fil %>% crosstab(
  race1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)



##################################################
#### ursprüngliche drei Ausprägungen - rechts ####
##################################################

data_filtered_v1_fil_right <- data_filtered_v1 %>% 
  filter(gender2!=3) %>% 
  filter(race2!=3) #Ausschluss Ausweichkategorien für Geschlecht und Hautton

#h1a (v1.5) - rechts
data_filtered_v1 %>% crosstab(
  machtposition2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v1.5) - rechts
data_filtered_v1 %>% crosstab(
  machtposition2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v1.5) - rechts
data_filtered_v1_fil_right %>% crosstab(
  gender2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)


#h2b (v1.5) - rechts
data_filtered_v1_fil_right %>% crosstab(
  gender2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v1.5) - rechts
data_filtered_v1_fil_right %>% crosstab(
  race2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v1.5) - rechts
data_filtered_v1_fil_right %>% crosstab(
  race2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v1.5) - rechts
data_filtered_v1_fil_right %>% crosstab(
  gender2,
  machtposition2,
  chi_square = TRUE,
  add_total = TRUE
)

#h4b (v1.5) - rechts
data_filtered_v1_fil_right %>% crosstab(
  race2,
  machtposition2,
  chi_square = TRUE,
  add_total = TRUE
)














#####################################
#### crosstabs per model -> v2.0 ####
#####################################

##################################
#### Variablen dichotomisiert ####
##################################

data_filtered_v2 <- data_tibble %>% 
  filter(version == "2_0")

data_filtered_v2_dich <- data_filtered_v2 %>% 
  mutate(
    large1_rec = case_when(
      large1 == 1 ~ 1,
      large1 == 2 ~ 2,
      large1 == 3 ~ 2
    ),
    large2_rec = case_when(
      large2 == 1 ~ 1,
      large2 == 2 ~ 2,
      large2 == 3 ~ 2
    ),
    position1_rec = case_when(
      position1 == 1 ~ 1,
      position1 == 2 ~ 2,
      position1 == 3 ~ 2
    ),   
    position2_rec = case_when(
      position2 == 1 ~ 1,
      position2 == 2 ~ 2,
      position2 == 3 ~ 2
    ),      
  )


##########################################
#### Variablen dichotomisiert - links ####
##########################################

data_filtered_v2_dich_fil <- data_filtered_v2_dich %>% 
  filter(gender1!=3) %>% 
  filter(race1!=3)


#h1a (v2.0) - dichotom - links
data_filtered_v2_dich %>% crosstab(
  machtposition1,
  large1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v2.0) - dichotom - links
data_filtered_v2_dich %>% crosstab(
  machtposition1,
  position1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v2.0) - dichotom - links
data_filtered_v2_dich_fil %>% crosstab(
  gender1,
  large1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2b (v2.0) - dichotom - links
data_filtered_v2_dich_fil %>% crosstab(
  gender1,
  position1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v2.0) - dichotom - links
data_filtered_v2_dich_fil %>% crosstab(
  race1,
  large1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v2.0) - dichotom - links
data_filtered_v2_dich_fil %>% crosstab(
  race1,
  position1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v2.0) -> keine Dichotomisierung nötig -> siehe unten

#h4b (v2.0) -> keine Dichotomisierung nötig -> siehe unten



###########################################
#### Variablen dichotomisiert - rechts ####
###########################################


data_filtered_v2_dich_fil_right <- data_filtered_v2_dich %>% 
  filter(gender2!=3) %>% 
  filter(race2!=3)


#h1a (v2.0) - dichotom
data_filtered_v2_dich %>% crosstab(
  machtposition2,
  large2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v2.0)
data_filtered_v2_dich %>% crosstab(
  machtposition2,
  position2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v2.0) - dichotom
data_filtered_v2_dich_fil_right %>% crosstab(
  gender2,
  large2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2b (v2.0) - dichotom
data_filtered_v2_dich_fil_right %>% crosstab(
  gender2,
  position2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v2.0) - dichotom
data_filtered_v2_dich_fil_right %>% crosstab(
  race2,
  large2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v2.0) - dichotom
data_filtered_v2_dich_fil_right %>% crosstab(
  race2,
  position2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v2.0) -> keine Dichotomisierung nötig -> siehe unten

#h4b (v2.0) -> keine Dichotomisierung nötig -> siehe unten





#################################################
#### ursprüngliche drei Ausprägungen - links ####
#################################################


data_filtered_v2_fil <- data_filtered_v2 %>% 
  filter(gender1!=3) %>% 
  filter(race1!=3)

#h1a (v2.0) - links
data_filtered_v2 %>% crosstab(
  machtposition1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v2.0) - links
data_filtered_v2 %>% crosstab(
  machtposition1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v2.0) - links
crosstabs_6 <- data_filtered_v2_fil %>% crosstab(
  gender1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_6, method = "bonferroni", round = 12)

#h2b (v2.0) - links
data_filtered_v2_fil %>% crosstab(
  gender1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v2.0) - links
data_filtered_v2_fil %>% crosstab(
  race1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v2.0) - links
data_filtered_v2_fil %>% crosstab(
  race1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v2.0) - links
data_filtered_v2_fil %>% crosstab(
  gender1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)

#h4b (v2.0) - links
data_filtered_v2_fil %>% crosstab(
  race1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)



##################################################
#### ursprüngliche drei Ausprägungen - rechts ####
##################################################


data_filtered_v2_fil_right <- data_filtered_v2 %>% 
  filter(gender2!=3) %>% 
  filter(race2!=3)

#h1a (v2.0) - rechts
data_filtered_v2 %>% crosstab(
  machtposition2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v2.0) - rechts
data_filtered_v2 %>% crosstab(
  machtposition2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v2.0) - rechts
data_filtered_v2_fil_right %>% crosstab(
  gender2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)


#h2b (v2.0) - rechts
data_filtered_v2_fil_right %>% crosstab(
  gender2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v2.0) - rechts
data_filtered_v2_fil_right %>% crosstab(
  race2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v2.0) - rechts
data_filtered_v2_fil_right %>% crosstab(
  race2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v2.0) - rechts
data_filtered_v2_fil_right %>% crosstab(
  gender2,
  machtposition2,
  chi_square = TRUE,
  add_total = TRUE
)

#h4b (v2.0) - rechts
data_filtered_v2_fil_right %>% crosstab(
  race2,
  machtposition2,
  chi_square = TRUE,
  add_total = TRUE
)















#####################################
#### crosstabs per model -> v3.5 ####
#####################################

##################################
#### Variablen dichotomisiert ####
##################################

data_filtered_v3 <- data_tibble %>% 
  filter(version == "3_5")

data_filtered_v3_dich <- data_filtered_v3 %>% 
  mutate(
    large1_rec = case_when(
      large1 == 1 ~ 1,
      large1 == 2 ~ 2,
      large1 == 3 ~ 2
    ),
    large2_rec = case_when(
      large2 == 1 ~ 1,
      large2 == 2 ~ 2,
      large2 == 3 ~ 2
    ),
    position1_rec = case_when(
      position1 == 1 ~ 1,
      position1 == 2 ~ 2,
      position1 == 3 ~ 2
    ),   
    position2_rec = case_when(
      position2 == 1 ~ 1,
      position2 == 2 ~ 2,
      position2 == 3 ~ 2
    ),      
  )

##########################################
#### Variablen dichotomisiert - links ####
##########################################


data_filtered_v3_dich_fil <- data_filtered_v3_dich %>% 
  filter(gender1!=3) %>% 
  filter(race1!=3)

#h1a (v3.5) - dichotom - links
data_filtered_v3_dich %>% crosstab(
  machtposition1,
  large1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v3.5) - dichotom - links
data_filtered_v3_dich %>% crosstab(
  machtposition1,
  position1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v3.5) - dichotom - links
data_filtered_v3_dich_fil %>% crosstab(
  gender1,
  large1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2b (v3.5) - dichotom - links
data_filtered_v3_dich_fil %>% crosstab(
  gender1,
  position1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v3.5) - dichotom - links
data_filtered_v3_dich_fil %>% crosstab(
  race1,
  large1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v3.5) - dichotom - links
data_filtered_v3_dich_fil %>% crosstab(
  race1,
  position1_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v3.5) -> keine Dichotomisierung nötig -> siehe unten

#h4b (v3.5) -> keine Dichotomisierung nötig -> siehe unten



###########################################
#### Variablen dichotomisiert - rechts ####
###########################################



data_filtered_v3_dich_fil_right <- data_filtered_v3_dich %>% 
  filter(gender2!=3) %>% 
  filter(race2!=3)

#h1a (v3.5) - dichotom - rechts
data_filtered_v3_dich %>% crosstab(
  machtposition2,
  large2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v3.5) - dichotom - rechts
data_filtered_v3_dich %>% crosstab(
  machtposition2,
  position2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v3.5) - dichotom - rechts
data_filtered_v3_dich_fil_right %>% crosstab(
  gender2,
  large2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h2b (v3.5) - dichotom - rechts
data_filtered_v3_dich_fil_right %>% crosstab(
  gender2,
  position2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v3.5) - dichotom - rechts
data_filtered_v3_dich_fil_right %>% crosstab(
  race2,
  large2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v3.5) - dichotom - rechts
data_filtered_v3_dich_fil_right %>% crosstab(
  race2,
  position2_rec,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v3.5) -> keine Dichotomisierung nötig -> siehe unten

#h4b (v3.5) -> keine Dichotomisierung nötig -> siehe unten









#################################################
#### ursprüngliche drei Ausprägungen - links ####
#################################################


data_filtered_v3_fil <- data_filtered_v3 %>% 
  filter(gender1!=3) %>% 
  filter(race1!=3)

#h1a (v3.5) - links
crosstabs_7 <- data_filtered_v3 %>% crosstab(
  machtposition1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_7, method = "bonferroni", round = 12)

#h1b (v3.5) - links
data_filtered_v3 %>% crosstab(
  machtposition1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v3.5) - links
crosstabs_8 <- data_filtered_v3_fil %>% crosstab(
  gender1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_8, method = "bonferroni", round = 12)

#h2b (v3.5) - links
crosstabs_9 <- data_filtered_v3_fil %>% crosstab(
  gender1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_9, method = "bonferroni", round = 12)

#h3a (v3.5) - links
data_filtered_v3_fil %>% crosstab(
  race1,
  large1,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v3.5) - links
data_filtered_v3_fil %>% crosstab(
  race1,
  position1,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v3.5) - links
crosstabs_10 <- data_filtered_v3_fil %>% crosstab(
  gender1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)

chisq.posthoc.test(crosstabs_10, method = "bonferroni", round = 12)

#h4b (v3.5) - links
data_filtered_v3_fil %>% crosstab(
  race1,
  machtposition1,
  chi_square = TRUE,
  add_total = TRUE
)




##################################################
#### ursprüngliche drei Ausprägungen - rechts ####
##################################################


data_filtered_v3_fil_right <- data_filtered_v3 %>% 
  filter(gender2!=3) %>% 
  filter(race2!=3)

#h1a (v3.5) - rechts
data_filtered_v3 %>% crosstab(
  machtposition2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#h1b (v3.5) - rechts
data_filtered_v3 %>% crosstab(
  machtposition2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#h2a (v3.5) - rechts
data_filtered_v3_fil_right %>% crosstab(
  gender2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#h2b (v3.5) - rechts
data_filtered_v3_fil_right %>% crosstab(
  gender2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#h3a (v3.5) - rechts
data_filtered_v3_fil_right %>% crosstab(
  race2,
  large2,
  chi_square = TRUE,
  add_total = TRUE
)

#h3b (v3.5) - rechts
data_filtered_v3_fil_right %>% crosstab(
  race2,
  position2,
  chi_square = TRUE,
  add_total = TRUE
)

#h4a (v3.5) - rechts
data_filtered_v3_fil_right %>% crosstab(
  gender2,
  machtposition2,
  chi_square = TRUE,
  add_total = TRUE
)

#h4b (v3.5) - rechts
data_filtered_v3_fil_right %>% crosstab(
  race2,
  machtposition2,
  chi_square = TRUE,
  add_total = TRUE
)








