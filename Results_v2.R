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