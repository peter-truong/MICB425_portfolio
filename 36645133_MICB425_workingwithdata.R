library(tidyverse)
read.table(file="Saanich.metadata.txt")
read.table(file="Saanich.metadata.txt")
metadata = read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")

#Let’s look at the oxygen (O2) in our water samples. Notice how we pipe the data frame with our metadata into the select function, so the following is equivalent to select(metadata, O2_uM)

metadata %>%  select(O2_uM)
metadata %>%  select(matches("O2|oxygen"))
metadata %>% 
  filter(O2_uM == 0)

#which depths have no oxygen, combine filter and select with a pipe

metadata %>% 
  filter(O2_uM == 0) %>% 
  select(Depth_m)

#Exercise 2Using dplyr, find at what depth(s) methane (CH4)
#is above 100 nM while temperature is below 10 °C. Print out a table showing only the depth, methane, and temperature data. %>% 


metadata %>% select(matches("C|temp")) 

metadata %>% 
  filter(CH4_nM > 100) %>% 
  filter(Temperature_C < 10) %>% 
  select(Depth_m)
  
#want to plot all nutrients in uM, n2o and ch4 are nM, use mutate function

metadata %>% 
  mutate(N2O_uM = N2O_nM/1000)

metadata %>% 
  mutate(N2O_uM = N2O_nM/1000) %>% 
  ggplot() + geom_point(aes(x=Depth_m, y=N2O_uM))

#Exercise 3Convert all variables that are in nM to μM. Output a table showing only the original nM and converted μM variables

metadata %>% select(matches("nM"))

metadata %>% 
  mutate (N2O_uM = N2O_nM/1000, Std_N2O_uM = Std_N2O_nM/1000, Std_CH4_uM = Std_CH4_nM/1000, CH4_uM = CH4_nM/1000) %>% 
  select(matches("N2O|CH4"))
