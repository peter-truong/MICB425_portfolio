library(tidyverse)

source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")

library(phyloseq)

#read.table(file="Saanich.metadata.txt")
#metadata = read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")

#read.table(file="phyloseq_object.RData")

#metadata = read.table(file="phyloseq_object.RData", header=TRUE, row.names=1, sep="\t")

load("phyloseq_object.RData")

metadata = read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")
ggplot(metadata, aes(x=O2_uM, y=Depth_m))

#to add data we must specify a geom)
ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point()
#top function is equivalent to
#ggplot(metadata) + geom_point(aes(x=O2_uM, y=Depth_m))
ggplot(metadata, aes(x=O2_uM, y=Depth_m, color="blue")) +
  geom_point()

#above , need to specify colour as part of geom function

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(color="blue")

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(shape="square")

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(size=10)

ggplot(metadata, aes(x=O2_uM, y=Depth_m, size=OxygenSBE_V)) +
  geom_point()

#Plot another nutrient of your choice against depth. Change the points to be purple triangles. For a guide to R shapes, 

ggplot(metadata, aes(x=PO4_uM, y=Depth_m)) +
  geom_point (shape=17, color="purple", size=5)

#Exercise 2-Using dplyr, convert the temperature variable from Celsius to Fahrenheit. Then create a dot plot of temperature in Fahrenheit against depth.


metadata %>%
  mutate (Temperature_F = (Temperature_C*1.8)+32) %>% 
  ggplot(aes (x=Temperature_F, y=Depth_m )) +
  geom_point(size=2)


plot_bar(physeq, fill="Phylum")

physeq_percent = transform_sample_counts(physeq, function(x) 100 * x/sum(x))    

plot_bar(physeq_percent, fill="Phylum")

plot_bar(physeq_percent, fill="Phylum") + 
  geom_bar(aes(fill=Phylum), stat="identity")  

#Exercise 3 Create a bar plot at a different taxonomic level with more descriptive x- and y-axis labels as well as a title. 
#Use the ggplot cheatsheet to learn the guides needed to change these labels.

plot_bar(physeq_percent, fill="Class") +
  
  geom_bar(aes(fill=Class), stat="identity") +
  labs (x="Sample Depth", y="Percent Relative Abundance", title= "Class from 10 to 200m in Saanich Inlet")


plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum)


plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum, scales="free_y") +
  theme(legend.position="none")

#Exercise 4 
#Using ggplot, create a faceted figure showing nutrient concentrations in nM for O2, PO4, SiO2, NO3, NH4, and NO2 by depth. 
#For this, you can use either the metadata table or the phyloseq object.
#Hint: Explore the dplyr function gather to manipulate the metadata so that your nutrients of interest are all in one variable (column).
#mutate (N2O_nM = N2O_uM*1000, Std_N2O_uM = Std_N2O_nM/1000, Std_CH4_uM = Std_CH4_nM/1000, CH4_uM = CH4_nM/1000)


  table_1=metadata %>%
  mutate(O2_nM = O2_uM*1000, PO4_nM = PO4_uM*1000, SiO2_nM = SiO2_uM*1000, NO3_nM = NO3_uM*1000, NH4_nM = NH4_uM*1000, NO2_nM = NO2_uM*1000) %>% 
  select(Depth_m, O2_nM, PO4_nM, SiO2_nM, NO3_nM, NH4_nM, NO2_nM)


table_2=table_1 %>% 
  gather (Nutrient, Concentration, O2_nM, PO4_nM, SiO2_nM, NO3_nM, NH4_nM, NO2_nM)

ggplot(table_2,aes(x=Depth_m, y=Concentration)) +
  geom_point() + geom_line() +
  facet_wrap(~Nutrient, scales="free_y") +
  theme(legend.position="none")






