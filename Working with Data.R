#comments 
install.packages("tidyverse")
library(tidyverse)
#In terminal, copy data
cp ~/Documents/MICB425_materials/Saanich... ~/Documents/MICB425_portfolio 

#only for materuals repo 


#Load data 
read.table(file="Saanich.metadata.txt")

read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t", na.strings="NAN")

metadata = read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")

#DAY 2

library(tidyverse) 
data %>% function
function(data)
#select variables with O2 or oxyfen in the name

  metadata %>%
  select(02-uM) 

metadata %>% 
  select(O2_uM)

metadata %>% 
  select(matches("O2|oxygen"))
#filter rows 

metadata %>% 
  filter(O2_uM == 0)

metadata %>% 
  filter(O2_uM == 0) %>% 
  select(Depth_m)

#Using dplyr, find at what depth(s) methan (CH4) is above 100 nM while temperature is below 10C. print out a table showing only the depthm methane, and temperature data.

metadata %>% 
  select(matches("CH4|methane"))
metadata %>% 
  select(matches("temperature"))

#Variables are Ch4-nM and Temperature_C

metadata %>% 
  filter (CH4_nM > 100 & Temperature_C < 10) %>% 
  select (Depth_m, CH4_nM, Temperature_C)


#mutate- convert variables, any math function works

metadata %>% 
  mutate(N20_uM = N2O_nM/1000) %>% 
#Grammer of graohics  
library(phyloseq)
library(phyloseq)
load("phyloseq_object.RData")  
source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")
library(phyloseq)
load ("phyloseq_object.RData")
load("~/Documents/MICB425_portfolio/.RData")
#Exercise 1
ggplot(metadata, aes(x=PO4_uM, y=Depth_m)) +
  geom_point(color="purple") + geom_point(shape=17)
#Exercise 2
metadata %>%
  mutate(Temperature_F= Temperature_C*9/5+32)
metadata %>% 
  mutate(Temperature_F= Temperature_C*9/5+32) %>% 
  ggplot() + geom_point(aes(x=Temperature_F, y=Depth_m))
#ggplot with dplyr
library(phyloseq)
load("phyloseq_object.RData")
physeq
plot_bar(physeq, fill="Phylum")
physeq_percent = transform_sample_counts(physeq, function(x) 100 * x/sum(x))
plot_bar(physeq_percent, fill="Phylum")
plot_bar(physeq_percent, fill="Phylum") + 
  geom_bar(aes(fill=Phylum), stat="identity")
#Exercise 3
plot_bar(physeq_percent, fill="Phylum",  title="Phyla from 10 to 200 m in Saanich Inlet") + 
  geom_bar(aes(fill=Phylum), stat="identity") + labs(x="Sample depth", y="percent relative abundance")
#Faceting

plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum)
plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum, scales="free_y") +
  theme(legend.position="none")

#Exercise 4

library(tidyverse)
library(phyloseq)
library(dplyr)
table= metadata
table= metadata %>%
  select(Depth_m, O2_uM, PO4_uM, SiO2_uM, NO3_uM, NH4_uM, NO2_uM)
table_1= table %>% 
  gather(Nutrients, Concentration, O2_uM, PO4_uM, SiO2_uM, NO3_uM, NH4_uM, NO2_uM)
ggplot(table_1, aes(x=Depth_m, y=Concentration)) +
  geom_point() + geom_line() + facet_wrap(~Nutrients, scales="free_y") + 
  theme(legend.position = "none")

