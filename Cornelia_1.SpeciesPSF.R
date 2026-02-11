###This is a trial to use github
## Calculates Home vs Away measures and pairwise PSF and puts files into same format
## Saves a species list to collect root traits from the database 
##  
##  Project: Review Plant & Soil
##
##  by  Gemma Rutten (gemma.rutten@unibe.ch)
##      Last Edited January 23
##
##
### Used data from Petermann et al 2008 (and in PART II Bennett et al 2017)

## clean working space 
cat("\014") 
rm(list=ls())

## set path
#path <- "C:/analysis/linkingRES&PSF"
#setwd(path)
getwd()

## load packages
library(tidyverse) # install.packages("tidyverse") #install only once 
library(vegan)

## load functions 
len<-function(x){length(na.omit(x))} #count number of reps without NAs
se<-function(x){sd(na.omit(x))/length(na.omit(x))} #calculate standard error 

## load data
#### table with plant species names and performance on home and away soil
## started with 3 studies but Crawford (A) was left out because no sterile treatments were used 

##### Study3. Pairwise PSF in Grassland communities #######

## reformat Petermann2008 data 

# read in data Petermann et al. 2008
raw.data <- read.csv("Data/RawData/Petermann2008Data.csv", sep=";", stringsAsFactors = T)
raw.data$species.ID<-as.factor(raw.data$species.ID)
str(raw.data)# 2160 obs

## replace species numbers with names
species <- read.csv("Data/RawData/Petermann2008SpeciesList.csv", sep=";", stringsAsFactors = F)
str(species)

sp<-as.character(species$Species)
raw.data$Species.A <- as.factor(sp[raw.data$species.ID])

so <- species %>% 
  select(soil.ID, Species) %>%
  arrange(soil.ID) 
sp2<-as.character(so$Species)
raw.data$Species.B <- as.factor(sp2[raw.data$soil.ID])

## replace treatment numbers with numbers
treat <- c("control", "fertilizer", "fungicide", "activated_carbon", "sterile")			
raw.data$treat <- as.factor(treat[as.factor(raw.data$trtm)])

head(raw.data)

##
datA<-raw.data %>%
  mutate(Species.pair = as.factor(paste(Species.A,Species.B, sep="_")))%>%
  filter(div==1)%>%
  group_by(Species.pair, treat, h_a) %>%
  dplyr::summarize(Species.A=first(Species.A),
                   Species.B=first(Species.B),
                   mean = mean(weight),
                   se = sd(weight)/sqrt(len(weight)),
                   N = len(weight), 
  )

datA$HA=ifelse(datA$Species.A == datA$Species.B, "home", "away")

### calculate PSF first separate home from away soils
H<-datA %>%
  filter(HA=="home")
A<-datA %>%
  filter(HA=="away")
str(A)
PSF<-merge(A, H, by = c("Species.A","treat"), all.x = T)
str(PSF)

### make and overview
#names(datA)
PSFs.C <- PSF %>%
  select(Species.A=Species.A,
         Species.B=Species.B.x,
         treat = treat ,
         AinA.mean = mean.y ,
         AinA.se = se.y,
         AinA.N = N.y,
         AinB.mean = mean.x,
         AinB.se = se.y,
         AinB.N = N.x)

### Calculate the actual PSFs (home vs away)
PSFs.C$HAa = log(PSFs.C$AinA.mean / PSFs.C$AinB.mean)

PSFs.C$Species.pair <-as.character(paste(PSFs.C$Species.A,PSFs.C$Species.B, sep="_"))# 240 obs.
# 48 unique species pairs= 24 species grown with 2 different functional groups
save(PSFs.C, file = "Data/home.vs.Away.PSF.pet.Rdata") 

#summary(PSFs.C)# 240 comparisons with 5 different treatments
names(PSFs.C)# Petermann has a control and sterile treatment

### separate the two to calculate PSF (conditioned/ unconditioned)

lifePSFs.C<-PSFs.C %>% 
  filter(treat == "control")%>%
  select("Species.pair","Species.A", "Species.B", "AinA.mean", "AinA.se",
         "AinA.N", "AinB.mean", "AinB.se","AinB.N","HAa")

sterPSFs.C<-PSFs.C %>% 
  filter(treat == "sterile" )%>%
  select("Species.pair","AinA.mean", "AinA.se",
         "AinA.N", "AinB.mean", "AinB.se","AinB.N")%>%
  `colnames<-`(c("Species.pair", "st.AinA.mean", "st.AinA.se",
                 "st.AinA.N", "st.AinB.mean", "st.AinB.se","st.AinB.N"))

sel.PSFs.C<-merge(lifePSFs.C,sterPSFs.C, by="Species.pair")

sel.PSFs.C$Hbio = log(sel.PSFs.C$AinA.mean / sel.PSFs.C$st.AinA.mean)
sel.PSFs.C$Abio = log(sel.PSFs.C$AinB.mean / sel.PSFs.C$st.AinB.mean)
sel.PSFs.C$Study<-c("petermann")## selected common columns

### Show the different PSF measures in a nicer graph
names(sel.PSFs.C)

sel.PSFs.C_meanHA <- setNames( 
  aggregate(HAa ~ Species.A, sel.PSFs.C, mean),
  c("Species", "meanPSF"))

sel.PSFs.C_meanFieldSter <- setNames( 
  aggregate(Hbio ~ Species.A, sel.PSFs.C, mean),
  c("Species", "meanPSFfs"))

## a potential graph
tiff('Plots/meanPSFfs.tiff',
     width = 20, height = 20, units = "cm", res = 400 , pointsize= 15,bg="white")
par(mar = c(4, 4, 1, 2),xpd = T)

ggplot() +
  #geom_point(mapping = aes(x = Species.A, y = HAa), data = sel.PSFs.C) +
  geom_point(
    mapping = aes(x = Species, y = meanPSFfs), data = sel.PSFs.C_meanFieldSter,
    colour = 'darkgreen', size = 3)+
  geom_hline(yintercept= 0)+
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(face = "italic"))

dev.off()


tiff('Plots/PetermansPSFsHA.tiff',
     width = 20, height = 20, units = "cm", res = 400 , pointsize= 15,bg="white")
par(mar = c(4, 4, 1, 2),xpd = T)

ggplot() +
  #geom_point(mapping = aes(x = Species.A, y = HAa), data = sel.PSFs.C) +
  geom_point(
    mapping = aes(x = Species, y = meanPSF), data = sel.PSFs.C_meanHA,
    colour = 'darkorange', size = 3)+
  geom_hline(yintercept= 0)+
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(face = "italic"))

dev.off()

## 
## Q1 What is the difference between the two feedback measures? What does this mean ecologically?
## Q2 What do you see, which species show a feedback?

###
###
###
### PART II
###
###
###


## Save a species list for the Root trait database
Species.A<-tibble(species.full=unique(PSFs.C$Species.A));Species.B<-tibble(species.full=unique(PSFs.C$Species.B))
species.list<-Species.A %>% full_join(Species.B)# 24 species 
## save a species list for GRoot trait db in other directory
write.csv2(species.list, "Data/linking.RES.PSF.SpeciesList.pet.csv")

## check if species number is correct
table(as.factor(PSFs.C$Species.A))# 10 ind
table(as.factor(PSFs.C$Species.B))# 10 ind

## ## check pairwise obs.
Cne <- PSFs.C %>%
  rename(Species.A1=Species.B,
         Species.B1=Species.A,
         BinB.mean = AinA.mean,
         BinB.se = AinA.se,
         BinB.N = AinA.N,
         BinA.mean = AinB.mean,
         BinA.se = AinB.se,
         BinA.N = AinB.N,
         HAb=HAa)

Cne$Species.pair <- paste(Cne$Species.A1, Cne$Species.B1, sep="_")

## check which combinations both have home soil 
PSFs.C[PSFs.C$Species.pair %in% Cne$Species.pair,] ## only two species pairs pairwise.

# All
Pairwise.Petermann<-merge(PSFs.C, Cne[ ,c(3:11)], by=c("Species.pair","treat"))
Pairwise.Petermann$pairwise<-Pairwise.Petermann$HAa+Pairwise.Petermann$HAb
save(Pairwise.Petermann, file = "Data/pw.PSF.pet.Rdata") 
#pwAB = HAa+HAb,
#pw1 = log(AinA.mean / AinB.mean) + log(BinB.mean / BinA.mean),
#pw2 = log(AinA.mean) - log(AinB.mean) - log(BinA.mean) + log(BinB.mean)

#####  Study 2. H/A PSF in Forests #######

# read in data Bennett
data <- read.csv("Data/RawData/Bennett_aai8212_database-s1.csv", sep=";")
str(data)# 100 obs

data$Species.pair<-as.factor(paste(data$Species.A,data$Species.B, sep="_"))
table(data$Species.pair)## most only measured once
### link AinA vs AinB with BinA vs BinB 
#names(data)
PSFs.B <- data %>%
  group_by(Species.pair) %>%
  dplyr::summarize(Species.A=first(Species.A),
                   Family.A= first(Family.A), 
                   mycorrhiza.A= first(Type.of.Mycorrhiza),
                   Species.B=first(Species.B),
                   Family.B= first(Family.B), 
                   mycorrhiza.B= first(Hetero.type),
                   AinA.mean = mean(Biomass.in.Conspecific.Soil),
                   AinA.se = se(Biomass.in.Conspecific.Soil),
                   AinA.N = len(Biomass.in.Conspecific.Soil),
                   AinB.mean = mean(Biomass.in.Heterospecific.Soil),
                   AinB.se = se(Biomass.in.Heterospecific.Soil),
                   AinB.N = len(Biomass.in.Heterospecific.Soil),
                   HAa = log(AinA.mean / AinB.mean),
                   st.AinA.mean = mean(Biomass.in.Conspecific.Sterile.soil),
                   st.AinA.se = se(Biomass.in.Conspecific.Sterile.soil),
                   st.AinA.N = len(Biomass.in.Conspecific.Sterile.soil),
                   st.AinB.mean = mean(Biomass.in.Heterospecific..Sterile.soil),
                   st.AinB.se = se(Biomass.in.Heterospecific..Sterile.soil),
                   st.AinB.N = len(Biomass.in.Heterospecific..Sterile.soil),
                   Hbio = log(AinA.mean / st.AinA.mean),
                   Abio = log(AinB.mean / st.AinB.mean))

PSFs.B[is.na(PSFs.B)] <- 0
PSFs.B$Species.pair <-as.character(PSFs.B$Species.pair)# 79 obs.

save(PSFs.B, file = "Data/home.vs.Away.PSF.ben.Rdata") 

## Save a species list for the Root trait database
Species.A<-tibble(species.full=unique(PSFs.B$Species.A));Species.B<-tibble(species.full=unique(PSFs.B$Species.B))
species.list<-Species.A %>% full_join(Species.B)# 44 species 
## save a species list for GRoot trait db in other directory
write.csv2(species.list, "Data/linking.RES.PSF.SpeciesList.ben.csv")

## check if species number is correct
table(as.factor(PSFs.B$Species.A))# 10 species
table(as.factor(PSFs.B$Species.B))# 38 species
intersect(levels(as.factor(PSFs.B$Species.A)), levels(as.factor(PSFs.B$Species.B)))# 4 common
4+(10-4)+(38-4)# 44 species 

## check pairwise obs.
B <- data %>%
  group_by(Species.pair) %>%
  dplyr::summarize(Species.A=first(Species.A),
                   Family.A= first(Family.A), 
                   mycorrhiza.A= first(Type.of.Mycorrhiza),
                   Species.B=first(Species.B),
                   Family.B= first(Family.B), 
                   mycorrhiza.B= first(Hetero.type),
                   BinB.mean = mean(Biomass.in.Conspecific.Soil),
                   BinB.se = se(Biomass.in.Conspecific.Soil),
                   BinB.N = len(Biomass.in.Conspecific.Soil),
                   BinA.mean = mean(Biomass.in.Heterospecific.Soil),
                   BinA.se = se(Biomass.in.Heterospecific.Soil),
                   BinA.N = len(Biomass.in.Heterospecific.Soil))

B[is.na(B)] <- 0

B$Species.pair <- paste(B$Species.B, B$Species.A, sep="_")

## check which combinations both have home soil 
PSFs.B[PSFs.B$Species.pair %in% B$Species.pair,] ## only two species pairs pairwise.

# Acer saccharum_Fraxinus americana & Betula papyrifera_Pinus strobus
Pairwise.Bennett<-merge(PSFs.B,B[c(1,8:13)], by="Species.pair")

###
# Table with plant species names and performance on home and away soil
load("Data/home.vs.Away.PSF.ben.Rdata") #PSFs bennett
summary(PSFs.B)# 79 comparisons
names(PSFs.B)# Bennett has sterile treatment for cons and hetero separately =  PSFlife/control
# "st.AinA.mean", "st.AinA.se", "st.AinA.N",    "st.AinB.mean", "st.AinB.se",   "st.AinB.N","Hbio","Abio"
sel.PSFs.B<-PSFs.B %>% 
  select(-c("Family.A"  ,   "mycorrhiza.A", "Family.B"  ,   "mycorrhiza.B"))
sel.PSFs.B$Study<-c("bennett")## selected common columns

# Table with plant species names and performance on home and away soil
# save(PSFs.C, file = "Data/home.vs.Away.PSF.pet.Rdata") 
load("Data/home.vs.Away.PSF.pet.Rdata") #PSFs petermann
summary(PSFs.C)# 240 comparisons with 5 different treatments
names(PSFs.C)# Peterman has a control and sterile treatment

lifePSFs.C<-PSFs.C %>% 
  filter(treat == "control")%>%
  select("Species.pair","Species.A", "Species.B", "AinA.mean", "AinA.se",
         "AinA.N", "AinB.mean", "AinB.se","AinB.N","HAa")

sterPSFs.C<-PSFs.C %>% 
  filter(treat == "sterile" )%>%
  select("Species.pair","AinA.mean", "AinA.se",
         "AinA.N", "AinB.mean", "AinB.se","AinB.N")%>%
  `colnames<-`(c("Species.pair", "st.AinA.mean", "st.AinA.se",
                 "st.AinA.N", "st.AinB.mean", "st.AinB.se","st.AinB.N"))
#
#
#
#
#save(Pairwise.Petermann, file = "Data/pw.PSF.pet.Rdata") #pairwise
load("Data/pw.PSF.pet.Rdata") #PSFs petermann
summary(Pairwise.Petermann)# 240 comparisons with 5 different treatments
names(Pairwise.Petermann)# Peterman has a control and sterile treatment


lifePSFs.C<-Pairwise.Petermann %>% 
  filter(treat == "control")%>%
  select("Species.pair","Species.A", "Species.B", "AinA.mean", "AinA.se",
         "AinA.N", "AinB.mean", "AinB.se","AinB.N","BinB.mean","BinB.se","BinB.N"
         , "BinA.mean", "BinA.se", "BinA.N","HAa", "HAb", "pairwise")

sterPSFs.C<-Pairwise.Petermann %>% 
  filter(treat == "sterile" )%>%
  select("Species.pair","AinA.mean", "AinA.se",
         "AinA.N", "AinB.mean", "AinB.se","AinB.N")%>%
  `colnames<-`(c("Species.pair", "st.AinA.mean", "st.AinA.se",
                 "st.AinA.N", "st.AinB.mean", "st.AinB.se","st.AinB.N"))

sel.PSFs.C<-merge(lifePSFs.C,sterPSFs.C, by="Species.pair")

sel.PSFs.C$Hbio = log(sel.PSFs.C$AinA.mean / sel.PSFs.C$st.AinA.mean)
sel.PSFs.C$Abio = log(sel.PSFs.C$AinB.mean / sel.PSFs.C$st.AinB.mean)
sel.PSFs.C$Study<-c("petermann")## selected common columns

# combine the two studies
names(sel.PSFs.B);length(sel.PSFs.B$Species.pair)#79
names(sel.PSFs.C[,-c(10:15,17,18)]);length(sel.PSFs.C[,-c(10:15,17,18)]$Species.pair)#48

PSFs<-rbind(sel.PSFs.B,sel.PSFs.C[,-c(10:15,17,18)])
str(PSFs)#127

##make a nice graph

bennet<- setNames( 
  aggregate(HAa ~ Species.A, sel.PSFs.B, mean),
  c("Species", "meanPSFfs"))

tiff('Plots/Bennet.tiff',
     width = 20, height = 20, units = "cm", res = 400 , pointsize= 15,bg="white")
par(mar = c(4, 4, 1, 2),xpd = T)

ggplot() +
  #geom_point(mapping = aes(x = Species.A, y = HAa), data = sel.PSFs.C) +
  geom_point(
    mapping = aes(x = Species, y = meanPSFfs), data = bennet,
    colour = 'darkviolet', size = 3)+
  geom_hline(yintercept= 0)+
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(face = "italic"))

dev.off()

