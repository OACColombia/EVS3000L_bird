######EVS3000L Data Analysis - Bird measurements ##########


#I will use the package tidyverse and ggplot2
  #remember to install.package("ggplot2") in your computer

#I already have the package, so I will not install nothing here. 
  #Nonetheless, RStudio is asking up (in yellow) if you want to install the packages
  #click 'yes' to install the needed packages

library(tidyverse) #I will use pipes. 
  #Pipes let you take the output of one function and send it directly to the next, 
    #which is useful when you need to do many things to the same dataset

#I will load the dataframe

bird <- read_csv("data/Bird_Measurements.csv")

head(bird)
#so, with this data we can compare graphically how much your measurements vary from mine

#But first, I have to manipulate the dataframe
names(bird)

#Actually, you didn't catch the mass annotated in the label, so I will remove that variable
bird <- bird[,c(1:13)]

#now to transform the data from wide to longer
bird1 <- bird %>%
  pivot_longer(names_to = "Measurement", 
               values_to = "Value", 
               cols = -(c(SpecimenID, Species, Sex, Measured_by)))
head(bird1)

#Now we can graph
library(ggplot2)

#I would like an order in particular for the species an the measurements
bird1$Species <- factor(bird1$Species, levels = c("Quiscalus major",
                                                  "Quiscalus quiscula",
                                                  "Cardinalis cardinalis",
                                                  "Pheucticus ludovicianus",
                                                  "Setophaga tigrina",
                                                  "Setophaga palmarum",
                                                  "Setophaga caerulescens",
                                                  "Setophaga petechia",
                                                  "Setophaga coronata",
                                                  "Setophaga pensylvanica",
                                                  "Setophaga ruticilla", 
                                                  "Setophaga americana"))


bird1$SpecimenID <- factor(bird1$SpecimenID, levels = c("UF45274",
                                                        "UF45268",
                                                        "UF45617",
                                                        "UF54876",
                                                        "UF43293",
                                                        "UF54877",
                                                        "UF45273",
                                                        "UF45663",
                                                        "UF45614",
                                                        "UF52189",
                                                        "UF52211",
                                                        "UF43290",
                                                        "UF52871",
                                                        "UF45615",
                                                        "UF44340"))


ggplot(bird1, aes(x = SpecimenID, y = Value, group = SpecimenID))+
  geom_line(color = "gray")+
  geom_point(aes(fill = Species, alpha = Measured_by), size = 3, shape = 21) +
  labs(x = "Specimen ID", y = "Values (mm)") +
  theme_classic()+
  facet_wrap(~Measurement, scales = "free_y", ncol = 3)+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust=1))


#How our measurement differ from the reported in literature? ####

#I download the values from AVONET (Tobias et al. 2022)
  #And adjusted the names of the variables

avonet <- read_csv("data/AVONET_data.csv")

#extract the species used in my data

avonet_EV <- avonet %>%
  filter(Species %in% bird1$Species)
table(avonet_EV$Species)

names(avonet_EV)
#Use only the variables I am interested (for now), and transform from wide to longer

avonet_EV1 <- avonet_EV %>%
  select(Species, Sex,`1_Total_Culmen`,`2_Nostril_Culmen`,`3_Beak_depth`,
         `4_Beak_width`,`5_Tarsus`,`6_Wing_length`,`7_Secondary_length`,
         `8_Kipps_distance`,`9_Tail_lenght`) %>%
  pivot_longer(names_to = "Measurement",
               values_to = "Value",
               cols = -(c(Species,Sex)))

head(avonet_EV1)

#generate a summary value of our data measured in class
library(Rmisc)
for_limits=summarySE(data = bird1,
                     measurevar = "Value",
                     groupvars = c("Species", "Measurement"), 
                     na.rm = TRUE)
for_limits

#Graph density of AVONET data, with an astherisk of our measurement
library(ggridges)

avonet_EV1$Species <- factor(avonet_EV1$Species, levels = c("Quiscalus major",
                                                  "Quiscalus quiscula",
                                                  "Cardinalis cardinalis",
                                                  "Pheucticus ludovicianus",
                                                  "Setophaga tigrina",
                                                  "Setophaga palmarum",
                                                  "Setophaga caerulescens",
                                                  "Setophaga petechia",
                                                  "Setophaga coronata",
                                                  "Setophaga pensylvanica",
                                                  "Setophaga ruticilla", 
                                                  "Setophaga americana"))


plt <- ggplot(avonet_EV1, aes(x=Value, y=Species, fill = Species))+
         geom_hline(yintercept = avonet_EV1$Species, color="gray")+
         geom_point(color="black", size=0.7)+
         geom_point(color="gray", size=0.5)+
         geom_density_ridges(alpha=0.75)+
         theme_classic()+
         labs(x="Value (mm)")+
        facet_wrap(~Measurement, scales = "free_x", ncol = 3)+
         theme(legend.position = "none",
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), 
               axis.text.y = element_text(face = "italic"))

plt + geom_text(data = for_limits,
                label = "(*)")
