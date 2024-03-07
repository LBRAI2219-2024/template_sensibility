
# The code should compile with any c++11 compiler, e.g. for g++: MinGW has been tested. 
# Then create a new system environment variable. Path --> C:\MinGW\bin
# 
# Open the terminal:
#   cd ~/GitHub/marshal-pipeline/17_06 CRootBox
# 
# FOR MAC AND LINUX
# g++ *.cpp -std=c++11 -o crootbox.out
# crootbox.out
#
#
# FOR WINDOWS
# g++ *.cpp -std=c++11 -o crootbox.exe   
# crootbox.exe

setwd("/Users/guillaumelobet/Desktop/crootbox-marshal")

########################################################################
# 1 : LOAD THE LIBRARIES AND SOURCE FILES
########################################################################

library(tidyverse)
library(plyr)
library(readr)
library(data.table)
library(dplyr)
library(Matrix)

# Custom functions
source("inputs/functions/io_function.R") # CROOTBOX
source("inputs/functions/getSUF.R") # MARSHAL

# Update the crootbox executable file if needed
# MAC
#file.copy("inputs/crootbox_source/crootbox.out", 
#           "inputs/crootbox.out", 
#           overwrite = T)
# WINDOWS
# file.copy("inputs/crootbox_source/crootbox.exe", 
#           "inputs/crootbox.exe", 
#           overwrite = T)




########################################################################
# 2 : RUN SINGLE SIMULATION
########################################################################

# We load the default parameter sets for the CRootBox simulation 
rparam <- read_rparam(path = "inputs/crootbox_source/modelparameter/param.rparam")
pparam <- read_pparam(path = "inputs/crootbox_source/modelparameter/param.pparam")

# Check the parameter files
head(rparam)

# Update the parameters for the simulation in the R dataframe
rparam$val1[rparam$name == "primaryroot" & rparam$param == "r"] <- 2 

print(rparam$val1[rparam$name == "primaryroot" & rparam$param == "r"])

# update the input text files 
write_rparam(rparam, "inputs/param.rparam")
write_pparam(pparam, "inputs/param.pparam")

# Run crootbox
#system("inputs/crootbox.out") # Run crootbox for mac
system("inputs/crootbox.exe") # Run crootbox for windows

# Get the results of the simulation
rootsystem <- fread("outputs/current_rootsystem.txt", header = T)

# Plot the root system
rootsystem %>%
  ggplot() +
  theme_classic() +
  geom_segment(aes(x = x1, y = z1, xend = x2, yend = z2), alpha=0.9) +
  coord_fixed()

# We load the default parameter sets for the MARSHAL simulation 
psiCollar <- -15000
soil <- read_csv("inputs/soil.csv")
conductivities <- read_csv("inputs/conductivities.csv")

head(conductivities)
print(soil)

# Run MARSHAL
hydraulics <- getSUF(rootsystem, conductivities, soil, psiCollar)

# Map the segment-scale results on the root system
hydraulic_archi <- hydraulics$root_system
hydraulic_archi$suf <- hydraulics$suf[,1]
hydraulic_archi$kr <- hydraulics$kr[,1]
hydraulic_archi$kx <- hydraulics$kx[,1]
hydraulic_archi$jr <- hydraulics$jr[,1]
hydraulic_archi$jxl <- hydraulics$jxl[,1]

# Get the plant scale results

print(paste0("Root system conductivity = ",hydraulics$krs))
print(paste0("Root system transpiration = ",hydraulics$tact))


# Plot the results
hydraulic_archi %>%
  ggplot() +
  theme_classic() +
  geom_segment(aes(x = x1, y = z1, xend = x2, 
                   yend = z2, col = kx), alpha=0.9) +
  scale_color_viridis_c() + 
  coord_fixed()







########################################################################
# 3 : SET THE SIMULATION PARAMETERS FOR SENSITIVITY ANALYSIS
########################################################################

# CROOTBOX PARAMETERS

# A. We store the input parameters we want to change in vectors, so 
# we can loop on these afterwards
vitesse_primaire_vec <- c(1:4) # 3 growth rates for primary roots
vitesse_secondaire_vec <- c(0.1, 0.2, 0.3, 0.4, 0.5) # 3 growth rates for secondary roots
simulation_time <- 30

# B. We load the default parameter sets for the simulation 
rparam <- read_rparam(path = "inputs/crootbox_source/modelparameter/param.rparam")
pparam <- read_pparam(path = "inputs/crootbox_source/modelparameter/param.pparam")

# C. We create variables that will contain the 
# results of our crootbox simulations
all_rootsystems <- NULL
n_tot_simulation <- length(vitesse_primaire_vec) * length(vitesse_secondaire_vec)
n_cr_sim <- 0


########################################################################
# 4 : RUN CROOTBOX
########################################################################

# We loop over the input parameters vectors (see 2.A)
for(vp in vitesse_primaire_vec){
  for(vs in vitesse_secondaire_vec){
  
    # Output the advancement in the simulation
    n_cr_sim <- n_cr_sim + 1
    print(paste0(n_cr_sim, " / ", n_tot_simulation, " crootbox sims"))
    
    # Modify parmeters
    
    # update "vitesse croissance primaire"
    rparam$val1[rparam$name == "primaryroot" & rparam$param == "r"] <- vp 
    
    # update "vitesse croissance secondaire"
    rparam$val1[rparam$type == 2 & rparam$param == "r"] <- vs 
    
    # update the simulation time
    pparam$val1[pparam$param == "simtime"] <- simulation_time
  
    # update the input text files 
    write_rparam(rparam, "inputs/param.rparam")
    write_pparam(pparam, "inputs/param.pparam")
    
    # Run crootbox
    #system("inputs/crootbox.out") # Run crootbox for Mac
    system("inputs/crootbox.exe") # Run crootbox for windows
    
    # Load the simulated data into R to process it and to store it for further use
    current_rootsystem <- fread("outputs/current_rootsystem.txt", header = T)
    
    # We enrich the root system simulation data with metadata
    # This is needed to find back the information in the large data file 
    # at the end of the simulations
    current_rootsystem <- current_rootsystem %>% 
      mutate(vitesse_primaire = vp, 
             vitesse_secondaire = vs,
             simulation_id = n_cr_sim)
    
    # We can store all the root systems data in the same BIG data frame
    all_rootsystems <- rbind(all_rootsystems, current_rootsystem)
    
    
    # We store the root system simulation with a unique name
    write_csv(current_rootsystem, 
              paste0("outputs/rootsystems/rootsystem_",vp,"_",vs,".csv"))  

  } 
}

# Plot all the root systems

str(all_rootsystems)

all_rootsystems %>% 
  ggplot() + 
  theme_classic() +
  geom_segment(aes(x = x1, y = z1, xend = x2, yend = z2), alpha=0.9) +
  coord_fixed() + 
  facet_grid(vitesse_primaire ~ vitesse_secondaire)











