#Author: Melina Takvorian, melina.takvorian@colostate.edu

#Last edited: 11/10/2025

#DESCRIPTION:
#This script takes the output from the Search_USFWS&NOAA,
#and pulls in the spatial data for the USFWS species to start extracting shapefiles


#Setup ----
setwd("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_RangeMaps/SearchSpecies_USFWS&NOAA")

##install packages----
# Package names
packages <- c("readxl","tidyverse","tidyr","dplyr","stringr","sf","terra","tmap", "leaflet")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#Read in files ----
usfws_1 <- st_read("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/AF Viewer OY5 Melina and Gillian/T&E Range Map Data/ExtractBMGR/USFWS_rawdata/usfws_complete_species_current_range_1.shp")
usfws_2 <- st_read("C:/Users/melinata/Downloads/usfws_complete_species_current_range/usfws_complete_species_current_range_2.shp")
matches_complete <- read_csv("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_RangeMaps/SearchSpecies_USFWS&NOAA/Output/MATCHES_full_scinames.csv")
cemml_raw <- read_xlsx("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/RangeMaps/SearchSpecies_USFWS&NOAA/Rawdata/Species Assessments - Viewer.xlsx")

#PULL INDIVIDUAL SPECIES Data ----
##usfws 1 ----
MGL1_A <- matrix(nrow = 1, ncol = 11)
colnames(MGL1_A) <-  colnames(usfws_1)
MGL1_A <- as.data.frame(MGL1_A)

num <- 0
for(i in 1:nrow(usfws_1)){
  
  newdf <- usfws_1[i,]
  sci_name <- newdf$SCINAME
  
  if(!sci_name %in% matches_complete$USFWS) next  #[CHECK IF NAME IS NOT IN THE MATCHES DATASET]
  num <- num+1
  
  com_name <- newdf$COMNAME
  com_name <- gsub(" ", "", tools::toTitleCase(com_name))
  
  speciesID <- which(cemml_raw$`Species Latin Name` == sci_name)
  speciesID <- as.numeric(speciesID)
  
  print(paste(com_name, "speciesID: ",speciesID, " | has been added"))
  
  MGL1_A[num, ] <- usfws_1[i,]
}

##usfws 2 ----
MGL2_A <- matrix(nrow = 1, ncol = 11)
colnames(MGL2_A) <-  colnames(usfws_1)
MGL2_A <- as.data.frame(MGL2_A)

num <- 0
for(i in 1:nrow(usfws_2)){
  
  newdf <- usfws_2[i,]
  sci_name <- newdf$SCINAME
  
  if(!sci_name %in% matches_complete$USFWS) next  #[CHECK IF NAME IS NOT IN THE MATCHES DATASET]
  num <- num+1
  
  com_name <- newdf$COMNAME
  com_name <- gsub(" ", "", tools::toTitleCase(com_name))
  
  speciesID <- which(cemml_raw$`Species Latin Name` == sci_name)
  speciesID <- as.numeric(speciesID)
  
  print(paste(com_name, "speciesID: ",speciesID, " | has been added"))
  
  MGL2_A[num, ] <- usfws_2[i,]
}

MGL_B <- bind_rows(MGL1_A, MGL2_A)

#MAKE GROUP OF SEPERATES ----
MGL_B_mult <- MGL_B  %>%
  group_by(COMNAME) %>%
  filter(n() > 1) %>%
  ungroup() 

MGL_B_mult <- st_sf(MGL_B_mult)


#create list of multiples similar ----

MGL_singles <- MGL_B_mult[0,] #where the final, merged data will go into

# Get unique species names
species_list <- unique(MGL_B_mult$SCINAME)

for (species in species_list) {
  # Get all rows for this species
  species_rows <- MGL_B_mult[MGL_B_mult$SCINAME == species, ]
  
  # Merge geometries
  merged_geom <- st_union(species_rows)
  
  # Take attributes from first row
  merged_row <- species_rows[1, ]
  merged_row$geometry <- merged_geom
  
  # Append to final sf
  MGL_singles <- rbind(MGL_singles, merged_row)
}

#delete the occurrences of species from species_list ----
MGL_C <- MGL_B %>% 
  filter(!SCINAME %in% species_list)

#add in final species files ----
MGL_D <- rbind(MGL_C, MGL_singles)

#set coordinate reference system to be the same as the original data`
#transform to spatial dataset
MGL_D <- st_sf(MGL_D)

#add CRS`
st_crs(MGL_D) <- st_crs(usfws_1)

##add speciesID
for(i in 1:nrow(MGL_D)){
  
  sci_name <- MGL_D$SCINAME[i] #get name for ID
  
  speciesID <- which(cemml_raw$`Species Latin Name` == sci_name) #find speciesID from list
  speciesID <- as.numeric(speciesID) #convert the ID to a number
  
  MGL_D$speciesID[i] <- speciesID #assign this ID to a new column
  
  print(paste(sci_name, "speciesID: ", speciesID, " | has been added"))
}

#EXPORTING ----

#EXPORT ALL FILES IN LIST THAT HAVE NOT BEEN DONE ALREADY

#store folder path
shapefile_folder <- "N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/AF Viewer OY5 Melina and Gillian/T&E Range Map Data/SHAPEFILES"

done_files <- "N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/AF Viewer OY5 Melina and Gillian/T&E Range Map Data/SHAPEFILES/Temporary"

#identify files that are already completed
speciesdone <- list.files(path = done_files, pattern = "\\.shp$")

#remove .shp ending to be able to run comparison later
for(i in 1:length(speciesdone)){
  strL <- str_length(speciesdone[i])
  newL <- strL - 4
  speciesdone[i] <- str_sub(speciesdone[i],1,newL)
}


for(i in 1:nrow(MGL_D)){
  
  #pull list item out so that it can be saved individually
  species_pull <- MGL_D[i,]
  
  ##pull species common name ----
  name <- species_pull$COMNAME
  
  if(name == "No common name"){
    name <- species_pull$SCINAME
  }
  
  name <- gsub(" ", "", tools::toTitleCase(name))
  name <- gsub("'", "", name)
  
  #CHECK TO SEE IF ALREADY COMPLETED -> IF NOT, create unique name for file
  if(name %in% speciesdone) next #SKIP THIS SPECIES BECAUSE IT IS ALREADY DONE and in Temporary folder
  
  #create name using name
  shapefile_location <- paste0(shapefile_folder,"/", name, ".shp")
  
  #export shapefile to file path
  st_write(species_pull, shapefile_location, append=FALSE)
  
}

#make csv for logging in the table for Adam
export_csv <- MGL_D[ ,c(6,7,12)] #only keep common name, scientific name, speciesID
shapefile_location <- paste0(shapefile_folder,"/", "usfws_pull.csv")
write.csv(export_csv, shapefile_location)

#example of plotting a shapefile ----

# cactus <- st_read("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/AF Viewer OY5 Melina and Gillian/T&E Range Map Data/SHAPEFILES/AcuñaCactus.shp")
# cactus_old <- st_read("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/AF Viewer OY5 Melina and Gillian/T&E Range Map Data/SHAPEFILES/Temporary/AcuñaCactus.shp")
# 
# tmap_mode("view")
# tm_shape(World, bbox = st_bbox(cactus_old)) + 
#   tm_polygons(fill = "gray90", col = "white") +  # background map
#   
#   tm_shape(cactus) +
#   tm_borders(col = "blue", lwd = 2) +
#   tm_fill(col = "blue", alpha = 0.3) +
# 
# tm_shape(cactus_old) +
#   tm_borders(col = "green", lwd = 2) +
#   tm_fill(col = "green", alpha = 0.3)
#   

