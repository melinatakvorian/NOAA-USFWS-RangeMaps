#Title: Create Shapefiles from NOAA dataset on ArcGIS Online

#Author: Melina Takvorian, melina.takvorian@colostate.edu

#Updated: 11/12/2025


#Setup ----

setwd("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/AF Viewer OY5 Melina and Gillian/T&E Range Map Data/SHAPEFILES")

##install packages----
# Package names
packages <- c("readxl","tidyverse","tidyr","dplyr","stringr","sf","terra","tmap", "leaflet", "arcgis", "arcgisbinding")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##installing and loading ArcGIS-R bridge (if not already on computer)----
install.packages("arcgis", repos = c("https://r-arcgis.r-universe.dev", "https://cloud.r-project.org"))
library(arcgis)

install.packages("arcgisbinding", repos = "https://r.esri.com", type = "win.binary")
library(arcgisbinding)

#Data import ----
##CEMML list of species ----
matches_complete <- read_csv("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_RangeMaps/SearchSpecies_USFWS&NOAA/Output/MATCHES_full_scinames.csv")
cemml_raw <- read_xlsx("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_Excel Files For Viewer/Species Assessments - Viewer.xlsx")

##Pulling NOAA data from ArcGIs Online ----
# (not being used) noaa_dataset1 <- arc_read("https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/Species_Ranges/FeatureServer/1")
#does not contain geometry, 'endangered species range areas'

noaa_dataset3 <- arc_read("https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/Species_Ranges/FeatureServer/3")
# 'diced endangered species range areas'
#IF THIS DOESNT WORK, RE-INSTALL THE ARCGIS PACKAGE

# (not being used) noaa_dataset4 <- arc_read("https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/Species_Ranges/FeatureServer/4")
# 'generalized endangered species range areas'

noaa_dataset_0 <- arc.open("https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/Species_Ranges/FeatureServer")
#should be a dataset, not whatever that is
#requires a /ID at the end, which is either 1,3,4 but none of them are successfully converted to

#Matches ----
#delete the values that are ONLY in NOAA
usfws_list <- matches_complete$USFWS
noaa_list <- matches_complete$NOAA

unique_matches <- noaa_list[!noaa_list %in% usfws_list]

noaa_A <- noaa_dataset3[0,]

num <- 0

for(i in 1:nrow(noaa_dataset3)){
  
  newdf <- noaa_dataset3[i,]
  sci_name <- newdf$Scientific_Name
  
  if(!sci_name %in% unique_matches) next  #[CHECK IF NAME IS NOT IN THE MATCHES DATASET]
  num <- num+1
  
  com_name <- newdf$Common_Name
  com_name <- gsub(" ", "", tools::toTitleCase(com_name))
  
  speciesID <- which(cemml_raw$`Species Latin Name` == sci_name)
  speciesID <- as.numeric(speciesID)
  
  print(paste(com_name, "speciesID: ",speciesID, " | has been added"))
  
  noaa_A[num, ] <- noaa_dataset3[i,]
}


#Make seperate table of species with multiple polygons ----
noaa_A_mult <- noaa_A  %>%
  group_by(Common_Name) %>%
  filter(n() > 1) %>%
  ungroup() 

noaa_A_mult <- st_sf(noaa_A_mult)

noaa_A_mult <- noaa_A_mult %>% relocate(geometry, .before = Scientific_Name_url)


#create dataframe with merged polygons for each species from above ----

#initialize dataset
noaa_singles <- noaa_A_mult[0,] #where the final, merged data will go into

#Get unique species names
species_list <- unique(noaa_A_mult$Scientific_Name)

#merge
for (species in species_list) {
  
  #need to only join the ones that are not empty
  if(st_is_empty(noaa_A_mult$geometry[species])) next
  
  # Get all rows for this species
  species_rows <- noaa_A_mult[noaa_A_mult$Scientific_Name == species, ]
  
  # Merge geometries
  merged_geom <- st_union(species_rows)
  
  # Take attributes from first row
  merged_row <- species_rows[1, ]
  merged_row$geometry <- merged_geom
  
  # Append to final sf
  noaa_singles <- rbind(noaa_singles, merged_row)
}

# Loop through each species in the list
for (species in species_list) {
  
  # Filter rows for the current species
  species_rows <- noaa_A_mult[noaa_A_mult$Scientific_Name == species, ]
  
  # Remove rows with empty geometries
  species_rows <- species_rows[!st_is_empty(species_rows$geometry), ]
  
  # Check if there are any rows left after filtering
  if (nrow(species_rows) == 0) next
  
  # Merge geometries for the species
  merged_geom <- st_union(species_rows$geometry)
  
  #    Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
  #      Loop 1 is not valid: Edge 9 is degenerate (duplicate vertex)
  
  # Take attributes from the first row
  merged_row <- species_rows[1, ]
  merged_row$geometry <- merged_geom
  
  # Append to final sf object
  noaa_singles <- rbind(noaa_singles, merged_row)
}

invalid_geom <- species_rows[!st_is_valid(species_rows$geometry), ]
print(invalid_geom)
plot(invalid_geom$geometry)


tmap_mode("view")
tm_shape(World, bbox = st_bbox(invalid_geom)) +
  tm_polygons(fill = "gray90", col = "white") +  # background map
  
  tm_shape(invalid_geom) +
  tm_borders(col = "blue", lwd = 2) +
  tm_fill(col = "blue", alpha = 0.3) 

species_rows$geometry <- st_simplify(species_rows$geometry, dTolerance = 0.01)

#delete the occurrences of species from species_list ----
noaa_C <- noaa_B %>% 
  filter(!Scientific_Name %in% species_list)

#add in final species files ----
noaa_D <- rbind(noaa_C, noaa_singles)

#set coordinate reference system to be the same as the original data`
#transform to spatial dataset
noaa_D <- st_sf(noaa_D)

#add CRS`
st_crs(noaa_D) <- st_crs(noaa_A)

##add speciesID
for(i in 1:nrow(noaa_D)){
  
  sci_name <- noaa_D$Scientific_Name[i] #get name for ID
  
  speciesID <- which(cemml_raw$`Species Latin Name` == sci_name) #find speciesID from list
  speciesID <- as.numeric(speciesID) #convert the ID to a number
  
  noaa_D$speciesID[i] <- speciesID #assign this ID to a new column
  
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


for(i in 1:nrow(noaa_D)){
  
  #pull list item out so that it can be saved individually
  species_pull <- noaa_D[i,]
  
  ##pull species common name ----
  name <- species_pull$COMNAME
  
  if(name == "No common name"){
    name <- species_pull$Scientific_Name
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
export_csv <- noaa_D[ ,c(_,_,_)] #only keep common name, scientific name, speciesID
shapefile_location <- paste0(shapefile_folder,"/", "noaa_pull.csv")
write.csv(export_csv, shapefile_location)

