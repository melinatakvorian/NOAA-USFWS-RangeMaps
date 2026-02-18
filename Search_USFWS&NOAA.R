#Author: Melina Takvorian, melina.takvorian@colostate.edu

#Last edited: 10/06/2025

#DESCRIPTION: 
#This R Script uses the list of all T&E Species from installations going to be analyzed,
#and runs a search against the NOAA Endangered Species web map layer and the USFWS endangered species range layer.
#The R script produces CSV files with the list of T&E species of interest that NOAA and USFWS have range data for.
#These CSV files can be viewed here:
#N:\RStor\CEMML\ClimateChange\0_Natural Resources Teams\Wildlife\_RangeMaps\NOAA-USFWS-RangeMaps\Output

#These species-match tests provide us an idea of where we can pull range data from to display in the dashboards.
#The summaries of the match-tests for NOAA and USFWS, as well as the manual search of the IUCN and eBird databases,
#can be viewed in the excel "RangeMapSearchTallies.xlsx" in the T&E Range Map data folder.

#NOTE TO USER: 
#Please ensure that the latest version of the smartsheet list of species
#is the same name as what is written in the script under "import datasets" for cemml_raw.
#If it different, please type the correct name in the quotation marks, as shown below: 
#read_xlsx("...AF Viewer OY5 Melina and Gillian/NEW EXCEL NAME").

#SETUP----
##set working directory to pull the files----
setwd("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_RangeMaps/NOAA-USFWS-RangeMaps")

##install packages----
# Package names
packages <- c("readxl","tidyr","dplyr","stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##import datasets ----
noaa_raw <- read.csv("Rawdata/EndangeredSpeciesRange_AttrTable.csv")
  #this data comes from: https://www.arcgis.com/home/item.html?id=2c0a74713eb04ae5921fca27c854a331
usfws_raw <- read.csv("Rawdata/USFWS_rangemaps.csv")
usfws2_raw <- read.csv("Rawdata/USFWS_rangemaps2.csv")
  #this dataset comes from: https://ecos.fws.gov/docs/species/shapefiles/usfws_complete_species_current_range.zip
cemml_raw <- read_xlsx("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_Excel Files For Viewer/Species Assessments - Viewer.xlsx")

##trim CEMML master list dataset----
# remove spaces in column names that we will keep because it is annoying to deal with in R
cemml_raw <- cemml_raw %>% rename(
  "Species.Common.Name" = `Species Common Name`,
  "Species.Latin.Name" = `Species Latin Name`,
  "Species.ID" = `Species ID#`
)

# remove the columns that are tallying each base up
cemml_raw <- cemml_raw %>% select(Species.Common.Name,Species.Latin.Name,Species.ID)

#INITIALIZE check_alt_names FUNCTION ----
#this function takes scientific names that have alternates, like "Phoebastria (=Diomedea) albatrus"
#and edits the existing cell to "Phoebastria albatrus" AND 
#CREATES A NEW ROW for the alternate name "Diomedea albatrus".
#This way, there are two chances for the species to get matched, instead of zero,
#had the original formatting not been changed.

check_alternate_names <- function(df, name_col) {
  # Convert column name string to symbol for tidy evaluation
  name_sym <- rlang::sym(name_col)
  
  df_expanded <- df %>%
    rowwise() %>%
    do({
      name <- as.character(.[[name_col]])
      # Check for pattern: Firstname (=Middlename) Lastname
      if (str_detect(name, "\\(=.+?\\)")) {
        # Extract parts
        first <- str_extract(name, "^[^ ]+")
        middle <- str_extract(name, "(?<=\\(=)[^\\)]+")
        last <- str_extract(name, "[^ ]+$")
        
        # Create two versions of the name
        name1 <- paste(first, last)
        name2 <- paste(middle, last)
        
        # Create two new rows with updated name
        row1 <- as.data.frame(., stringsAsFactors = FALSE)
        row2 <- row1
        
        row1[[name_col]] <- name1
        row2[[name_col]] <- name2
        
        bind_rows(row1, row2)
      } else {
        as.data.frame(., stringsAsFactors = FALSE)
      }
    }) %>%
    ungroup()
  
  return(df_expanded)
}

#MATCH TEST #1----
#Matches the full name to the full name 
#i.e. matches 'Genus species subspecies' to 'Genus species subspecies'

##CEMML & NOAA ----

#run check_alternate_names function for NOAA
noaa_raw_expanded <- check_alternate_names(noaa_raw, "Scientific_Name")

#run match
match_cemml_noaa <- match(cemml_raw$Species.Latin.Name, noaa_raw_expanded$Scientific_Name)

#create list of matched names - NOAA
realmatchnoaa <- list()

for(i in 1:length(match_cemml_noaa)){
  val <- match_cemml_noaa[i]
  realmatchnoaa[i] <- noaa_raw_expanded$Scientific_Name[val]
}

realmatchnoaa <- realmatchnoaa[!is.na(realmatchnoaa)]  


##CEMML & USFWS 1/2 ----
#run check_alternate_names function for uSFWS
usfws_raw_expanded <- check_alternate_names(usfws_raw, "SCINAME")  

#run match
match_cemml_usfws <- match(cemml_raw$Species.Latin.Name, usfws_raw_expanded$SCINAME)

#create list of matched names - USFWS
realmatchUSFWS <- list()

for(i in 1:length(match_cemml_usfws)){
  val <- match_cemml_usfws[i]
  realmatchUSFWS[i] <- usfws_raw_expanded$SCINAME[val]
}

realmatchUSFWS <- realmatchUSFWS[!is.na(realmatchUSFWS)]

##populate table for LONG name matches ----
longname <- as.data.frame(matrix(nrow=nrow(cemml_raw),ncol=2))
longname <- longname %>% dplyr::rename(
  "USFWS" = V1, 
  "NOAA" = V2
)

#fill in USFWS column
for(i in 1:length(realmatchUSFWS)){
  val <- realmatchUSFWS[[i]]
  longname[i,1] <- val
} 

#fill in NOAA column
for(i in 1:length(realmatchnoaa)){
  val <- realmatchnoaa[[i]]
  longname[i,2] <- val
} 

##CEMML & USFWS 2/2 ----
#run check_alternate_names function for uSFWS
usfws2_raw_expanded <- check_alternate_names(usfws2_raw, "SCINAME")  

#run match
match_cemml_usfws2 <- match(cemml_raw$Species.Latin.Name, usfws2_raw_expanded$SCINAME)

#create list of matched names - USFWS
realmatchUSFWS2 <- list()

for(i in 1:length(match_cemml_usfws)){
  val <- match_cemml_usfws[i]
  realmatchUSFWS2[i] <- usfws2_raw_expanded$SCINAME[val]
}

realmatchUSFWS2 <- realmatchUSFWS2[!is.na(realmatchUSFWS2)]

#table creation

#fill in USFWS column
rownum <- as.numeric(length(realmatchUSFWS))

for(i in 1:length(realmatchUSFWS2)){
  val <- realmatchUSFWS2[[i]]
  rownum <- rownum + 1
  longname[rownum,1] <- val
} 

##CSV creation for LONG names ----
finaldata_longname <- longname

write.csv(finaldata_longname, "Output/MATCHES_full_scinames.csv") #make clear this is the complete dataset

#MATCH TEST #2 ----
#ONLY 'GENUS SPECIES' LEVEL MATCHES
#i.e. matches 'Genus species' to 'Genus species'

#transform data to 'Genus species'
cemml_raw <- cemml_raw %>%
  mutate(short_name = str_trim(str_extract(Species.Latin.Name, "^\\S+\\s+\\S+")))

noaa_raw_expanded <- noaa_raw_expanded %>% 
  mutate(short_name = str_trim(str_extract(Scientific_Name, "^\\S+\\s+\\S+")))

##SHORTER CEMML & NOAA ----
#run match
match_cemmlSHORT_noaa <- match(cemml_raw$short_name, noaa_raw_expanded$short_name)

#create list of matched names - NOAA
realmatchSHORTNOAA <- list()

for(i in 1:length(match_cemml_noaa)){
  val <- match_cemmlSHORT_noaa[i]
  realmatchSHORTNOAA[i] <- noaa_raw_expanded$short_name[val]
}

realmatchSHORTNOAA <- realmatchSHORTNOAA[!is.na(realmatchSHORTNOAA)]  


##SHORTER CEMML & USFWS ----

#transform data to 'Genus species'

#CEMML data is already done (see NOAA section above)
usfws_raw_expanded <- usfws_raw_expanded %>% 
  mutate(short_name = str_trim(str_extract(SCINAME, "^\\S+\\s+\\S+")))

#run match
match_cemmlSHORT_usfws <- match(cemml_raw$short_name, usfws_raw_expanded$short_name)

#create list of matched names - USFWS
realmatchSHORTusfws <- list()

for(i in 1:length(match_cemmlSHORT_usfws)){
  val <- match_cemmlSHORT_usfws[i]
  realmatchSHORTusfws[i] <- usfws_raw_expanded$short_name[val]
}

realmatchSHORTusfws <- realmatchSHORTusfws[!is.na(realmatchSHORTusfws)]  


##populate table for SHORT name matches----
shortname <- as.data.frame(matrix(nrow=nrow(Cemml_raw),ncol=2))
shortname <- shortname %>% dplyr::rename(
  "USFWS" = V1, 
  "NOAA" = V2
)

#fill in USFWS column
for(i in 1:length(realmatchSHORTusfws)){
  val <- realmatchSHORTusfws[[i]]
  shortname[i,1] <- val
} 

#fill in NOAA column
for(i in 1:length(realmatchSHORTNOAA)){
  val <- realmatchSHORTNOAA[[i]]
  shortname[i,2] <- val
} 

##SHORTER CEMML & USFWS 2/2 ----

#transform data to 'Genus species'

#CEMML data is already done (see NOAA section above)
usfws2_raw_expanded <- usfws2_raw_expanded %>% 
  mutate(short_name = str_trim(str_extract(SCINAME, "^\\S+\\s+\\S+")))

#run match
match_cemmlSHORT_usfws2 <- match(cemml_raw$short_name, usfws2_raw_expanded$short_name)

#create list of matched names - USFWS
realmatchSHORTusfws2 <- list()

for(i in 1:length(match_cemmlSHORT_usfws2)){
  val <- match_cemmlSHORT_usfws2[i]
  realmatchSHORTusfws2[i] <- usfws2_raw_expanded$short_name[val]
}

realmatchSHORTusfws2 <- realmatchSHORTusfws2[!is.na(realmatchSHORTusfws2)] 

#fill in USFWS column
rownum <- as.numeric(length(realmatchSHORTusfws))

for(i in 1:length(realmatchSHORTusfws2)){
  val <- realmatchSHORTusfws2[[i]]
  rownum <- rownum + 1
  shortname[rownum,1] <- val
} 

## CSV creation for SHORT names ----
finaldata_shortname <- shortname #make clear this is the complete dataset

write.csv(finaldata_shortname, "Output/MATCHES_short_scinames.csv")
write.csv(cemml_raw, "Output/LIST_ALL_shortened_scinames.csv")