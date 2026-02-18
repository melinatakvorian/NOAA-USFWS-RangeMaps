# NOAA-USFWS-RangeMaps

This R Script uses the list of all T&E Species from installations going to be analyzed, and runs a search against the NOAA Endangered Species web map layer and the USFWS endangered species range layer. The R script produces CSV files with the list of T&E species of interest that NOAA and USFWS have range data for. These CSV files can be viewed here:
N:\RStor\CEMML\ClimateChange\0_Natural Resources Teams\Wildlife\AF Viewer OY5 Melina and Gillian\T&E Range Map Data\SearchSpecies_USFWS&NOAA\Output

These species-match tests provide us an idea of where we can pull range data from to display in the dashboards. The summaries of the match-tests for NOAA and USFWS, as well as the manual search of the IUCN and eBird databases, can be viewed in the excel "RangeMapSearchTallies.xlsx" in the T&E Range Map data folder.

USAGE:
Running this script is very simple. There are just a few things to check before you do so to make sure you do not run into any errors! 
Checklist
- Have any of the following folder names changed? '_RangeMaps/SearchSpecies_USFWS&NOAA' and 'Ouput' or 'Rawdata'?
- Does the "RawData" folder include two excel spreadsheets, one with USFWS in its name and one with EndangeredSpecies?
- Does the '_Excel Files for Viewer' folder have the most up to date species list under the name "Species Assessments - Viewer"?

CONTACT:
Melina Takvorian, melina.takvorian@colostate.edu
