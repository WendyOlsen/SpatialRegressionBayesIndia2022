####August 2023 University of Manchester

####Script for ICC calculations for with both poisson and Logit MLM
####Authors: Diego Perez Ruiz, Madhu Chauhan, Wendy Olsen

### This script is the starting point for the Spatial Besag-Yorke-Mollie-2 estimation activities.

### This first cleaning script clean data-- 

###--ensure only sexes are male and female 
###-- keep a very wide set of adult age groups
###data for individuals aged 14-max age, --double digitizing state and 
###district codes, --making a combined district code for district name-matching, 
###adding count of individuals per district as sample size; (this is an unweighted adult case count)
###adding state names to the dataset, extracting state code and name from 2011 
###shape file.

setwd("C:/data/SpatialBayesian2023newFiles") 
#revise the working directory to place saved data in the correct folder.

library(dplyr)
library(tidyr)
library(stringr)
library(withr)
library(readr)
library(readxl)
#install.packages("readstata13")
library(readstata13)
#loading the PLFS1718 data from where it was merged using Stata.  All persons. )
IndiaPLFS201718 <-  read.dta13("C:/data/PLFS20172018/plfs_2018version13.dta")

                           # previously we used IndiaPLFS20178adults1565.csv")
                                #you might use library(readstata13)
                                #dat <- read.dta13("path to file.dta")
             #and if you want to work entirely in stata, you can use this: 
                                #save.dta13(dat, file="newfile.dta")  
#but we have stata 18 so we used that to make a file called by the stata v13 in its filename.
#Filtering for only males and females
IndiaPLFS201718 <- IndiaPLFS201718 %>% filter( sex != "3")

#Filtering for people less than 25 years of age (14-25)
#IndiaPLFS201718 <- IndiaPLFS201718 %>% filter( age <= "24")

#code for dist and state concatenation as distcode variable, also making 
#dist code and state codes 2 digit values
IndiaPLFS201718 <-IndiaPLFS201718 %>% 
  mutate(state = str_pad(string = state, pad = '0', width = 2), 
         district = str_pad(string = district, pad = '0', width = 2)) %>% 
  unite(distcode, state, district, sep='', remove = FALSE) 

save(IndiaPLFS201718, file="IndiaPLFS201718.rds")

#Checking if the code worked fine and if we have two digit state Id's, district 
#ID's and a combination of state and district codes as distcode in the table. 
#This dist code can be used to map the districts for spatial analysis in the future.

table(IndiaPLFS201718$distcode)

summary(unique(IndiaPLFS201718$state))

IndiaPLFS201718$distcode

#Adding state names to the state code from the data layout file
#Importing data layout state code sheet from PLFS

#first prepare 2 files for the join (move locations)
Data_LayoutPLFSinfo <- read_excel("C:/data/PLFS20172018/Data_LayoutPLFS.xlsx", sheet = "State code")

colnames(Data_LayoutPLFSinfo) = Data_LayoutPLFSinfo[1, ] # the first row will be the header
Data_LayoutPLFSinfo = Data_LayoutPLFSinfo[-1, ]          # removing the first row.
colnames(Data_LayoutPLFSinfo)<- c("state","stateName")
Data_LayoutPLFSinfo$state<-as.numeric(Data_LayoutPLFSinfo$state)

Data_LayoutPLFSinfo <-Data_LayoutPLFSinfo %>% 
  mutate(state = str_pad(string = state, pad = '0', width = 2))
IndiaPLFS201718<- left_join(IndiaPLFS201718, Data_LayoutPLFSinfo, by= 'state')

#Creating the corresponding RDS file with state, dist,
#distcode, rural, sex, female, medwork, monthly expenditure,  MPerCapitaExp, and log of MPCE for ease in usability in future.

IndiaPLFS201718  <- IndiaPLFS201718  %>% 
  dplyr::select( psid, state, stateName, district, distcode, age, sex, female, rural, medwork, hhsize, comb_wt , mpce, logRsincpc)

#note we are using the /data folder inside the workshop activity folder. The command below cannot use / key.

#      #      #      The below 'save' places the main all-persons file into two folders      #      #      #  
saveRDS(IndiaPLFS201718, file = "IndiaPLFS201718.rds")
setwd("c:/data/SpatialBayesian2023newFiles/data")
saveRDS(IndiaPLFS201718, file = "IndiaPLFS201718.rds")


# Part 2
# Aim 2:  getting a List of all the district codes from the census2011 shp file and 
#creating an rds for the same to compare and check the missing districts in the
#shape file and to use it in future to map with the dist codes in the PLFS dataframe.

setwd("c:/data/SpatialBayesian2023newFiles/maps-master/Districts/Census_2011")

library(sf)

India_Districts<-  st_read("2011_Dist.shp")

India2011Census<-data.frame(district = India_Districts$DISTRICT,
                censuscode = India_Districts$censuscode,statename= India_Districts$ST_NM)

India2011Census<-unique(India2011Census)

setwd("c:/data/SpatialBayesian2023newFiles/data")
saveRDS(India2011Census, file = "India2011Census.RDS")

                  #The below not needed 2023.

#add the sample size as a variable to the dataset with total counts of 
#observations in each district in the data [note, we need to make the count
#whilst the age-range covered in the data is correct, see above to limit ages.]

#IndiaPLFS201718<-IndiaPLFS201718 %>%
#  add_count(distcode, name = 'sample_size')

#Create a new RDS file with district code and district sample for future usability.

#distSampleCount<-IndiaPLFS201718%>%distinct(distcode, sample_size)
#sum(distSampleCount$sample_size)
#saveRDS(distSampleCount, file = "Sample_Sizes.rds")
#write.csv(distSampleCount, "Sample_Sizes.csv", row.names=FALSE, quote=FALSE)



