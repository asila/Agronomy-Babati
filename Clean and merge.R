library(xlsx)

library(misty) # For checking duplicates

library(dplyr)

setwd('~/Alliance/Job/Data')

# Step one: Get data ---- 

## ├ Which datasets?

### ├ Take stock?
list.files(pattern = '*.xlx*')

## ├ Read Agronomic Maize data ----
mze <- read.xlsx('Babati_Mz_Quality Analysis_AgroSurvey2021.xlsx', 1)


## ├ Read field survey data ----
#field <- read.csv('Babati_agronomic_2021.csv')

## ├ Read updated field Agronomic data ----
field <- read.xlsx('Agronomic survey_cleaned_May_2022_as.xlsx', 1)

field[1:5,]

# Exclude farms with labrecord column labelled as "Discard"
dim(field)

field <- field %>%
  filter(Labrecord != 'Discard')

dim(field)
## ├ Check for duplicates ----
k <- which(duplicated(field[,'gpslongitude'])== TRUE)

## ├ Resolved for farmer Hajara Salim Andrea is a duplicate ----

# Delete the other duplicate record missing HHID

p <- which(field[,"gpslongitude"] %in% field[k,'gpslongitude'])

field[p,1:5]

names(field)
# Disregarding merging names from Powell use the updated mze table names
field0 <-  field[,1:141]
mze[1:4,]

# Linking field0
field_mze <- merge(field0,mze, by.x = 'Labrecord', by.y = 'FARMER')

# Ensure we have only unique records

field_mze <- unique(field_mze)

dim(field_mze)

names(field_mze)

summary(field_mze$WT_GRAINS)

k <- which(is.na(field_mze$WT_GRAINS)== TRUE)

field_mze[k,1:5]

## Read soil data and merge to field_mze
soil <- read.xlsx('Soil Analysis_Babati2021 Data.xlsx', 1)

# Get farmers into uppercase
soil <- soil %>%
  mutate(Farmer.Name = toupper(Farmer.Name))

## Now merge soil data to merged survey and crop micronutrients
#field_msoil <- merge(field_mze, soil, by.x  = 'Fieldrecord', by.y = 'Farmer.Name')

# Read merge name xlsx file obtained from Powell
names <- read.xlsx('Merge names.xlsx', 1)

# First merge names file with field survey file 
n_field <- merge(names, field, by.x = 'Fieldrecord', by.y = 'FARMER', all.y = TRUE)

n_field[,1:4]
# Next merge n_field above with plant concentration data from lab

data <- merge(n_field, mze, by.x = 'Labrecord', by.y = 'FARMER')

data_p <- merge(n_field, mze, by.x = 'Labrecord', by.y = 'FARMER', all.y = TRUE)

# save the two linked datasets
write.csv(data, file = './Cleaned/Field_survey_quality.csv', row.names = FALSE)

write.csv(data_p, file = './Cleaned/Field_survey_by_quality.csv', row.names = FALSE)


with(data, plot(gpslongitude , gpslatitude))

# Check for duplicated lat long
gps <- field %>%
  select(gpslatitude, gpslongitude, gpsaccuracy) %>%
  duplicated()


g <- which(gps == TRUE)

which(field$gpslatitude == field$gpslatitude[g])

# Further cleaned datap to get more farms
data <-  read.xlsx('~/Alliance/Job/Data/Cleaned/Agronomic survey_cleaned_May_2022_as.xlsx', 1)

soil <- read.xlsx('~/Alliance/Job/Data/Cleaned/Soil_wetchem_Babati2021.xlsx',1)

# Check for duplicates in the two tables
datad <- df.duplicated(data,Fieldrecord)

soild <- df.duplicated(soil,"Farmer Name_MK")

write.csv(soild, file = "~/Alliance/Job/Data/Cleaned/Soil data with duplicates.csv", row.names = FALSE)


# Get unique data frames for the two
datau <- df.unique(data,Fieldrecord)

soilu <- df.unique(soil,"Farmer Name_MK")


l <- which(!soilu$Farmer.Name_MK %in% datau$Fieldrecord)

write.csv(soilu[-l,], file = "~/Alliance/Job/Data/Cleaned/Soil data matching agronomic survey.csv", row.names = FALSE)

write.csv(soilu[l,], file = "~/Alliance/Job/Data/Cleaned/Soil data not matching agronomic survey.csv", row.names = FALSE)

p <- which(t(as.vector(soild["Farmer Name_MK"])) %in% soilu[l,"Farmer Name_MK"])

length(p)

soild <- df.duplicated(soil,"Farmer Name_MK")

# Create new variables for joining tables 1. datau and 2. soilu
# Get farmers into uppercase
soil <- soil %>%
  mutate(Farmer.Village = paste0(tolower(soil$`Farmer Name_MK`),'.',tolower(Village)))

data <- data %>%
  mutate(Farmer.Village = paste0(tolower(data$`Fieldrecord`),'.',tolower(village)))


vps <- merge(data, soil, by = 'Farmer.Village')

vps <- unique(vps) 

dim(vps)

z <- which(!soil$Farmer.Village %in% vps$Farmer.Village)


#write.csv(soil[z,], file = '~/Alliance/Job/Data/Cleaned/Soil not matched with survey data.csv', row.names = FALSE)


# Create new variables for joining tables 1. datau and 2. soilu
# Get farmers into uppercase
soil <- soil %>%
  mutate(Farmer.Village = paste0(tolower(soil$`Farmer Name`),'.',tolower(Village)))

data <- data %>%
  mutate(Farmer.Village = paste0(tolower(data$`Fieldrecord`),'.',tolower(village)))

vpsz <- merge(data, soil, by = 'Farmer.Village')

vpm <- bind_rows(vps,vpsz)

vpm <- unique(vpm) 

dim(vpm)

write.csv(vpm, file = '~/Alliance/Job/Data/Cleaned/Agronomic survey matched with soil.csv', row.names = FALSE)

dfv <- which(!data$Farmer.Village %in% vpm$Farmer.Village)

sfv <- which(as.vector(soil$Farmer.Village) %in% vpm$Farmer.Village)


write.csv(data[dfv,], file = '~/Alliance/Job/Data/Cleaned/Agronomic survey not matched with soil.csv', row.names = FALSE)

write.csv(soil[sfv,], file = '~/Alliance/Job/Data/Cleaned/Soil not matched with survey data.csv', row.names = FALSE)

# We realize there are more mismatches because Gallapo and Endanoga is one and the same village
# So we create a new field for village in both names and rename Endanoga as Gallapo

soil <- soil %>%
  mutate(Villages_calc = Village)

data <- data %>%
  mutate(Villages_calc = village)


soil <- soil %>%
  mutate(Farmer.Villagec = paste0(tolower(soil$`Farmer Name_MK`),'.',tolower(Villages_calc)))

data <- data %>%
  mutate(Farmer.Villagec = paste0(tolower(data$`Fieldrecord`),'.',tolower(Villages_calc)))

# Get rows with Endanoga and replace with Gallapo
unique(data$Village)

data <- data %>%
  mutate(Farmer.Villagec = gsub('endanoga','gallapo',data$Farmer.Villagec, ignore.case = TRUE))

soil <- soil %>%
  mutate(Farmer.Villagec = gsub('endanoga','gallapo',soil$Farmer.Villagec, ignore.case = TRUE))

vpsz <- merge(data, soil, by = 'Farmer.Villagec')

# Ensure we have a similar table with the remaining rows linking
#soil <- soil %>%
 # mutate(Farmer.Villagec = paste0(tolower(soil$`Farmer Name`),'.',tolower(Village)))

data$Farmer.Villagec[291] <- 'asha tekway.endanoga'

vps <- merge(data, soil, by = 'Farmer.Villagec')

vpm <- unique(vps) 

dim(vps)

vpm <- bind_rows(vps,vpsz)

vpm <- unique(vpm) 

dim(vpm)


dfv <- which(!as.vector(data$Farmer.Villagec) %in% vps$Farmer.Villagec)

sfv <- which(!as.vector(soil$Farmer.Villagec) %in% vps$Farmer.Villagec)


write.csv(vps, file = '~/Alliance/Job/Data/Cleaned/Agronomic survey matched with soil.csv', row.names = FALSE)

write.csv(data[dfv,], file = '~/Alliance/Job/Data/Cleaned/Agronomic survey not matched with soil.csv', row.names = FALSE)

write.csv(soil[sfv,], file = '~/Alliance/Job/Data/Cleaned/Soil not matched with survey data.csv', row.names = FALSE)

# Read the table from teh updated soil[sfv,]

soil.sfv <- read.xlsx('~/Alliance/Job/Data/Soil Analysis_Babati2021 Data_Updated_by_Mike.xlsx', 1)

galapo <- which(soil.sfv$Villages_calc == 'Galapo')

gash <- grep("Qash",soil.sfv$Villages_calc, ignore.case = TRUE)

soil.sfv$Villages_calc[gash]

# Replace the two mispelt villages

soil.sfv$Villages_calc[galapo] <- 'Gallapo'

soil.sfv$Villages_calc[gash] <- 'Gallapo'



a <- which(tolower(soil.sfv$`Farmer Name_MK`) %in% tolower(data$Fieldrecord))

b <- which(tolower(soil.sfv$`Farmer Name`) %in% tolower(data$Fieldrecord))

# Set bth records for soil.sfv$`Farmer Name to replace bth soil.sfv$`Farmer Name_MK

soil.sfv$`Farmer Name_MK`[b] <- soil.sfv$`Farmer Name`[b]

# Update a
a <- which(tolower(soil.sfv$`Farmer Name_MK`) %in% tolower(data$Fieldrecord))

# Then finally merge soil.sfv to data
soil.sfv <- soil.sfv %>%
  mutate(Villages_calc = Village)

soil.sfv <- soil.sfv %>%
  mutate(Farmer.Villagec = paste0(tolower(soil.sfv$`Farmer Name_MK`),'.',tolower(Villages_calc)))

soil.sfv <- soil.sfv %>%
  mutate(Farmer.Villagec = gsub('endanoga','gallapo',soil.sfv$Farmer.Villagec, ignore.case = TRUE))

vpsz.sfv <- merge(data, soil.sfv, by = 'Farmer.Villagec')

villaged <-  c('Sabilo', 'Gallapo', 'Gallapo','Gallapo','Gallapo')

soil.sfva <- soil.sfv[a,] %>%
  mutate(villaged = villaged)

# Read updated data tables and for survey and soil edited on 23/May/2022 with Kinyua
data.e <- read.csv('./cleaned/Agronomic survey not matched with soil_edited.csv')
soil.e <- read.xlsx('./cleaned/Soil Matched_Mike_edited.xlsx', 1)

# replace the name for galapo village with double l

soil.e <- soil.e %>%
  mutate(Villages_calc = gsub('galapo','gallapo',soil.e$Villages_calc, ignore.case = TRUE))

soil.e <- soil.e %>%
  mutate(Villages_calc = gsub('qash','gallapo',soil.e$Villages_calc, ignore.case = TRUE))

### Prepapre data tables for merging
soil.e <- soil.e %>%
  mutate(Farmer.Villagec = paste0(tolower(soil.e$`Farmer Name`),'.',tolower(Villages_calc)))

data.e <- data.e %>%
  mutate(Farmer.Villagec = paste0(tolower(data.e$`Fieldrecord`),'.',tolower(Villages_calc)))

# Get rows with Endanoga and replace with Gallapo
unique(data.e$Village)

data.e <- data.e %>%
  mutate(Farmer.Villagec = gsub('endanoga','gallapo',data.e$Farmer.Villagec, ignore.case = TRUE))

soil.e <- soil.e %>%
  mutate(Farmer.Villagec = gsub('endanoga','gallapo',soil.e$Farmer.Villagec, ignore.case = TRUE))

# work with soil shoing NA in column 34
d <- which(is.na(soil.e[,34]) == 'TRUE')

soil.e <- soil.e[d,]

vpsz.e0 <- merge(data.e, soil.e, by = 'Farmer.Villagec')

# Get for those using suffixed _MK
soil.e <- soil.e %>%
  mutate(Farmer.Villagec = paste0(tolower(soil.e$`Farmer Name_MK`),'.',tolower(Villages_calc)))

data.e <- data.e %>%
  mutate(Farmer.Villagec = paste0(tolower(data.e$`Fieldrecord`),'.',tolower(Villages_calc)))

# Get rows with Endanoga and replace with Gallapo
unique(data.e$Village)

data.e <- data.e %>%
  mutate(Farmer.Villagec = gsub('endanoga','gallapo',data.e$Farmer.Villagec, ignore.case = TRUE))

soil.e <- soil.e %>%
  mutate(Farmer.Villagec = gsub('endanoga','gallapo',soil.e$Farmer.Villagec, ignore.case = TRUE))

# work with soil showing NA in column 34
d <- which(is.na(soil.e[,34]) == 'TRUE')

soil.e <- soil.e[d,]

vpsz.e <- merge(data.e, soil.e, by = 'Farmer.Villagec')

all <- bind_rows(vpsz.e0,vpsz.e)

all <- unique(all)

## All agronomic dataa with soil are
ag_soil <- rbind(vps[,-c(177,179)],vpsz.sfv[,-c(177,179)],all[,-c(177,179,207)])

cbind(colnames(vps),colnames(vpsz.sfv), colnames(all[,-207]))

write.csv(ag_soil, file = '~/Alliance/Job/Data/Cleaned/Fully merged agronomy with soil.csv', row.names = FALSE)

# Finally get the file with soil data which has not been matched with survey
ag_soil <- read.csv('~/Alliance/Job/Data/Cleaned/Fully merged agronomy with soil.csv') 

ag_soil <- unique(ag_soil)

u <-  which(!soil.e$Farmer.Villagec %in% ag_soil$Farmer.Villagec)

f2 <- 'shabani mwiru.gallapo -> missing)'
f3 <- 'francis michael amo.gallapo -> from sabilo village in soil'
f4 <- 'humanness ally singia.gallapo -> missing'
f5 <- 'juma ramadhani.gallapo -> fixed'
f6 <- 'magret oim.gallapo -> missing'
f7 <- 'frida naasi.gallapo -> missing'
f8 <- 'omary hassan.gallapo -> missing'
f9 <- 'na.gallapo -> missing'
f10 <- 'boay barangi(felista nada\r\r\n).long -> missing'
f11 <- 'gabriel  kamrishi (marietha).long -> missing'
f12 <- 'na.long -> missing'
f13 <- 'stella slaa.long -> missing'
f14 <- 'na.gallapo -> missing'
f15 <- 'na.sabilo -> missing'
f16 <- 'ohn lory petro.sabilo -> missing'

soil.e$Farmer.Villagec[u]

v <- grep("lor", data$Farmer.Villagec,ignore.case = TRUE)

data$Farmer.Villagec[v]

write.csv(soil.e[u, ], file = './cleaned/Soil Matched_Mike_edited_still_missing', row.names = FALSE)

