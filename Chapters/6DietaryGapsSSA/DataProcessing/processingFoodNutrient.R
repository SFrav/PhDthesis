library(dplyr)
files <- list.files(path = 'NutritientCompositionTables/NutrientContentRaw/', pattern = "\\.csv$")
#Need to rename the files slightly. There should be 8 files of each type
files <- files[(c(grep("macro", files), grep("amino", files), grep("vit", files), grep("mine", files)))]
#nutritionDF <- read.csv('NutritientCompositionTables/NutrientContentRaw/brothsmacro_090406.csv', skip = 2, stringsAsFactors = F)
#nutritionDF <- b[!is.na(as.numeric(nutritionDF$X) + 1),]
#files <- files[-grep("brothsmacro_090406.csv", files)]
macro <- NULL
amino <- NULL
vita <- NULL
miner <- NULL

for(i in 1:length(files)){
  j <- ifelse(length(grep("Macronutrient", read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]))[,2])) ==0, 2, grep("Macronutrient", read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]))[,2]))[1] 
  k <- ifelse(length(grep("amino", tolower(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]))[,2]))) ==0, 2, grep("amino", tolower(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]))[,2])))[1]
  l <- ifelse(length(grep("Vitamins", read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]))[,2])) ==0, 2, grep("Vitamins", read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]))[,2]))[1]
  m <- ifelse(length(grep("Minerals", read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]))[,2])) ==0, 2, grep("Minerals", read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]))[,2]))[1]
  if(colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]), skip = j))[2] == "Macronutrients"){
    r <- read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]), skip = j, stringsAsFactors = F)
    r <- r[!is.na(as.numeric(r$X) + 1),]
    r$code <- colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i])))[1]
    r$category <- colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i])))[2]
    colnames(r)[2] <- "foodItem"
    macro <- rbind(macro, r)
    
  }
  if(tolower(colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]), skip = k))[2]) == "amino.acids"){
    r <- read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]), skip = k, stringsAsFactors = F)
    r <- r[!is.na(as.numeric(r$X) + 1),]
    r$code <- colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i])))[1]
    r$category <- colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i])))[2]
    colnames(r)[2] <- "foodItem"
    amino <- rbind(amino, r)
  }
  if(colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]), skip = l))[2] == "Vitamins"){
    r <- read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]), skip = l, stringsAsFactors = F)
    r <- r[!is.na(as.numeric(r$X) + 1),]
    r$code <- colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i])))[1]
    r$category <- colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i])))[2]
    colnames(r)[2] <- "foodItem" 
    vita <- rbind(vita, r)
  }
  if(colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]), skip = m))[2] == "Minerals"){
    r <- read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i]), skip = m, stringsAsFactors = F)
    r <- r[!is.na(as.numeric(r$X) + 1),]
    r$code <- colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i])))[1]
    r$category <- colnames(read.csv(paste0('NutritientCompositionTables/NutrientContentRaw/', files[i])))[2]
    colnames(r)[2] <- "foodItem" 
    miner <- rbind(miner, r)
  }
  
}

amino$X <- as.numeric(amino$X)
vita$X <- as.numeric(vita$X)
miner$X <- as.numeric(miner$X)
nutritionDF <- left_join(macro, select(amino, -foodItem, -category, -code), by ="X")
nutritionDF <- left_join(nutritionDF, select(vita, -foodItem, -category, -code), by = "X")
nutritionDF <- left_join(nutritionDF, select(miner, -foodItem, -category, -code), by = "X")
#nutritionDF then forms the basis for the nutritionCropLivestock.csv file under the DataParams folder
#From here, nutritionDF is matched to crops produced and some categories are added from USDA ect.

rm(files, macro, amino, vita, miner, r, i, j, k, l, m)
