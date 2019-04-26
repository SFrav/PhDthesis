#PPI score
library(gsubfn)

#data
rawPPI <- read.csv('6_Sortie de pauvrete.csv', stringsAsFactors = F)
rawPPI$Mem14rep <- as.numeric(gsubfn(".", list("A" = 0, "B" = 5, "C" = 6, "D" =10, "E" = 13, "F" = 19, "G" = 29), rawPPI$Mem14rep))
rawPPI$Lanecparep <- as.numeric(gsubfn(".", list("A" = 0, "B" = 4, "C" = 5), rawPPI$Lanecparep))
rawPPI$Teprimarep <- as.numeric(gsubfn(".", list("A" = 0, "B" = 0, "C" = 9), rawPPI$Teprimarep))
rawPPI$Souenegrep <- as.numeric(gsubfn(".", list("A" = 0, "B" = 4, "C" = 5, "D" = 8), rawPPI$Souenegrep))
rawPPI$Toilposrep <- as.numeric(gsubfn(".", list("A" = 0, "B" = 4, "C" = 15), rawPPI$Toilposrep))
rawPPI$Metelerep <- as.numeric(gsubfn(".", list("A" = 0, "B" = 10), rawPPI$Metelerep))
rawPPI$Mematerep <- as.numeric(gsubfn(".", list("A" = 0, "B" = 3), rawPPI$Mematerep))
rawPPI$Memotorep <- as.numeric(gsubfn(".", list("A" = 0, "B" = 6), rawPPI$Memotorep))
rawPPI$Meagrirep <- as.numeric(gsubfn(".", list("A" = 0, "B" = 8), rawPPI$Meagrirep))
rawPPI$Bovposrep <- as.numeric(gsubfn(".", list("A" = 0, "B" = 2, "C" = 3, "D" =7), rawPPI$Bovposrep))
rawPPI$PPI_score <- rowSums(rawPPI[, 7:16], na.rm=T)
datPPI <- rawPPI[, c("Household.ID", "PPI_score")]
        