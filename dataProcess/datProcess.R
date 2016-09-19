# Process the scHemeData_v2 file; exported from Excel as a .txt file

if (basename(getwd()) != "scHemer") setwd("..")

# Process text file
dat <- read.table("dataProcess/scHemeData_v2.txt", header = TRUE, row.names = NULL, stringsAsFactors = FALSE)
cellnames <- dat[,1]

rgb <- data.matrix(dat[,c(2,3,4)])
rgbhex <-apply(rgb, 1, function(x) rgb(x[1], x[2], x[3]))

crgb <- data.matrix(dat[,c(5,6,7)])
rgbclust <-apply(crgb, 1, function(x) rgb(x[1], x[2], x[3]))

pca <- data.matrix(dat[,c(8,9,10)])

# Scale range to [-1, 1]
range02 <- function(x, newMax = 1, newMin = -1){ (x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin }
pca <- apply(pca, 2, range02)

tfnames <- names(dat[,c(13:dim(dat)[2])])
tfdat <- data.matrix(dat[,c(13:dim(dat)[2])])

saveRDS(cellnames, "data/cellnames.rds")
saveRDS(rgbhex, "data/rgbhex.rds")
saveRDS(rgbclust, "data/rgbclust.rds")
saveRDS(pca, "data/pca.rds")
saveRDS(tfnames, "data/tfnames.rds")
saveRDS(tfdat, "data/tfdat.rds")

# Read .RData file and send to a binary format
load("dataProcess/cisbp_hg19_unique.08_Jun_2016.RData")
nn <- names(pwms@listData)
pwms <- lapply(nn, function(n){
    pwms@listData[[n]]@profileMatrix
})
names(pwms) <- nn
saveRDS(pwms, "data/pwms.rds")

### Sample Plot
n <- "ENSG00000113658_LINE19373_SMAD5_I_N1"
m <- t(t(2^pwms[[n]])/colSums(2^pwms[[n]]))
seqLogo(m)


### Deal with correlation matrices
library(R.matlab)
library(reshape2)

m <- readMat("dataProcess/hg19_corrMat_08-Jun-2016.mat")
corM <- m$corrMat
colnames(corM) <- unname(unlist(m$TFnames))
row.names(corM) <- unname(unlist(m$TFnames))
corM[upper.tri(corM, diag = TRUE)] <- NA
mlong <- reshape2::melt(corM, na.rm = TRUE)
rownames(mlong) <- NULL

saveRDS(mlong, "data/humanTFcorr.rds")
