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

# Do Cell types
cn <- cellnames
ss <- strsplit(cn, "-")
ctypes <- sapply(1:length(ss), function(i){
    if(i <= 419) ss[[i]][3]
    else if(i <= 445)ss[[i]][2]
    else if(i <= 574) ss[[i]][3]
    else if(i <= 1002) ss[[i]][2]
    else if(i <= 1169) ss[[i]][3]
    else if(i <= 1255) ss[[i]][2]
    else if(i <= 1675) ss[[i]][3]
    else ss[[i]][5]
})

# Updates from Jason
ctypes[ctypes == "UNK"] <- "GMP1low"
ctypes[ctypes == "MCP"] <- "pDC"

df <- cbind(cn,ctypes)
colnames(df) <- c("name", "type")

saveRDS(df, "data/celltypes.rds")
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

m <- readMat("dataProcess/mm9_corrMat_08-Jun-2016.mat")
corM <- m$corrMat
colnames(corM) <- unname(unlist(m$TFnames))
row.names(corM) <- unname(unlist(m$TFnames))
corM[upper.tri(corM, diag = TRUE)] <- NA
mlong <- reshape2::melt(corM, na.rm = TRUE)
rownames(mlong) <- NULL

#saveRDS(mlong, "data/humanTFcorr.rds")

colnames(mlong) <- c("TF1", "TF2", "Pearson")
saveRDS(mlong, file = "mouse_cisBP_correlationMatrix.rds")  


# Process correlation lists
corvals <- seq(0.15, 1.00, 0.01)
listTFs <- lapply(corvals, function(cor){
    print(cor)
    TFnames <- tfshort[order(varTF, decreasing=TRUE)]
    hTFc <-  mlong
    i <- 1
    TFgroups <- list()
    while(length(TFnames) != 0){
        # Name
        tfcur <- TFnames[[1]]
        s <- strsplit(tfcur, split = "_")[[1]]
        tfshortname <- paste(s[2:length(s)], collapse = "_")
        #Find matches
        boo <- (hTFc$Var1 == tfcur | hTFc$Var2 == tfcur) & hTFc$value >= cor
        hits <- hTFc[boo, ]
        tfhits <- unique(c(as.character(hits$Var1), as.character(hits$Var2), tfcur))
        #Update lists
        TFnames <- TFnames[!(TFnames %in% tfhits)]
        sing <- list(tfhits)
        nn <- c(names(TFgroups), paste0("Group ", as.character(i), " ~ ", tfshortname))
        TFgroups[i] <- sing
        names(TFgroups) <- nn
        hTFc <- hTFc[hTFc$Var1 %in% TFnames & hTFc$Var2 %in% TFnames,]
        i <- i + 1
    }
    TFgroups
})
names(listTFs) <- paste0("g", as.character(corvals))
saveRDS(listTFs, "data/listTFs.rds")

