# Process the scHemeData_v2 file; exported from Excel as a .txt file

if (basename(getwd()) != "scHemer") setwd("..")

# Process text file; merge with other to keep order
dat <- read.table("dataProcess/121208_pc.txt", header = TRUE, row.names = NULL, stringsAsFactors = FALSE)

col <- read.table("dataProcess/motifs_data.col.xls", header = FALSE,row.names = NULL, stringsAsFactors = FALSE)[,1]
row <- as.character(read.table("dataProcess/motifs_data.row.xls", header = FALSE,row.names = NULL, stringsAsFactors = FALSE)[,1])
mdat <-  data.matrix(read.table("dataProcess/motifs_data.xls", header = FALSE,row.names = NULL, stringsAsFactors = FALSE))

#mdf <- data.frame(row, mdat)
#colnames(mdf) <- c("Name", col)

rgb <- data.matrix(dat[,c(2,3,4)])
rgbhex <-apply(rgb, 1, function(x) rgb(x[1], x[2], x[3]))

rgb <- data.matrix(read.table("dataProcess/cluster_info.txt"))
rgbclust <-apply(rgb, 1, function(x) rgb(x[1], x[2], x[3]))

pca <- data.matrix(dat[,5:14])

# cf <- factor(rgbhex, levels = as.character(unique(rgbhex)), ordered = TRUE)
# 
# d <- data.frame(cellnames, pca, colidx = as.integer(cf))
# plotly::plot_ly(d, x = ~PC1, y = ~PC2, z = ~PC3, text = paste0("Cell:", cellnames),
#         type="scatter3d", mode="markers", marker = list(size = 3),
#         color = ~as.ordered(colidx), colors = as.character(unique(cf))) %>%
#     layout(showlegend = FALSE)
# 
# Y <- scTools::spRing(pca, k = 7, method = "pearson")
# d$Y1 <- Y[,1]
# d$Y2 <- Y[,2]
# plotly::plot_ly(d, x = ~Y1, y = ~Y2, text = paste0("Cell:", cellnames),
#         type="scatter", mode="markers",
#         color = ~as.ordered(colidx), colors = as.character(unique(cf))) %>%
#     layout(showlegend = FALSE)

# Scale range to [-1, 1]
range02 <- function(x, newMax = 1, newMin = -1){ (x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin }
#pca <- apply(pca, 2, range02)

tfnames <- col
colnames(mdat) <- col
tfdat <- mdat

# Do Cell types
cellnames <- dat[,1]
cn <- cellnames
ss <- strsplit(cn, "-")
ctypes <- sapply(1:length(ss), function(i){
    if(i <= 359) ss[[i]][3]
    else if(i <= 386)ss[[i]][2]
    else if(i <= 503)ss[[i]][3]
    else if(i <= 875) ss[[i]][2]
    else if(i <= 1012) ss[[i]][3] 
    else if(i <= 1085) ss[[i]][2] 
    else if(i <= 1169) ss[[i]][3]
    else if(i <= 1255) ss[[i]][3]
    else if(i <= 1433) ss[[i]][3]
    else if(i <= 1675) ss[[i]][5]
    else if(i <= 1818) ss[[i]][5]
    else ss[[i]][4]
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
#load("dataProcess/cisbp_hg19_unique.08_Jun_2016.RData")
#nn <- names(pwms@listData)
#pwms <- lapply(nn, function(n){
#    pwms@listData[[n]]@profileMatrix
#})
#names(pwms) <- nn
#saveRDS(pwms, "data/pwms.rds")

### Sample Plot
#n <- "ENSG00000113658_LINE19373_SMAD5_I_N1"
#m <- t(t(2^pwms[[n]])/colSums(2^pwms[[n]]))
#seqLogo(m)


### Deal with correlation matrices
#library(R.matlab)
#library(reshape2)

#m <- readMat("dataProcess/hg19_corrMat_08-Jun-2016.mat")
#corM <- m$corrMat
#colnames(corM) <- unname(unlist(m$TFnames))
#row.names(corM) <- unname(unlist(m$TFnames))
#corM[upper.tri(corM, diag = TRUE)] <- NA
#mlong <- reshape2::melt(corM, na.rm = TRUE)
#rownames(mlong) <- NULL

#saveRDS(mlong, "data/humanTFcorr.rds")
mlong <- readRDS("data/humanTFcorr.rds")

# Process correlation lists
tfshort <- sapply(tfnames, function(t){
  paste(strsplit(t, split = "_")[[1]][-1], collapse="_")
})

varTF <- matrixStats::colVars(mdat)

corvals <- seq(0.15, 1.00, 0.01)
listTFs <- lapply(corvals, function(cor){
    print(cor)
    TFnames <- tfnames[order(varTF, decreasing=TRUE)]
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

