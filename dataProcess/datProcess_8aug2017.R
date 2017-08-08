# Process the scHemeData_v2 file; exported from Excel as a .txt file

if (basename(getwd()) != "scHemeR") setwd("..")

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

pca <- read.table("dataProcess/8aug2017_correctPCs.txt", header = FALSE, sep = ",")
colnames(pca) <- paste0("PC", as.character(1:10))

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
