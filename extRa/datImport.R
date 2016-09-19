cellnames <- readRDS("data/cellnames.rds")
rgbhex <- readRDS("data/rgbhex.rds")
rgbclust <- readRDS("data/rgbclust.rds")
pca <- readRDS("data/pca.rds")
tfnames <- readRDS("data/tfnames.rds")
tfdat <- readRDS("data/tfdat.rds")
pwms <- readRDS("data/pwms.rds")
humanTFcorr <- readRDS("data/humanTFcorr.rds")

varTF <- apply(tfdat, 2, var)

# Remove the text before the first underscore to simplify
tt <- sapply(tfnames, function(n){
    s <- strsplit(n, split = "_")[[1]]
    paste(s[2:length(s)], collapse = "_")
})

# Switch names and values
tfshort <- names(tt); names(tfshort) <- unname(tt)
# 
# start <- Sys.time()
# # Do this grouping stuff
# TFnames <- tfshort[order(varTF, decreasing=TRUE)]
# cor <- 0.7
# hTFc <- humanTFcorr
# i <- 1
# TFgroups <- list()
# while(length(TFnames) != 0){
#     print(length(TFnames))
#     # Name
#     tfcur <- TFnames[[1]]
#     print(tfcur)
#     s <- strsplit(tfcur, split = "_")[[1]]
#     tfshortname <- paste(s[2:length(s)], collapse = "_")
#     
#     #Find matches
#     boo <- (hTFc$Var1 == tfcur | hTFc$Var2 == tfcur) & hTFc$value >= cor
#     hits <- hTFc[boo, ]
#     hTFc <- hTFc[!boo,]
#     tfhits <- unique(c(as.character(hits$Var1), as.character(hits$Var2), tfcur))
#     
#     #Update lists 
#     TFnames <- TFnames[!(TFnames %in% tfhits)]
#     sing <- list(tfhits)
#     nn <- c(names(TFgroups), paste0("Group ", as.character(i), " ~ ", tfshortname))
#     TFgroups[i] <- sing
#     names(TFgroups) <- nn
#     i <- i + 1
# }
# 
# Sys.time () - start