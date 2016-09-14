cellnames <- readRDS("data/cellnames.rds")
rgbhex <- readRDS("data/rgbhex.rds")
rgbclust <- readRDS("data/rgbclust.rds")
pca <- readRDS("data/pca.rds")
tfnames <- readRDS("data/tfnames.rds")
tfdat <- readRDS("data/tfdat.rds")

# Remove the text before the first underscore to simplify
tt <- sapply(tfnames, function(n){
    s <- strsplit(n, split = "_")[[1]]
    paste(s[2:length(s)], collapse = "_")
})

# Switch names and values
tfshort <- names(tt); names(tfshort) <- unname(tt)