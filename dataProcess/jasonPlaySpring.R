install.packages("devtools")
install.packages("plotly")
devtools::install_github("caleblareau/scTools")

if (basename(getwd()) != "scHemer") setwd("..")

# Process text file
dat <- read.table("121208_pc.txt", header = TRUE, row.names = NULL, stringsAsFactors = FALSE)
cellnames <- dat[,1]

rgb <- data.matrix(dat[,c(2,3,4)])
rgbhex <-apply(rgb, 1, function(x) rgb(x[1], x[2], x[3]))

#crgb <- data.matrix(dat[,c(5,6,7)])
#rgbclust <-apply(crgb, 1, function(x) rgb(x[1], x[2], x[3]))

pca <- data.matrix(dat[,5:14])

cf <- factor(rgbhex, levels = as.character(unique(rgbhex)), ordered = TRUE)


# PC plot
#d <- data.frame(cellnames, pca, colidx = as.integer(cf))
#plotly::plot_ly(d, x = ~PC1, y = ~PC2, z = ~PC3, text = paste0("Cell:", cellnames),
#        type="scatter3d", mode="markers", marker = list(size = 3),
#        color = ~as.ordered(colidx), colors = as.character(unique(cf))) %>%
#    layout(showlegend = FALSE)

# 2D plot from Spring
Y <- scTools::spRing(pca, k = 7, method = "pearson")  #### CHANGE K HERE AS DESIRED ####
d$Y1 <- Y[,1]
d$Y2 <- Y[,2]
plotly::plot_ly(d, x = ~Y1, y = ~Y2, text = paste0("Cell:", cellnames),
        type="scatter", mode="markers",
        color = ~as.ordered(colidx), colors = as.character(unique(cf))) %>%
    layout(showlegend = FALSE)

# When you have a plot you like, spit it out
write.table(d, file = "toJason_newPCA.txt", quote = FALSE, row.names = FALSE, sep = "\t")