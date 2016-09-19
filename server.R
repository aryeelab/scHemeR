source("extRa/libraries.R")
source("extRa/datImport.R")

shinyServer(function(input, output, session) {
    
    rv <- reactiveValues(
        minColVal = 0,
        maxColVal = 10,
        small2Sig = 2,
        big2Sig = 8, 
        valVec = c(0,10)
        )
    
    output$minMaxColor <- renderUI({
        sliderInput("minMaxColor", "Set Dynamic Color Range", round = -1,
                    min = rv$minColVal, max = rv$maxColVal, step = 0.1,
                    value=c(round(rv$small2Sig,1),round(rv$big2Sig,1)))
    })
    
    output$tfname <- renderUI({
        if(input$sortVar){
            tts <- tfshort[order(varTF, decreasing=TRUE)]
        } else {
            tts <- tfshort
        }
        selectInput("tfname", "Select Transcription Factor", selectize = TRUE, 
            selected = "LINE487_JUNB_D_N3", choices = list("TF Annotation" = tts))
    })

    observe({
        if(input$colorVisPoints != "Cell" & input$colorVisPoints != "Cluster"){
            rv$valVec <- tfdat[,input$tfname] 
            rv$minColVal <- round(min(rv$valVec), 1)
            rv$maxColVal <- round(max(rv$valVec), 1)
            rv$small2Sig <- round(mean(rv$valVec) - 2*sd(rv$valVec), 1)
            rv$big2Sig <- round(mean(rv$valVec) + 2*sd(rv$valVec), 1)
        }
    })
    

    output$plotgraph1 = renderPlotly({

        if(input$colorVisPoints == "Cell" | input$colorVisPoints == "Cluster"){
            if(input$colorVisPoints == "Cell"){
                cf <- factor(rgbhex, levels = as.character(unique(rgbhex)), ordered = TRUE)
            } else {
                cf <- factor(rgbclust, levels = as.character(unique(rgbclust)), ordered = TRUE)
            }
            d <- data.frame(cellnames, pca, colidx = as.integer(cf))
            plot_ly(d, x = PC1, y = PC2, z = PC3, text = paste0("Cell:", cellnames),
                    type="scatter3d", mode="markers", marker = list(size = 3),
                    color = as.ordered(colidx), colors = rev(as.character(unique(cf)))) %>%
                layout(showlegend = FALSE)
            
        } else { # TF score
            col <- as.numeric(rv$valVec)
            col[col < input$minMaxColor[1]] <- input$minMaxColor[1]
            col[col > input$minMaxColor[2]] <- input$minMaxColor[2]
            d <- data.frame(cellnames, pca, Score = col, stringsAsFactors = FALSE)
            
            #Adjust for spectral
            if(input$contColorTheme == "Spectral"){
                cols <- rev(RColorBrewer::brewer.pal(11, "Spectral")) 
                } else { cols <- input$contColorTheme
            }
            plot_ly(d, x = PC1, y = PC2, z = PC3, text = c(paste0("Cell: ", cellnames, "<br>", "TF Score: ", Score)),
                        type="scatter3d", mode="markers", marker = list(size = 3),
                        color = Score, colors = cols)
        }
        
    })
    
    output$TFplot <- renderPlot({
        if(input$colorVisPoints == "Cell" | input$colorVisPoints == "Cluster"){
            return(NULL)
        } else {
           x <- pwms[[input$tfname]]
           m <- round(-0.258 * (0.0310078- exp(x)), 4)
           seqLogo(m)
        }
    })

    
#    output$brush <- renderPrint({
#        d <- event_data("plotly_selected")
#        if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
#    })


})
