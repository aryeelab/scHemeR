source("extRa/libraries.R")
source("extRa/datImport.R")

shinyServer(function(input, output, session) {
    
    rv <- reactiveValues(
        minColVal = 0,
        maxColVal = 10,
        small2Sig = 2,
        big2Sig = 8, 
        valVec = c(0,10), 
        varValue = 1,
        tfname = "ENSG00000171223_LINE487_JUNB_D_N3",
        corrVal = "0.2",
        tsg =  "Group 1 ~ LINE487_JUNB_D_N3",
        tsgp = NULL,
        tcfg = NULL
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
    
    output$tfpossiblegroups <- renderUI({
        ggroup <- paste0("g", rv$corrVal)
        selectInput("tfspecifiedgroup", "Specify Grouping", selectize = TRUE,
            selected = NULL, choices = list("Groups" = names(listTFs[[ggroup]])))
    })
    
    output$groupTFopts <- renderUI({
        ggroup <- paste0("g", rv$corrVal)
        choicesTFingroup <- listTFs[[ggroup]][[rv$tsg]]
        selectInput("TFchosenFromGroup", "Select TF from Group", selectize = TRUE,
            selected = NULL, choices = list("Members" = unname(tt[choicesTFingroup])))
    })

    observe({
        if(input$colorVisPoints != "Cell" & input$colorVisPoints != "Cluster"){
            rv$valVec <- tfdat[,rv$tfname] 
            rv$minColVal <- round(min(rv$valVec), 1)
            rv$maxColVal <- round(max(rv$valVec), 1)
            rv$small2Sig <- round(mean(rv$valVec) - 2*sd(rv$valVec), 1)
            rv$big2Sig <- round(mean(rv$valVec) + 2*sd(rv$valVec), 1)
        }
    })
    
    observe({ rv$tfname <- input$tfname })
    observe({ 
        rv$corrVal <- as.character(input$groupCor)
        rv$tsgp <- NULL
        rv$tcfg <- NULL
        rv$tsg <- "Group 1 ~ LINE487_JUNB_D_N3"
        rv$tfname  <- "ENSG00000171223_LINE487_JUNB_D_N3"
        
    })
    observe({ rv$tsg <- input$tfspecifiedgroup })
    observe({ rv$tfname <- tfshort[input$TFchosenFromGroup] }) 
    
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
    
    output$tfvarianceval <- renderText({
        if(!(input$colorVisPoints == "Cell" | input$colorVisPoints == "Cluster")){
            rv$varValue <- varTF[rv$tfname]
        }
        paste0("Variance: ", as.character(round(rv$varValue,2)))
    })
    
    output$TFplot <- renderPlot({
        if(input$colorVisPoints == "Cell" | input$colorVisPoints == "Cluster"){
            return(NULL)
        } else {
           x <- pwms[[rv$tfname]]
           m <- round(-0.258 * (0.0310078- exp(x)), 4)
           seqLogo(m)
        }
    })

    
#    output$brush <- renderPrint({
#        d <- event_data("plotly_selected")
#        if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
#    })


})
