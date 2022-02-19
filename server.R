library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(RcmdrMisc)
library(lmtest)
library(shinyjs)
library(shinyalert)
source("functions.R")

shinyServer(function(input, output){
    # Plot formulation of the selected distribution
    output$Formulation <- renderImage({
        return(list(
            src = paste0("src/Formulations/",input$SI_DT,".png"),
            contentType = "image/png",
            alt = paste0("PDF of ",input$SI_DT," distribution")
        ))
    },deleteFile=FALSE)

    # Dynamical parameter input
    output$dynamic_parameters <- renderUI({
        switch(input$SI_DT,
            "Normal" = fluidRow(
                column(3,"μ =",style="text-align:right"),
                column(3,textInput('Normal_miu',label = NULL)),
                column(3,"σ =",style="text-align:right"),
                column(3,textInput('Normal_sigma',label = NULL,placeholder="σ should > 0"))
            ),
            "Poisson" = fluidRow(
                column(3,"λ =",style="text-align:right",offset=3),
                column(3,textInput('Poisson_lambda',label = NULL,placeholder="λ should > 0"))
            ),
        )
    })

    # Plot PDF
    observeEvent(input$BT_Plot, {
        output$PO_PDF <- renderPlot({
            if(input$SI_DT=="Normal"){
            }
            if(input$SI_DT=="Poisson"){
                x = DisPoisson(as.integer(input$Poisson_lambda))
            }
            
            ggplot() + geom_point(aes(x=x[[1]],y=x[[2]]),color="black")
        })
    })

})