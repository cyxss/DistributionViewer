library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(RcmdrMisc)
library(lmtest)
library(shinyjs)
library(shinyalert)
source("Distribution.r")

shinyServer(function(input, output){
    output$Main <- renderImage({
        return(
            list(
                src = paste0("src/MainPage.jpg"),
                contentType = "image/jpg",
                width = 600,
                height = 800,
                alt = "The Relationship of distributions"
        ))
    })

    # Plot formulation of the selected distribution
    output$Formulation <- renderImage({
        if(input$SI_DT == "Gamma"){
            if(input$Gamma_para == "θ = "){
                return(
                    list(
                        src = paste0("src/Formulations/",input$SI_DT,"_scale.svg"),
                        contentType = "image/svg+xml",
                        width = 400,
                        height = 300,
                        alt = paste0("Pdf of ",input$SI_DT," distribution")
                ))
                }else{
                    return(
                        list(
                            src = paste0("src/Formulations/",input$SI_DT,"_rate.svg"),
                            contentType = "image/svg+xml",
                            width = 400,
                            height = 300,
                            alt = paste0("Pdf of ",input$SI_DT," distribution")
                    ))
                }
        }else{
            return(list(
            src = paste0("src/Formulations/",input$SI_DT,".svg"),
            contentType = "image/svg+xml",
            width = 400,
            height = 300,
            alt = paste0("Pdf of ",input$SI_DT," distribution")
        ))
        }
        
    },deleteFile=FALSE)

    # Dynamical parameter input
    output$dynamic_parameters <- renderUI({
        switch(input$SI_DT,
            "Gamma" = fluidRow(
                column(3,"k =",style="text-align:right"),
                column(3,numericInput('Gamma_k',label = NULL,value = 1)),
                column(3,selectInput("Gamma_para", NULL, 
                    choices = c("θ = ", "β = "), 
                    selected = "θ = ")),
                column(3,numericInput('Gamma_value',label = NULL,value = 0.5))
            ),
            "Beta" = fluidRow(
                column(3,"α =",style="text-align:right"),
                column(3,numericInput('Beta_alpha',label = NULL,value = 2)),
                column(3,"β =",style="text-align:right"),
                column(3,numericInput('Beta_beta',label = NULL,value = 3))
            ),
            "Cauchy" = fluidRow(
                column(3,"x0 =",style="text-align:right"),
                column(3,numericInput('Cauchy_x0',label = NULL,value = 5)),
                column(3,"γ =",style="text-align:right"),
                column(3,numericInput('Cauchy_gamma',label = NULL,value = 1))
            ),
            "Chisquare" = fluidRow(
                column(3,"k =",style="text-align:right",offset=3),
                column(3,numericInput('Chisquare_k',label = NULL,value = 3)),
            ),
            "Exponential" = fluidRow(
                column(3,"λ =",style="text-align:right",offset=3),
                column(3,numericInput('Exponential_lambda',label = NULL,value = 3)),
            ),
            "Normal" = fluidRow(
                column(3,"μ =",style="text-align:right"),
                column(3,numericInput('Normal_miu',label = NULL,value = 3)),
                column(3,"σ =",style="text-align:right"),
                column(3,numericInput('Normal_sigma',label = NULL,,value = 1))
            ),
            "StudentT" = fluidRow(
                column(3,"ν =",style="text-align:right",offset=3),
                column(3,numericInput('StudentT_niu',label = NULL,value = 3)),
            ),
            "Binomial" = fluidRow(
                column(3,"n =",style="text-align:right"),
                column(3,numericInput('Binomial_n',label = NULL,value = 100)),
                column(3,"k =",style="text-align:right"),
                column(3,numericInput('Binomial_k',label = NULL,value = 0.1))
            ),
            "Geometric" = fluidRow(
                column(3,"p =",style="text-align:right",offset=3),
                column(3,numericInput('Geometric_p',label = NULL,value = 0.5)),
            ),
            "NegBinomial" = fluidRow(
                column(3,"n =",style="text-align:right"),
                column(3,numericInput('NegBinomial_r',label = NULL,value = 10)),
                column(3,"k =",style="text-align:right"),
                column(3,numericInput('NegBinomial_p',label = NULL,value = 0.5))
            ),
            "Poisson" = fluidRow(
                column(3,"λ =",style="text-align:right",offset=3),
                column(3,numericInput('Poisson_lambda',label = NULL,value = 5))
            )
        )
    })

    # Plot PDF
    observeEvent(input$BT_Plot, {
        output$PO_PDF <- renderPlot({
            if(input$SI_DT=="Gamma"){
                if(input$Gamma_para=="θ = "){
                    isscale = TRUE
                }else{
                    isscale = FALSE
                }
                dis_para <- list(input$Gamma_k, input$Gamma_value, isscale)
            }
            if(input$SI_DT=="Beta"){
                dis_para <- list(input$Beta_alpha, input$Beta_beta)
            }
            if(input$SI_DT=="Cauchy"){
                dis_para <- list(input$Cauchy_x0, input$Cauchy_gamma)
            }
            if(input$SI_DT=="Chisquare"){
                dis_para <- list(input$Chisquare_k)
            }
            if(input$SI_DT=="Exponential"){
                dis_para <- list(input$Exponential_lambda)
            }
            if(input$SI_DT=="Normal"){
                dis_para <- list(input$Normal_miu, input$Normal_sigma)
            }
            if(input$SI_DT=="StudentT"){
                dis_para <- list(input$StudentT_niu)
            }
            if(input$SI_DT=="Binomial"){
                dis_para <- list(input$Binomial_n,input$Binomial_n)
            }
            if(input$SI_DT=="Geometric"){
                dis_para <- list(input$Geometric_p)
            }
            if(input$SI_DT=="NegBinomial"){
                dis_para <- list(input$NegBinomial_r,input$NegBinomial_p)
            }
            if(input$SI_DT=="Poisson"){
                dis_para <- list(input$Poisson_lambda)
            }
            PlotDistribution(disname = input$SI_DT, param = dis_para)
        })
    })

})