library(shiny)
library(shinythemes)
library(ggplot2)
library(RcmdrMisc)
library(lmtest)
library(markdown)
library(shinyjs)
library(shinyalert)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
    navbarPage(p(icon("chart-line",lib = "font-awesome"),em("Distribution Viewer"),style="text-align:center"),
        tabPanel("Home",
            h2("It is a online tool to plot pdf of different distirbutions with adjustable parameters.",style="text-align:center"),
            fluidRow(
            column(imageOutput("Main"),style="text-align:center",width=8,offset=2)
            )
        ),
        tabPanel("Viewer",
        # Distribution Selection
        fluidRow(
            column(h3("Select the distribution type:",style="text-align:right"),width=5),
            column(selectInput("SI_DT", NULL, 
                    choices = c("Gamma", "Beta", "Cauchy", "Chisquare", "Exponential", "Normal", "StudentT", "Binomial", "Geometric", "NegBinomial", "Poisson"), 
                    selected = "Gamma"),width=7)
        ),
        hr(),
        # print the pdf formulation
        h3("PDF formulation",style="text-align:center"),
        fluidRow(
            column(imageOutput("Formulation"),style="text-align:center",width=6,offset=3)
        ),
        hr(),
        # generate the parameters inputs dynamically
        h3("Parameters",style="text-align:center"),
        fluidRow(uiOutput("dynamic_parameters")),
        fluidRow(actionButton("BT_Plot","Plot PDF",style="text-align:center",icon=icon("pencil",lib = "font-awesome"))
            ,style="text-align:right"),
        hr(),
        # Plot the pdf with selected parameters
        h3("PDF Plot",style="text-align:center"),
        plotOutput('PO_PDF',click= clickOpts(id = "PO_PDF_click")),
        hr(),
        # print statistics
        h3("Statistics",style="text-align:center"),
        verbatimTextOutput("TO_Stat")
    )),
    hr(),
    p(em("Developed by Yixin Chen, MinSheng Hao, Boyang Wang"),br("XGlab, Tsinghua University, Beijing, China"),style="text-align:center; font-family: times")
))