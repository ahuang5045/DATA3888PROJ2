
shinyUI(
  fluidPage(
    titlePanel("Prediction of Acute Renal Failure by Gene Expression"),
    br(),
    br(),
    br(),
    fluidRow(
      column(4, textInput(inputId = "url",
                              label = "Place URL with csv file of gene expression measurements",
                              value = "https://raw.github.sydney.edu.au/ahua4943/Data3888DII/master/test_dat.csv?token=AAABCV2DSCSFMG5JQXT43US6W54RW"),
                    actionButton(inputId = "button", label = "Fetch data and predict"),
                    textOutput(outputId = "dummy")
                    ),
      column(8, DT::dataTableOutput(outputId = "predictions"))),
    
    br(),
    hr(),
    br(),
    
    fluidRow(
      column(3, textInput(inputId = "genedis",
                          label = "Number of genes to display",
                          value = "25"),
             actionButton(inputId = "button2", label = "Render plot")),
      column(9, plotOutput(outputId = "parameter_plot"))
    ),
    

    br(),
    hr(),
    br(),
    
    sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Lambda", sliderInput("lambda", 
                                       label = "Lambda value:",
                                       min = 0, max = 0.3, value = reg$lambda.1se),
                 actionButton(inputId = "button3", label = "Update Lambda"))
      ),
      br(),
      textOutput(outputId = "demographic3")),
      mainPanel(br(),
                textOutput(outputId = "demographic0"),
                textOutput(outputId = "demographic1"),
                textOutput(outputId = "demographic2"),
                DT::dataTableOutput(outputId = "cv_table"),
                )
    ),
    br(),
    plotOutput(outputId = "cv_plot"),
  
    br(),
    hr(),
    br(),

    fluidRow(
      column(3,
           selectInput("select", label = "Select gene to plot", 
                       choices = para[1], selected = 1)),
      column(9, plotOutput(outputId = "gene_specific"))
             ),
    
  
    #verbatimTextOutput(outputId = "res")
  )
)
            



  
  
            #dataTableOutput(outputId = "summary_tab"),
            
            #plotlyOutput(outputId = "wind_plot", height = "400px"),
            #
            


#fluidpage: for aesthetics (grid to place things)
#creates a list "Input"
#plotOutput takes output of server.R (which has created a list "Output")! 
