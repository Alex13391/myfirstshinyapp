library(shinydashboard)
library(shiny)
library(ggplot2)
library(DT)
library(shinyBS)
library(ISLR)
library(MASS)
library(plotly)
T1=p("The aim of this app is to provide a friendly introduction to those who may be interested
     in joining the data science career.",hr(),"Throughout this page you will find the most common
     and widely used Machine Learning techniques such as the use of linear models to predict
     the behaviour of economic growth, as well as using Naive Bayes algorithm to classify 
     different kind of irises depending on measurements taken around the eye.")
T2=p("This app's template is also free to use and avaiable on", a("Github",href="https://github.com/"),
     p("so feel free to create your own branch and make any suggestions. They would be very welcomed."))
# Define UI for application that draws a histogram
ui <- dashboardPage(
  

  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName = "tabin",icon=icon("dashboard")),
      menuItem("Plotting Techniques",tabName = "tabpl",icon=icon("algolia")),
      menuItem("Machine Learning",tabName="tabml",icon=icon("cog"))
          )
  ),
  dashboardBody(
    dashboardthemes::shinyDashboardThemes(
      theme = "grey_light"
    ),
    tabItems(
      tabItem(tabName="tabin",
              fluidRow(
                column(6,h1("My Shiny App"),
                       h3("Welcome to an introduction to Data Science"),
                       h6("This app was developed using",a("Shiny by Rstudio", href = "http://www.rstudio.com/
      products/shiny/shiny-user-showcase/"))),
                column(6,tags$img(height = 100, 
                                  width = 100, 
                                  src = "bigorb.png"))
              ),
              hr(),
              fluidRow(
                column(6,T1),
                column(6,T2)
              )
      ),
      tabItem(tabName="tabpl",
                column(5,wellPanel(
                       selectInput(inputId="sel1",
                              label = "Choose a distribution",
                              choices=c("Normal","Uniform","Cauchy","Chi Squared 2 Degrees","Poisson 2","Beta 2 1","Exponential 2"),
                              multiple = FALSE ),
                       sliderInput(inputId="slid1",
                              label = "Choose how many observations",
                              value=25, min=1, max=100),
                       actionButton(inputId="go",
                                       label="Update distribution"),
                                       hr(),
                                    radioButtons("plot_type", "Plot type",
                                                 c("base", "ggplot2")),
                                    hr(),
                                    checkboxGroupInput("plot_density", "Plot density",
                                                c("Density", "Mean"),selected = "Density")
                )
              
              ),
              column(7, plotOutput("hist"),
                     verbatimTextOutput("stats"))
              
      ),
      tabItem(tabName="tabml",
              navbarPage(id="tabset",strong("Choose a technique type"),
                          tabPanel(tabName="unsupervised","Unsupervised",
                                   tabsetPanel(
                                     id = 'dataset',
                                     tabPanel("diamonds",
                                              column(9,DT::dataTableOutput("mytable1")),
                                              column(3,conditionalPanel(
                                                'input.dataset === "diamonds"',
                                                checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                                                                   names(diamonds), selected = names(diamonds))))),
                                     tabPanel("mtcars", 
                                              column(9,DT::dataTableOutput("mytable2")),
                                              column(3,conditionalPanel(
                                                'input.dataset === "mtcars"',
                                                checkboxGroupInput("show_vars2", "Columns in mtcars to show:",
                                                                   names(mtcars), selected = names(mtcars)[c(1,2,3,4)])
                                              ))),
                                     tabPanel("iris", DT::dataTableOutput("mytable3")),
                                     wellPanel(
                                       
                                       actionButton("show","Show Clustering"),
                                       bsModal("modal", "Clustering in a popup window", "show", size = "large",
                                               sidebarPanel(
                                                 conditionalPanel(
                                                   'input.dataset === "iris"',
                                                   selectInput('xcoliris', 'X Variable', names(iris)),
                                                   selectInput('ycoliris', 'Y Variable', names(iris),
                                                               selected = names(iris)[[2]]),
                                                   numericInput('clustersiris', 'Cluster count', 3,
                                                                min = 1, max = 9)
                                                 ),
                                                 conditionalPanel(
                                                   'input.dataset === "mtcars"',
                                                   selectInput('xcolcars', 'X Variable', names(mtcars)),
                                                   selectInput('ycolcars', 'Y Variable', names(mtcars),
                                                               selected = names(mtcars)[[2]]),
                                                   numericInput('clusterscars', 'Cluster count', 3,
                                                                min = 1, max = 9)
                                                 ),
                                                 conditionalPanel(
                                                   'input.dataset === "diamonds"',
                                                   selectInput('xcoldiamond', 'X Variable', names(diamonds)[-c(2,3,4)],
                                                               selected = names(diamonds)[[1]]),
                                                   selectInput('ycoldiamond', 'Y Variable', names(diamonds)[-c(2,3,4)],
                                                               selected = names(diamonds)[[5]]),
                                                   numericInput('clustersdiamond', 'Cluster count', 3,
                                                                min = 1, max = 9)
                                                 )
                                               ),mainPanel(
                                                 wellPanel(
                                                   conditionalPanel(
                                                     'input.dataset==="diamonds"',
                                                     plotOutput("cdiamonds")
                                                   ),
                                                   conditionalPanel(
                                                     'input.dataset==="mtcars"',
                                                     plotOutput("ccars")
                                                   ),
                                                   conditionalPanel(
                                                     'input.dataset==="iris"',
                                                     plotOutput("ciris")
                                                   )
                                                 )
                                               ))
                                     )
                                   )),
                          tabPanel(tabName="supervised","Supervised",
                                   tabsetPanel(id= "supml",
                                     tabPanel("Regression",
                                              fluidRow(
                                                column(4,
                                                       selectInput(inputId = "regdataset",
                                                                   label = "Dataset to be reviewed",
                                                                   choices=list("Swiss"="swiss",
                                                                                "Boston"="Boston",
                                                                                "Credit"="Credit")
                                                                   )
                                                ),
                                                column(4,
                                                       uiOutput("uiout")
                                                       #selectInput(inputId = "regout",
                                                       #            label = "Outcome variable",
                                                       #            choices=names(data.frame(Credit[1,],swiss[1,],Boston[1,])))
                                                ),
                                                column(4,
                                                       uiOutput("uiout2")
                                                )
                                              ),
                                              fluidRow(column(9,
                                                wellPanel(
                                                  conditionalPanel(
                                                  'input.regdataset === "swiss"',
                                                  plotlyOutput("regswiss")),
                                                  conditionalPanel(
                                                  'input.regdataset === "Boston"',
                                                  plotOutput("regBoston")),
                                                  conditionalPanel(
                                                    'input.regdataset ==="Credit"',
                                                    plotOutput("regCredit"))))
                                                  ,
                                                  column(3,actionButton("show2","Show model"))
                                                
                                              ),bsModal("modal2", "Summary from selected model", "show2",
                                                        conditionalPanel(
                                                          'input.regdataset === "swiss"',
                                                          verbatimTextOutput("summarys")),
                                                        conditionalPanel(
                                                          'input.regdataset === "Boston"',
                                                          verbatimTextOutput("summaryB")),
                                                        conditionalPanel(
                                                          'input.regdataset ==="Credit"',
                                                          verbatimTextOutput("summaryC"))
                                                        )
                                              
                                     ),
                                     tabPanel("Classification")
                                       
                                     
                                   )
                                   )
                         )
              )
  
    )
  )
)


server <- function(input, output) {
  
  output$uiout <- renderUI({
    if (is.null(input$regdataset))
      return()
    
    switch(input$regdataset,
           "swiss" = selectInput("dynamic",
                                 label= "Select outcome",
                                 choices=names(swiss)),
           "Boston" = selectInput("dynamic",
                                  label= "Select outcome",
                                  choices=names(Boston)),
           "Credit" = selectInput("dynamic",
                                  label= "Select outcome",
                                  choices=names(Credit))
           )
  })

  output$uiout2 <- renderUI({
    if (is.null(input$regdataset))
      return()
    
    switch(input$regdataset,
           "swiss" = selectInput("dynamic2",
                                 label= "Select explanatory variable",
                                 choices=names(swiss)),
           "Boston" = selectInput("dynamic2",
                                  label= "Select explanatory variable",
                                  choices=names(Boston)),
           "Credit" = selectInput("dynamic2",
                                  label= "Select explanatory variable",
                                  choices=names(Credit))
    )
  })

  #Setting up the reactive plots for the regression tab
  # swiss dataset
  selectedDataregS <- reactive({
    swiss[, c(input$dynamic, input$dynamic2)]
    })
  
  output$regswiss <- renderPlotly({
    plot_ly(
      selectedDataregS(),x=~selectedDataregS()[,1], y=~selectedDataregS()[,2],
      color = ~selectedDataregS()[,1], size = ~selectedDataregS()[,1]) %>%
      layout(title = paste(input$dynamic, "vs ", input$dynamic2),
             xaxis = list(title = input$dynamic),
             yaxis = list(title = input$dynamic2)
             )
   
  })
  
  # boston dataset
  selectedDataregB <- reactive({
    Boston[, c(input$dynamic, input$dynamic2)]
  })
  
  output$regBoston <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedDataregB(),
         pch = 20, cex = 3)
    
  })
  
  # Credit dataset
  selectedDataregC <- reactive({
    Credit[, c(input$dynamic, input$dynamic2)]
  })
  
  output$regCredit <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedDataregC(),
         pch = 20, cex = 3)
    
  })
  
  #Setting up the summary displayed in the modal panel NEEDED 3 ONE FOR EACH
  # DATASET DUE TO THE NATURE OF THE MODAL IMPLYING CONDITIONAL PANELS
  output$summarys <-renderPrint({
        fit <- lm(swiss[,input$dynamic] ~ swiss[,input$dynamic2])
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
  })
  
  
  # A different modal button to display clustering options, although is limited
  # by it's own nature of being on the server logic instead of the UI.
#  observeEvent(input$show, {
#    showModal(modalDialog(
#      title = "Important message",
#      "This is an important message!"
#    ))
#  })

  # Preparing the displayed data and plot ouputs on the clustering plots for each dataset:
  # Iris dataset
  selectedDatai <- reactive({
    iris[, c(input$xcoliris, input$ycoliris)]
  })
  
  clustersi <- reactive({
    kmeans(selectedDatai(), input$clustersiris)
  })
  
  output$ciris <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedDatai(),
         col = clustersi()$cluster,
         pch = 20, cex = 3)
    points(clustersi()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  # Diamonds dataset
  selectedDatad <- reactive({
    diamonds[sample(nrow(diamonds), 500), c(input$xcoldiamond, input$ycoldiamond)]
  })
  
  clustersd <- reactive({
    kmeans(selectedDatad(), input$clustersdiamond)
  })
  
  output$cdiamonds <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedDatad(),
         col = clustersd()$cluster,
         pch = 20, cex = 3)
    points(clustersd()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  # mtcars Dataset
  selectedDatac <- reactive({
    mtcars[, c(input$xcolcars, input$ycolcars)]
  })
  
  clustersc <- reactive({
    kmeans(selectedDatac(), input$clusterscars)
  })
  
  output$ccars <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedDatac(),
         col = clustersc()$cluster,
         pch = 20, cex = 3)
    points(clustersc()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
  # choose columns to display
  diamonds2 = diamonds[sample(nrow(diamonds), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(diamonds2[, input$show_vars, drop = FALSE],options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(mtcars[,input$show_vars2, drop = FALSE], options = list(lengthMenu = c(5, 30, 50), pageLength = 5,orderClasses = TRUE))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  # We use eventReactive here depending on the update button, so the
  # output is updated only when the button is triggered
  datasetInput <- eventReactive(input$go, {
    switch(input$sel1,
           "Normal" = rnorm(input$slid1),
           "Uniform" = runif(input$slid1),
           "Cauchy" = rcauchy(input$slid1),
           "Chi Squared 2 Degrees" = rchisq(input$slid1,2),
           "Poisson 2" = rpois(input$slid1,2),
           "Beta 2 1" = rbeta(input$slid1,2,1),
           "Exponential 2" = rexp(input$slid1,2)
           )
  }, ignoreNULL = FALSE)

  # We fit a conditional if inside the renderPlot to showcase both
  # different types of plot functions avaiables in R
  # Using additional plots like ggplotly or gganimate are left as 
  # proposed course work.
  output$hist <- renderPlot({ 
    if (input$plot_type == "base") {
      hist(datasetInput())
    } else if (input$plot_type == "ggplot2") {
      if (all(c("Density","Mean") %in% input$plot_density)){
        ggplot(as.data.frame(datasetInput()),aes(x=datasetInput()))+
        geom_histogram(color="darkblue",
                       fill="lightblue",
                       bins=nclass.FD(datasetInput()))+
        stat_density()+
        geom_vline(aes(xintercept=mean(datasetInput())),
                     color="blue", linetype="dashed", size=1)+
        theme_bw()
      } else {
        if (input$plot_density == "Mean"){
        ggplot(as.data.frame(datasetInput()),aes(x=datasetInput()))+
          geom_histogram(color="darkblue",
                         fill="lightblue",
                         bins=nclass.FD(datasetInput()))+
          geom_vline(aes(xintercept=mean(datasetInput())),
                     color="blue", linetype="dashed", size=1)+
          theme_bw()
      } else if (input$plot_density == "Density"){
        ggplot(as.data.frame(datasetInput()),aes(x=datasetInput()))+
          geom_histogram(color="darkblue",
                         fill="lightblue",
                         bins=nclass.FD(datasetInput()))+
          stat_density()+
          theme_bw()
      } else if (is_false(any(c("Density","Mean"))) %in% input$plot_density){
        ggplot(as.data.frame(datasetInput()),aes(x=datasetInput()))+
          geom_histogram(color="darkblue",
                         fill="lightblue",
                         bins=nclass.FD(datasetInput()))+
          theme_bw()
      }
      }
      
    } 
  })
  output$stats <- renderPrint({
    summary(datasetInput())
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

