library(plotly)
library(shiny)
library(dplyr)

USPersonalExpenditure <- data.frame("Categorie" = rownames(USPersonalExpenditure), USPersonalExpenditure)
USPersonalExpenditure$Categorie=c("Comida y Tabaco","Industria del Ladrillo","Sanidad","Asistencia personal","Educación Privada")
data <- USPersonalExpenditure[, c('Categorie', 'X1960')]
data2 <- USPersonalExpenditure[, c('Categorie', 'X1955')]
data3 <- USPersonalExpenditure[, c('Categorie', 'X1950')]
data4 <- USPersonalExpenditure[, c('Categorie', 'X1945')]
data5 <- USPersonalExpenditure[, c('Categorie', 'X1940')]

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

p <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', X1960, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
    layout(title = 'Inversiones entre los años 1960 y 1965',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

q <- plot_ly(data2, labels = ~Categorie, values = ~X1955, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', X1955, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
    layout(title = 'Inversiones entre los años 1955 y 1960',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

r <- plot_ly(data3, labels = ~Categorie, values = ~X1950, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', X1950, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             
             showlegend = FALSE) %>%
    layout(title = 'Inversiones entre los años 1950 y 1955',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

s <- plot_ly(data4, labels = ~Categorie, values = ~X1945, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', X1945, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             
             showlegend = FALSE) %>%
    layout(title = 'Inversiones entre los años 1945 y 1950',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


t <- plot_ly(data5, labels = ~Categorie, values = ~X1940, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', X1940, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             
             showlegend = FALSE) %>%
    layout(title = 'Inversiones entre los años 1940 y 1945',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# Define UI for application that draws a histogram
ui <- fluidPage(

    
    titlePanel("Inversiones del gobierno de los Estados Unidos entre los años 1940 y 1960"),

     
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="sel1",
                        label = "Elija el comienzo de un lustro",
                        choices=c("1940","1945","1950","1955","1960"),
                        multiple = FALSE ),
            h5("Deslice el ratón sobre el gráfico para comprobar la cantidad neta en dólares."),
            hr(),
            h5("La unidad utilizada es el ",em("billion"),"americano, equivalente a mil millones en castellano.")
        ),

        
        mainPanel(
            column(10,
            conditionalPanel('input.sel1==="1960"',plotlyOutput("distPlot")),
            conditionalPanel('input.sel1==="1955"',plotlyOutput("distPlot2")),
            conditionalPanel('input.sel1==="1950"',plotlyOutput("distPlot3")),
            conditionalPanel('input.sel1==="1945"',plotlyOutput("distPlot4")),
            conditionalPanel('input.sel1==="1940"',plotlyOutput("distPlot5")),
            h6("Alejandro Ballesteros Hernández"))
           
        )
    )
)


server <- function(input, output) {

    output$distPlot <- renderPlotly({p})
    output$distPlot2 <- renderPlotly({q})
    output$distPlot3 <- renderPlotly({r})
    output$distPlot4 <- renderPlotly({s})
    output$distPlot5 <- renderPlotly({t})
}

shinyApp(ui = ui, server = server)
