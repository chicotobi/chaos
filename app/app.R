library(shiny)
library(tidyverse)
library(deSolve)

ui <- fluidPage(
    fluidRow(
        column(3,
               sliderInput("a","a",min = -12/3, max = 0, value = -8/3,step=1/3),
               sliderInput("b","b",min = -15, max = 0, value = -10,step = 1),
               sliderInput("c","c",min = 0, max = 50, value = 28,step = 2),
               sliderInput("X","X",min = 0, max = 2, value = 1,step = 0.2),
               sliderInput("Y","Y",min = 0, max = 2, value = 1,step = 0.2),
               sliderInput("Z","Z",min = 0, max = 2, value = 1,step = 0.2)
        ),
        column(9,
               plotlyOutput("plt")
        )
    )
)

server <- function(input, output) {
    output$plt <- renderPlotly({
        parameters <<- c(a=input$a,b=input$b,c=input$c)
        state <<- c(X=input$X,Y=input$Y,Z=input$Z)
        lorenz <- function(t, state, parameters) { 
            with(as.list(c(state, parameters)),{  
                # rate of change+   
                dX <- a*X + Y*Z    
                dY <- b * (Y-Z)
                dZ <- -X*Y + c*Y - Z
                list(c(dX, dY, dZ))
            })
        }
        times <<- seq(0,100,0.01)
        ode(y = state, times = times, func = lorenz, parms = parameters) %>%
            as.data.frame() %>% plot_ly(x=~X, y=~Y, z=~Z, type='scatter3d', mode='lines')
    })
}

shinyApp(ui = ui, server = server)