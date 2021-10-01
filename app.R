
# PROYECCIONES ACTUARIALES DEL IPS

library(shiny)
#library(DT)
#library(dygraphs)
#library(tidyverse) # %>%
#library(magrittr)  # %<>%
#library(lubridate)
#library(plotly)
#library(xts)
#library(dygraphs)
#directorio de trabajo
#setwd("C:/proyact/shiny")

#npm install bootstrap-icons


# lista de iconos https://getbootstrap.com/docs/4.5/extend/icons/

ui<- dashboardPage(skin="blue",
                   dashboardHeader(title="Estudio Actuarial del IPS", titleWidth = 500),
                   dashboardSidebar(
                       sidebarMenu(
                           menuItem("Población", tabName = "pob"),
                           menuItem("Laboral", tabName = "lab"),
                           menuItem("Cobertura", tabName = "cob"),
                           menuItem("Económicos", tabName = "eco"),
                           menuItem("Salarios", tabName = "sal"),
                           menuItem("Pensiones", tabName = "pen")

                       )
                   ),
                   
                   dashboardBody(
                       
                       tabItems(
                           tabItem("pob",
                                   h1("Proyecciones de Población"),
                                   tabsetPanel(type = "tabs",
                                               
                                               tabPanel("Gráfica 1", dygraphOutput("pobg2")),
                                               
                                               tabPanel("Tabla", dataTableOutput("tablapob0"))
                                   )
                                   

                           ),
                           
                           
                           tabItem("lab",
                                     h1("Proyecciones del mercado laboral"),
                                     tabsetPanel(type = "tabs",
                                                 
                                                 tabPanel("Gráfica 1", dygraphOutput("labg1")),
                                                 
                                                 tabPanel("Gráfica 2", plotOutput("labg2")),
                                                 tabPanel("Tabla", dataTableOutput("tablalab1"))
                                     )
                                   
                                   
                           ),
                           
                           
                           
                           tabItem("cob",
                                     h1("Proyecciones sobre Cobertura"),
                                     tabsetPanel(type = "tabs",
                                                 
                                           
                                                 
                                                 tabPanel("Gráfica 1", plotOutput("cobg1")),
                                                 
                                                 tabPanel("Gráfica 2", plotOutput("cobg2")),
                                                 
                                                 tabPanel("Tabla", dataTableOutput("tablacob"))
                                     )
                                   
                                   
                           ),
                           
                           
                           
                           tabItem("sal",
                                     h1("Proyecciones sobre salarios"),
                                   tabsetPanel(type = "tabs",
                                               
                                               tabPanel("Tabla", dataTableOutput("tablasal")),
                                               
                                               tabPanel("Gráfica 1", plotOutput("salg1")),
                                               
                                               tabPanel("Gráfica 2", plotOutput("salg2")),
                                               
                                               tabPanel("Gráfica 3", plotOutput("salg3")),
                                               
                                               tabPanel("Gráfica 4", plotlyOutput("salg4"))
                                   )
                                   
                                   
                           ),
                           
                           
                           tabItem("pen",
                                     h1("Proyecciones sobre Pensiones"),
                                   tabsetPanel(type = "tabs",
                                               
                                               tabPanel("Tabla", dataTableOutput("tablapen")),
                                               
                                               tabPanel("Gráfica 1", plotOutput("peng1")),
                                               
                                               tabPanel("Gráfica 2", plotOutput("peng2")),
                                               
                                               tabPanel("Gráfica 3", plotOutput("peng3")),
                                               
                                               tabPanel("Gráfica 4", plotlyOutput("peng4"))
                                   )
                                   
                                   
                           )
                           
                           
                       )
                   )
)

server<-function(input,output,session){
    
   
  #POBLACION 
  tpob<- reactive({
    
    data <- read.csv("pobproy.csv")
    data$fecha %<>% mdy()
    
    colnames(data) = c("Año","Fecha","Sexo","Cantidad")
    
    return(data)
  })
  
  
  

  
  # Gráfico 1
  output$pobg2 <- renderDygraph({
    
    datosaño2 = aggregate(
      cbind(Cantidad) ~ Fecha,
      data = tpob(), 
      FUN = sum)
    
    
    datosaño2_ts <- xts(x = datosaño2[, 1:2],
                       order.by = datosaño2$Fecha)
    dygraph(datosaño2_ts) %>%
      dyOptions(labelsUTC = F, labelsKMB = F,
                fillGraph = T, 
                drawGrid = F, colors = "#D9AE55") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.4,
                  hideOnMouseOut = FALSE) %>%
      dyRoller(rollPeriod = 2)

  })
  
  
  # gráfico 3


  output$tablapob0 <- renderDataTable({
    
    tablapob = aggregate(
      cbind(Cantidad) ~ Fecha,
      data = tpob(), 
      FUN = sum)
  })
  
  
  #LABORALES
  
  tablalab<- reactive({
    
    tlab1 <- read.csv("proylab.csv")
    tlab1$fecha %<>% mdy()
    colnames(tlab1) = c("Año","Total_M","Total_M","Activos_H","Activos_M","Ocupados_H","Ocupados_M","Fecha")
    return(tlab1)
  })
  
  output$tablalab1<- renderDataTable({
    tablalab()
  })
  
  
  # Gráfico 1
  output$labg1 <- renderDygraph({
    
    datoslabaño1 = aggregate(
      cbind(Activos_H, Activos_M) ~ Fecha,
      data = tablalab(), 
      FUN = sum)
    
    
    datoslabaño1_ts <- xts(x = datoslabaño1[, 1:3],
                        order.by = datoslabaño1$Fecha)
    dygraph(datoslabaño1_ts) %>%
      dyOptions(labelsUTC = TRUE, labelsKMB = TRUE,
                fillGraph = TRUE, 
                drawGrid = F, colors = "#D9AE55") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.4,
                  hideOnMouseOut = FALSE) %>%
      dyRoller(rollPeriod = 2)
  })
    
  #ECONOMICOS
  
  
  #

}


shinyApp(ui,server)
