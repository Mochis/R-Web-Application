library(shiny)

shinyUI(navbarPage(title = "Práctica de Simulación en R", inverse = TRUE,
  tabPanel("Principal",
      headerPanel("Modelos Estadísticos y Simulación"),
      fluidRow(
          column(12,
                 plotOutput("main")
                 )
        )
      #mainPanel()
  ),
  
  navbarMenu("Distribuciones",
  
  ##============================= BINOMIAL NEGATIVA ================================           
  tabPanel("Binomial negativa",
             headerPanel("Distribución binomial negativa"),
               
               # Panel con parámetros de entrada
               sidebarPanel("Parámetros",
                hr(),
                sliderInput("nBinom", "N", min = 100, max = 10000, step = 100, value = 5000),
                numericInput("r","r", min=0, max=100, step=1, value=10),
                numericInput("pBinom","p", min=0.01, max=1, step=0.01, value=0.25),
                helpText("Nota: Los valores decimales se expresan con el caracter punto. Ej: 0.25"),
                hr(),
                sliderInput("probBinom", "Intervalo probabilidad", min = 0, max = 100, value = c(25,35)),
                hr()
                #submitButton("Simular")
               ),
               
               # Panel principal con gráfica y tabla de datos
               mainPanel(
                 tabsetPanel(
                   tabPanel("Gráfica",
                            tabsetPanel(
                              tabPanel("Distribución de frecuencias relativas",
                                       plotOutput("plotBinom"),
                                       checkboxInput("probRealBinom", label = "Comparar con distribución de probabilidad real", value = F)
                              ),
                              tabPanel("Función de distribución acumulada",
                                       plotOutput("plotBinom2")
                              ),
                              tabPanel("Distribución de probabilidad real",
                                       plotOutput("plotBinom3")
                              ),
                              tabPanel("Gráfico de cajas",
                                       plotOutput("plotBinom4")
                              )
                              
                            )
                   ),
                   tabPanel("Resumen estadístico", 
                            verbatimTextOutput("summaryBinom"),
                            verbatimTextOutput("summaryDataBinom"),
                            tableOutput("tableMeanBinom"),
                            tableOutput("tableProbBinom")
                   ),
                   tabPanel("Tabla de frecuencias", dataTableOutput("tableBinom")),
                   tabPanel("Tabla de datos", dataTableOutput("dataTableBinom"))                   
               )
             )
    ),
   ##============================= EXPONENCIAL ======================================= 
    tabPanel("Exponencial",
             headerPanel("Distribución exponencial"),
             sidebarPanel("Parámetros",
              hr(),
              sliderInput("nExp", "N", min = 100, max = 10000, step = 100, value = 5000),
              numericInput("lambda","Lambda", min = 0, max = 1000, step = 0.01, value = 0.1),
              helpText("Nota: Los valores decimales se expresan con el caracter punto. Ej: 0.25"),
              hr(),
              sliderInput("probExp", "Intervalo probabilidad", min = 0, max = 100, value = c(5,15)),
              hr()
              #submitButton("Simular")
             ),
             # Panel principal con gráfica y tabla de datos
             mainPanel(
               tabsetPanel(
                 tabPanel("Gráfica",
                          tabsetPanel(
                            tabPanel("Histograma de valores simulados",
                                     plotOutput("plotExp"),
                                     checkboxInput("densityExp", label = "Comparar con función de densidad", value = F)
                            ),
                            tabPanel("Función de distribución",
                                     plotOutput("plotExp2")
                            ),
                            tabPanel("Función de densidad",
                                     plotOutput("plotExp3")
                            ),
                            tabPanel("Gráfico de cajas",
                                     plotOutput("plotExp4")
                            )
                            
                          )
                 ),
                 tabPanel("Resumen estadístico", 
                          verbatimTextOutput("summaryExp"),
                          verbatimTextOutput("summaryDataExp"),
                          tableOutput("tableMeanExp"),
                          tableOutput("tableProbExp")
                 ),
                 tabPanel("Tabla de frecuencias", dataTableOutput("tableExp")),
                 tabPanel("Tabla de datos", dataTableOutput("dataTableExp"))                   
               ))
             
             ),
   ##================================ GEOMETRICA ===============================
    tabPanel("Geométrica",
             headerPanel("Distribución geométrica"),
             sidebarPanel("Parámetros",
              hr(),
              sliderInput("nGeom", "N", min = 100, max = 10000, step = 100, value = 5000),
              numericInput("pGeom","p", min = 0.01, max = 1, step = 0.01, value = 0.25),
              helpText("Nota: Los valores decimales se expresan con el caracter punto. Ej: 0.25"),
              hr(),
              sliderInput("probGeom", "Intervalo probabilidad", min = 0, max = 100, value = c(1,10)),
              hr()
              #submitButton("Simular")              
             ),
             # Panel principal con gráfica y tabla de datos
             mainPanel(
               tabsetPanel(
                 tabPanel("Gráfica",
                          tabsetPanel(
                            tabPanel("Distribución de frecuencias relativas",
                                     plotOutput("plotGeom"),
                                     checkboxInput("probRealGeom", label = "Comparar con distribución de probabilidad real", value = F)
                            ),
                            tabPanel("Función de distribución acumulada",
                                     plotOutput("plotGeom2")
                            ),
                            tabPanel("Distribución de probabilidad real",
                                     plotOutput("plotGeom3")
                            ),
                            tabPanel("Gráfico de cajas",
                                     plotOutput("plotGeom4")
                            )
                            
                          )
                 ),
                 tabPanel("Resumen estadístico", 
                          verbatimTextOutput("summaryGeom"),
                          verbatimTextOutput("summaryDataGeom"),
                          tableOutput("tableMeanGeom"),
                          tableOutput("tableProbGeom")
                 ),
                 tabPanel("Tabla de frecuencias", dataTableOutput("tableGeom")),
                 tabPanel("Tabla de datos", dataTableOutput("dataTableGeom"))                   
               ))
    ),
    tabPanel("Weibull",
             headerPanel("Distribución de Weibull"),
             sidebarPanel("Parámetros",
              hr(),
              sliderInput("nWeib", "N", min = 100, max = 10000, step = 100, value = 5000),
              numericInput("a","alpha", min = 0.01, max = 100, step = 0.01, value = 5),
              helpText("Nota: Los valores decimales se expresan con el caracter punto. Ej: 0.25"),
              hr(),
              sliderInput("probWeib", "Intervalo probabilidad", min = 0, max = 3, step = 0.01, value = c(0.5,1.5)),
              hr()
              #submitButton("Simular") 
             ),
             # Panel principal con gráfica y tabla de datos
             mainPanel(
               tabsetPanel(
                 tabPanel("Gráfica",
                          tabsetPanel(
                            tabPanel("Histograma de valores simulados",
                                     plotOutput("plotWeib"),
                                     checkboxInput("densityWeib", label = "Comparar con función de densidad", value = F)
                            ),
                            tabPanel("Función de distribución",
                                     plotOutput("plotWeib2")
                            ),
                            tabPanel("Función de densidad",
                                     plotOutput("plotWeib3")
                            ),
                            tabPanel("Gráfico de cajas",
                                     plotOutput("plotWeib4")
                            )
                            
                          )
                 ),
                 tabPanel("Resumen estadístico", 
                          verbatimTextOutput("summaryWeib"),
                          verbatimTextOutput("summaryDataWeib"),
                          tableOutput("tableMeanWeib"),
                          tableOutput("tableProbWeib")
                 ),
                 tabPanel("Tabla de frecuencias", dataTableOutput("tableWeib")),
                 tabPanel("Tabla de datos", dataTableOutput("dataTableWeib"))                   
               ))
    )
  )
))