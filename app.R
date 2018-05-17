#########################################################################
##### Installing packages into ‘/usr/local/lib/R/site-library’   ########
#########################################################################
### Instalcion de Paquetes para Shiny Server
# sudo su - -c "R -e \"install.packages('shiny')\""
# sudo su - -c "R -e \"install.packages('shinythemes')\""
# sudo su - -c "R -e \"install.packages('flexdashboard')\""
# sudo su - -c "R -e \"install.packages('DT')\""
# sudo su - -c "R -e \"install.packages('highcharter')\""
# sudo su - -c "R -e \"install.packages('plotly')\""
# sudo su - -c "R -e \"install.packages('tidyverse')\""
# sudo su - -c "R -e \"install.packages('reshape2')\""
# sudo su - -c "R -e \"install.packages('tseries')\""
# sudo su - -c "R -e \"install.packages('forecast')\""

#########################################################################

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#-------------------   MODELO PREDICTIVO V10 DGIP  ------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(shiny)
library(shinythemes)
library(flexdashboard)
#Tablas y Graficos
library(DT)
library(highcharter)
library(plotly)
#Bases de Datos
library(tidyverse)
library(dplyr)
library(reshape2)
#Proyeccion Series Tiempo
library(tseries)
library(forecast)
#>> Carga de Datos
load('Data/Datos_SAE_Act.RData')
# PARAMETROS INICIALES -----------------------------------------
source("Code/ParametrosIniciales.R",local = TRUE)

# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!    USER INTERFACE   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
ui <- navbarPage(title = "Modelo Predictivo DGIP",
                 header = tags$h2("Header-Plataforma",tags$head(tags$link(rel='shortcut icon', href='epn.ico', type='image/x-icon'))),
                 position = "fixed-top",#theme=shinytheme('flatly'),#theme = 'estilo.css',
                 footer = fluidRow(column(12,img(src='epn_logo.png',width='30px',align='center'),
                                          tags$b('Proyecto: '),' "Modelo Predictivo para Reprobación de Estudiantes"' ,
                                          '-',tags$a('DGIP-EPN (2018)',href='http://www.epn.edu.ec'),
                                          tags$b('  ||  '),tags$b('Desarrollado por: '),
                                          tags$a('C. Pachacama &',href='http://www.linkedin.com/in/cristian-david-pachacama'),
                                          tags$a('M. Sanchez',href='http://www.linkedin.com/in/miguel-ángel-sánchez-epn')
                 )
                 ),
                 #Header de la Pagina
                 #tags$head(tags$link(rel='shortcut icon', href='iconoEPN.ico', type='image/x-icon')),
                 #INTRODUCCION E INFORMACION DEL PROYECTO ----------------
                 tabPanel('Introducción',icon=icon('home'),
                          fluidRow(
                            
                            sidebarPanel(img(src='epn_logo2.png',width='90%',align='center' ),
                                         fluidRow(' '),shiny::hr(),
                                         fluidRow(
                                           column(3,tags$b('Proyecto:')),column(1),
                                           column(8,'Modelo Predictivo para Reprobación de estudiantes.')
                                         ),shiny::hr(),
                                         fluidRow(
                                           column(3,tags$b('Unidad:')),column(1),
                                           column(8,'Dirección de Gestión de la Información')
                                         ),shiny::hr(),
                                         fluidRow(
                                           column(3,tags$b('Director:')),column(1),
                                           column(8,'Msc. Roberto Andrade')
                                         ),shiny::hr(),
                                         fluidRow(
                                           column(3,tags$b('Analístas:')),column(1),
                                           column(8,'Miguel Angel Sánchez & Cristian Pachacama')
                                         ),shiny::hr()
                                         
                            ),
                            
                            mainPanel(
                              tags$h3('Modelo Predictivo para Reprobación de estudiantes.'),
                              shiny::hr(),#tags$h4('Resumen'),
                              fluidRow(' '),
                              tags$p('El propósito de esta plataforma es el integrar en una sola interfaz
                                     un modelo que permita predecir el número de estudiantes que tomarán 
                                     determinada materia, basados en su comportamiento histórico y el de
                                     los estudiantes que han tomado dicha materia. Esto con la finalidad
                                     que es necesario conoceer esto para el adecuado provisionamiento de
                                     recursos tales como aulas. profesores, materiales, etc. La plataforma
                                     se compone de dos partes, referentes a análisis MACRO y MICRO del modelo.'),
                              tags$h4('Modelo MACRO'),
                              tags$p('La parte MACRO del modelo es en donde se aborda desde un
                                     enfoque general la reprobación de estudiantes, resumiendo esta 
                                     información (histórica y predicciones) en un reporte asociado a las 
                                     materias de cada Carrera. Se abordan dentro de este modelo dos 
                                     metodologías,la primera basada en el ', tags$i('Promedio') ,'de reprobación
                                     historíca de las materias, para realizar las predicciones.'),
                              tags$p('La segunda metodología basada en un modelo predictivo llamado ',
                                     tags$i('ARIMA,'), 'que de igual manera utiliza los porcentajes de reprobación
                                     histórico de las materias para realizar la predicción.'),
                              tags$p(tags$i('Observación. '),'Estas dos metodologías muestran resultados parecidos,
                                     en algunos casos el modelo de Promedios realiza mejores predicciones que el modelo ARIMA,
                                     usualmente por que el modelo ARIMA requiere de que exista suficiente información histórica de la materia,
                                     en cambio el modelo de Promedios funciona bien inclusive cuando se dispone de poca información
                                     histórica de una materia.'),
                              tags$h4('Modelo MICRO'),
                              tags$p('El otro enfoque, que llamamos MICRO, es un análisis desagregado de la
                                     información de cada uno de los alumnos y el "Riesgo de Reprobación" del
                                     mismo, esto para cada una de las materias de las distintas Carreras.'),
                              tags$h4(tags$b('Conclusiones')),
                              tags$p('El modelo MICRO resulta ser más preciso que el MACRO,
                                     se recomienda usarlo excepto en materias de Titulación, Cursos de Actualización, Posgrados y Laboratorios.')
                              
                              
                            )
                            
                          ),shiny::hr()
                          
                          
                 ),
                 #MODELO MACRO ===============================================
                 
                 navbarMenu("Modelo MACRO",
                            #Modelo Usando PROMEDIO de Porcentajes -----------
                            tabPanel('Promedio',
                                     
                                     fluidRow(
                                       
                                       sidebarPanel(width=4,
                                                    #Panel de Control INFORME MACRO --------
                                                    tags$h3('Panel de Control'),
                                                    tags$p('Para obtener un informe de reprobación por materia, 
                                                           primero especifique la siguiente información.'),
                                                    tags$p(tags$b('Observación. '),'Las predicciones se realizan para el Periodo Seleccionado, y 
                                                           se basan únicamente en información histórica del porcentaje de Reprobación.'),
                                                    selectInput(inputId='facultad_in',label='Seleccione Facultad',choices = facultad_lista_shy0),
                                                    selectInput(inputId='carrera_in',label='Seleccione Carrera',choices = NULL),
                                                    selectInput(inputId='periodo_in',label='Seleccione Periodo',choices = NULL),
                                                    #selectInput(inputId='n_periodo_in',label='Número de Periodos (Histórico)',choices = NULL),
                                                    sliderInput(inputId='n_periodo_in',label = 'Número de Periodos (Histórico)',min = 1,max = 10,value = 5),
                                                    
                                                    actionButton('Informe_boton',label='Generar Informe',icon = icon('newspaper-o')),shiny::hr(),
                                                    #Grafico Histórico Reprobacion ------------
                                                    highchartOutput('graf_mini_reprob',height = '280px'),
                                                    tags$p('Este gráfico muestra el porcentaje de reprobación histórico de la materia seleccionada.')
                                                    
                                       ),
                                       mainPanel(
                                         #Titulo Informe MACRO promedio
                                         tags$h2(textOutput("titulo_macro")),shiny::hr(),
                                         tags$h4(textOutput("facultad_macro")),
                                         tags$h4(textOutput("carrera_macro")),
                                         tags$h4(textOutput("periodos_macro")),shiny::hr(),
                                         #Tabla Informe MACRO promedio
                                         fluidRow(DT::dataTableOutput("informe_reprob"))  
                                         
                                       )
                                       
                                     ),shiny::hr()
                            ),
                            
                            #Modelo usando ARIMA de Porcentajes -----------------
                            tabPanel('ARIMA',
                                     
                                     fluidRow(
                                       
                                       sidebarPanel(width=4,
                                                    #Panel de Control INFORME MACRO2 ---------
                                                    tags$h3('Panel de Control'),
                                                    tags$p('Para obtener un informe de reprobación por materia, 
                                                           primero especifique la siguiente información.'),
                                                    tags$p(tags$b('Observación. '),'Las predicciones se realizan para el Periodo Seleccionado, y 
                                                           se basan únicamente en información histórica del porcentaje de Reprobación.'),
                                                    selectInput(inputId='facultad_in2',label='Seleccione Facultad',choices = facultad_lista_shy0),
                                                    selectInput(inputId='carrera_in2',label='Seleccione Carrera',choices = NULL),
                                                    selectInput(inputId='periodo_in2',label='Seleccione Periodo',choices = NULL),
                                                    selectInput(inputId='n_periodo_in2',label='Número de Periodos (Histórico)',choices = NULL),
                                                    
                                                    actionButton('Informe_boton2',label='Generar Informe',icon = icon('newspaper-o')),shiny::hr(),
                                                    #Grafico Histórico Reprobacion
                                                    # highchartOutput('graf_mini_reprob2',height = '280px')
                                                    plotlyOutput('graf_mini_reprob2',height = '300px'),
                                                    tags$p('Este gráfico muestra el porcentaje de reprobación histórico de la materia seleccionada.')
                                                    
                                       ),
                                       mainPanel(
                                         tags$h3(textOutput("titulo_macro2")),shiny::hr(),
                                         # Botón de descarga
                                         downloadButton("downloadData2", "Descargar Arima"), shiny::hr(),
                                         fluidRow(DT::dataTableOutput("informe_reprob2")),shiny::hr()
                                         
                                       )
                                       
                                     ),shiny::hr()
                            )
                            
                 ),
                 #MODELO MICRO ===============================================
                 
                 tabPanel("Modelo MICRO",
                            fluidRow(
                              
                              sidebarPanel(width=4,
                                           #Panel de Control INFORME MACRO --------
                                           tags$h3('Panel de Control'),
                                           tags$p('Para obtener un informe los estudiantes reprobados por materia, 
                                                           primero especifique la siguiente información.'),
                                           tags$p(tags$b('Observación. '),'Las predicciones se realizan para el Periodo Seleccionado, y 
                                                           es necesaria la información de la Calificación del primer Bimestre del estudiante',tags$i('(Calificación 1).')),
                                           selectInput(inputId='facultad_in3',label='Seleccione Facultad',choices = facultad_lista_shy0),
                                           selectInput(inputId='carrera_in3',label='Seleccione Carrera',choices = NULL),
                                           selectInput(inputId='periodo_in3',label='Seleccione Período',choices = NULL),
                                           #selectInput(inputId='n_periodo_in3',label='Número de Periodos (Histórico)',choices = NULL),
                                           sliderInput(inputId = 'n_periodo_in3',label='Número de Periodos (Histórico)',min = 1,max = 10,value = 7),
                                           tags$p('Presione el botón ',tags$i('Generar Informe'),' y posteriormente seleccione (dando click)
                                                  una de las materias de la lista, y se generará un reporte con la prediccion
                                                  de los estudiantes Aprobados/Reprobados de dicha materia.'),
                                           actionButton('Informe_boton3',label='Generar Informe',icon = icon('newspaper-o')),shiny::hr(),
                                           #Grafico Histórico Reprobacion ------------
                                           highchartOutput('graf_mini_reprob3',height = '300px'),
                                           tags$p('Este gráfico muestra el porcentaje de reprobación histórico de la materia seleccionada.')
                                           
                              ),
                              mainPanel(
                                # Informe Resumen por Carrera -------------------
                                tags$h2(textOutput("titulo_micro3")),shiny::hr(),
                                tags$h4(textOutput("facultad_micro3")),
                                tags$h4(textOutput("carrera_micro3")),
                                tags$h4(textOutput("periodos_micro3")),shiny::hr(),
                                
                                fluidRow(DT::dataTableOutput("informe_reprob3")),shiny::hr(),
                                
                                #Informe LOGIT por Materia(Seleccionada) --------
                                tags$h2('Predicción de Aprobados y Reprobados'),shiny::hr(),
                                fluidRow(
                                  
                                  column(width = 7,
                                         tags$h4(textOutput("materia_logit3")),
                                         tags$h4(textOutput("periodo_logit3")),
                                         tags$h4(textOutput("precision_logit3"))
                                  ),
                                  column(width = 2,
                                         tags$h4('Porcentaje Reprobación (Predicción): ')
                                  ),
                                  column(width = 3,
                                         gaugeOutput(outputId = "velocimetro",height = "100px")
                                  )
                                  
                                ),
                                shiny::hr(),
                                
                                #Tabla por Profesor y Paralelo
                                fluidRow(DT::dataTableOutput("informe_profesor3")),shiny::hr(),
                                
                                #Infome Logit
                                fluidRow(DT::dataTableOutput("informe_logit3")),shiny::hr(),
                                
                                #Validacion Modelo
                                tags$h2('Resumen del Modelo'),
                                fluidRow(verbatimTextOutput("validacion_logit3"))
                              )
                              
                            ),shiny::hr()
                 )
                 
)



# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
server <- function(input, output,session) {
  # ANALISIS MACRO 
  # >> PROMEDIOS ========================================================
  
  #Generacion de Listas de Carreras y Periodos -----------------
  source("Code/ListasInputPanel.R",local = TRUE)
  
  #Reactividad de Inputs Panel ---------------------------------
  source("Code/Promedios/ReactividadInput.R",local = TRUE)
  
  #Generación de Informe Reprobacion ---------------------------
  source("Code/Promedios/Informe.R",local = TRUE)
  
  #Grafico de Reprobación Historica ----------------------------
  source("Code/Promedios/Grafico.R",local = TRUE)
  
  # >> PREDICCION ARIMA  =================================================

  #Reactividad de Inputs Panel ---------------------------------
  source("Code/Arima/ReactividadInput.R",local = TRUE)

  #Generación de Informe Reprobacion ---------------------------
  source("Code/Arima/Informe.R",local = TRUE)
  
  #Grafico de Reprobación Historica ----------------------------
  source("Code/Arima/Grafico.R",local = TRUE)
  
  # >> MODELO MICRO - Logistico  =========================================
  
  #Reactividad de Inputs Panel ---------------------------------
  source("Code/ModeloMicro/ReactividadInput.R",local = TRUE)
  
  #Generación de Informe Reprobacion ---------------------------
  source("Code/ModeloMicro/Informe.R",local = TRUE)
  
  #Grafico de Reprobación Historica ----------------------------
  source("Code/ModeloMicro/Grafico.R",local = TRUE)
  
  #Generación de LOGIT por Materia  ----------------------------
  source("Code/ModeloMicro/InformeLogit.R",local = TRUE)
}


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     RUN APP      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
shinyApp(ui = ui, server = server)





