#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#============    Tabla de Reprobacion(LOGIT)   ===============
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#Titulo de Reporte LOGIT -------------------------------------
output$materia_logit3 <- renderText(expr = {
  fila = input$informe_reprob3_row_last_clicked
  fila = fila[length(fila)]
  if(is.null(fila)){fila=1}
  
  materia = informe_data3()$informe %>% select(Materia)
  materia = as.character(unlist(materia))
  materia = materia[fila]
  
  titulo=paste("Materia: ", materia)
  return(titulo)
})

output$periodo_logit3 <- renderText(expr = {
  titulo=paste("Período: ",input$periodo_in3)
  return(titulo)
})

output$precision_logit3 <- renderText(expr = {
  precision = paste0("Precisión: ", round(informe_data3()$precision,digits = 1),"%")
  return(precision)
})
#Reporte Paralelos y Profesores -----------------------------
output$informe_profesor3 <- renderDataTable(server = FALSE,
                                          expr={
                                            fila = input$informe_reprob3_row_last_clicked
                                            fila = fila[length(fila)]
                                            if(is.null(fila)){fila=1}
                                            
                                            facultad_aux = input$facultad_in3
                                            carrera_aux = input$carrera_in3
                                            period_aux  = input$periodo_in3
                                            
                                            materia_aux = informe_data3()$informe %>% select(Materia)
                                            materia_aux = as.character(unlist(materia_aux))
                                            materia_aux = materia_aux[fila]
                                            #Informe Logit
                                            informe_aux = informe_data3()$predic_logit %>%
                                              dplyr::filter(facultadMateria==facultad_aux &
                                                       carrera==carrera_aux &
                                                       periodo==period_aux &
                                                       materia==materia_aux
                                              ) %>% 
                                              select(codigoEstudiante,
                                                     Paralelo=paralelo,
                                                     nombProfesor,
                                                     calificacion1,calificacion2,
                                                     supletorio,Estado=Estado_Aprob,
                                                     #Riesgo_Reprobacion=RiesgoReprob,
                                                     RepruebaPredic,Reprueba)
                                            #Profesor Paralelo
                                            profesor = informe_aux %>% 
                                              group_by(Paralelo) %>% 
                                              summarise(Profesor=unique(nombProfesor))
                                            #Matriculados Por Paralelo
                                            matriculados = informe_aux %>% 
                                              group_by(Paralelo) %>% 
                                              summarise(Matriculas=n_distinct(codigoEstudiante))
                                            
                                            #Promedios Calificacion 1 x Paralelo
                                            promedio1 = informe_aux %>% 
                                              group_by(Paralelo) %>% 
                                              summarise("Calificacion1 Promedio"=mean(calificacion1,na.rm = TRUE))
                                            
                                            #Promedios Calificacion 2  x Paralelo
                                            promedio2 = informe_aux %>% 
                                              group_by(Paralelo) %>% 
                                              summarise("Calificacion2 Promedio"=mean(calificacion2,na.rm = TRUE))
                                            
                                            #Reprobados x Paralelo
                                            reprobados1 = informe_aux %>% 
                                              group_by(Paralelo) %>% 
                                              summarise(Reprobados=sum(Reprueba,na.rm = TRUE))
                                            
                                            #Reprobados Predichos x Paralelo
                                            reprobados2 = informe_aux %>% 
                                              group_by(Paralelo) %>% 
                                              summarise(`Reprobados Prediccion`=sum(RepruebaPredic,na.rm = TRUE))
                                            
                                            #Consolidacion de Datos
                                            tabla_aux = profesor %>% 
                                              inner_join(matriculados,by="Paralelo") %>%
                                              inner_join(promedio1,by="Paralelo") %>% 
                                              inner_join(promedio2,by="Paralelo") %>% 
                                              inner_join(reprobados1,by="Paralelo") %>% 
                                              inner_join(reprobados2,by="Paralelo")
                                            
                                            #Aplicar Formato Data Table
                                            tabla_aux= tabla_aux %>% datatable(#filter = 'top',
                                                                               # Extensiones Para Tabla Larga y Boton de Descarga
                                                                               #extensions = c('Buttons'), #'Responsive',
                                                                               options = list(
                                                                                 dom = 't',
                                                                                 # buttons = list('copy','print', list(
                                                                                 #   extend = 'collection',
                                                                                 #   buttons = c('csv', 'excel', 'pdf'),
                                                                                 #   text = 'Download'
                                                                                 # )),
                                                                                 pageLength = 5,
                                                                                 searching = FALSE,
                                                                                 searchHighlight = TRUE
                                                                                 ),rownames = FALSE
                                            ) %>% 
                                              formatRound(columns = c('Calificacion1 Promedio','Calificacion2 Promedio')) %>% 
                                              formatStyle(columns = c('Reprobados','Reprobados Prediccion'),
                                                          background = styleColorBar(c(0,max(1,dim(informe_aux)[1])),
                                                                                     color='yellow'),
                                                          backgroundSize = '98% 88%',
                                                          backgroundRepeat = 'no-repeat',
                                                          backgroundPosition = 'center')
                                            return(tabla_aux)
                                          }
)

#Tabla Reactiva Logit  ---------------------------------------
tabla_logit <- reactive({
  fila = input$informe_reprob3_row_last_clicked
  fila = fila[length(fila)]
  if(is.null(fila)){fila=1}
  
  facultad_aux = input$facultad_in3
  carrera_aux = input$carrera_in3
  period_aux  = input$periodo_in3
  
  materia_aux = informe_data3()$informe %>% select(Materia)
  materia_aux = as.character(unlist(materia_aux))
  materia_aux = materia_aux[fila]
  
  tabla_aux = informe_data3()$predic_logit %>%
    dplyr::filter(facultadMateria==facultad_aux &
             carrera==carrera_aux &
             periodo==period_aux &
             materia==materia_aux
    ) %>% 
    select(Estudiante = codigoEstudiante,
           N.Mat = numMatricula,
           Paral = paralelo,
           Calif1 = calificacion1,
           Calif2 = calificacion2,
           Suplet = supletorio,
           Estado = Estado_Aprob,
           `Riesgo Reprobar` = RiesgoReprob,
           Prediccion = ReprobacionPredic) %>% 
    arrange(desc(N.Mat),desc(`Riesgo Reprobar`))
})

#Reporte Prediccion LOGIT ------------------------------------
output$informe_logit3 <- renderDataTable(server = FALSE,
                                         expr={
                                           tabla_aux = tabla_logit()%>% 
                                             datatable(filter = 'top',
                                                       # Extensiones Para Tabla Larga y Boton de Descarga
                                                       extensions = c('Buttons'), #'Responsive',
                                                       options = list(dom = 'Bfrtip',
                                                                      buttons = list('copy','print', list(
                                                                        extend = 'collection',
                                                                        buttons = c('csv', 'excel', 'pdf'),
                                                                        text = 'Descargar'
                                                                      )),
                                                                      pageLength = 20,
                                                                      searching = FALSE,
                                                                      searchHighlight = TRUE),
                                                       rownames = FALSE
                                             ) %>% 
                                             formatRound(columns = 'Riesgo Reprobar') %>% 
                                             formatStyle(columns = 'Riesgo Reprobar',
                                                         background = styleColorBar(c(0,100),color='orange'),
                                                         backgroundSize = '98% 88%',
                                                         backgroundRepeat = 'no-repeat',
                                                         backgroundPosition = 'center')
                                           return(tabla_aux)
                                         }
)


#Velocimetro - Porcentaje de Reprobación ----------------------------
output$velocimetro <- flexdashboard::renderGauge(expr = {
  reprobacion = 100*mean(tabla_logit()$Prediccion=="Reprueba", na.rm = TRUE)
  reprobacion = round(reprobacion ,digits = 1)
  flexdashboard::gauge(value = reprobacion ,min = 0,max = 100,label = "Reprobación",symbol = "%",
                       sectors = gaugeSectors(success = c(0,15),warning = c(15,30),danger = c(30,100)))
  
})
