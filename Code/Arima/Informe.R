#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#==================     ARIMA    =============================
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



# Funcion INFORME REPROBADOS ----------------------------------
informe_data_ARIMA <- eventReactive(input$Informe_boton2,{
  
  # INPUTS INFORME ----------------
  facultad_aux <- input$facultad_in2
  carrera_aux <- input$carrera_in2
  period_aux  <- input$periodo_in2
  n_periodos_aux <- as.numeric(input$n_periodo_in2)
  #Lista TOTAL Periodos Condicionada x Facultad
  period_lista <- BDDsae_spk %>% 
    dplyr::filter(facultadMateria==facultad_aux & carrera==carrera_aux) %>%
    select(periodo) %>% distinct() %>% #dplyr::filter(!endsWith(periodo,"R")) %>% 
    arrange(periodo)
  period_lista <- period_lista[!endsWith(period_lista$periodo,"R"),]
  #Lista de Periodos (Historico) hasta la seleccionada
  period_lista_aux <- period_lista$periodo[1:(which(period_lista$periodo==period_aux)-1)]
  period_lista_aux <- period_lista_aux[(length(period_lista_aux)-n_periodos_aux+1):length(period_lista_aux)]
  
  period_lista_aux0<-period_lista_aux
  
  period_lista_aux <- c("Materia",period_lista_aux)
  
  #Filtro de datos por Facultad y Carrera
  BDD_aux <- BDDsae_spk  %>% 
    dplyr::filter(carrera==carrera_aux & facultadMateria==facultad_aux)
  
  #Corregir Data NULL y NA
  BDD_aux <- BDD_aux %>% mutate_all(funs(replace(., (.=="NULL")|(.=="NA"), NA)))
  
  # Tabla 1: Paralelos  ----------------------------------------
  paralelos_total<- BDD_aux %>%  #Se usa la BDD de consola R(no Spark)
    rename(Materia=materia,Periodo=periodo)%>% 
    group_by(Materia,Periodo) %>%
    summarise(Paralelos=n_distinct(paralelo)) %>% 
    arrange(Periodo, Materia) #%>% 
    # dcast(formula =  Materia ~ Periodo,fun.aggregate =  sum, value.var="Paralelos")
  paralelos_total<-dcast(paralelos_total,
                         formula =  Materia ~ Periodo,
                         fun.aggregate =  sum, 
                         value.var="Paralelos")
  paralelos_aux <- paralelos_total %>% select(Materia,Paralelos=period_aux)
  paralelos_aux$Materia = as.character(paralelos_aux$Materia)
  
  #Tabla 2: Matriculados --------------------------------------
  matriculados <- BDD_aux %>% 
    rename(Materia=materia, Periodo=periodo) %>% 
    group_by(Materia,Periodo) %>% 
    summarise(Matriculados=n_distinct(codigoEstudiante))%>% 
    arrange(Periodo, Materia) #%>% 
    # dcast(formula =  Materia ~ Periodo,fun.aggregate =  sum, value.var="Matriculados")
  matriculados <- dcast(matriculados,
                        formula =  Materia ~ Periodo,
                        fun.aggregate =  sum, 
                        value.var="Matriculados")
  matriculad_aux <- matriculados %>% select(Materia,Matriculados=period_aux)
  matriculad_aux$Materia = as.character(matriculad_aux$Materia)
  matriculad_hist <- matriculados %>% select(period_lista_aux)
  
  #Matriculados Real
  matriculad_real <- matriculados %>% select(c(period_lista_aux,period_aux))
  
  #Tabla 3: Reprobados ----------------------------------------
  reprobados <- BDD_aux %>% 
    rename(Materia = materia, Periodo = periodo, CreditosxMateria = creditosMateria) %>% 
    mutate(Reprueba=as.numeric( !(aprueba=="E" | aprueba=="A"))) %>% 
    group_by(Materia,Periodo) %>% 
    summarise(Total_Reprob=sum(Reprueba)) %>% 
    arrange(Periodo, Materia) #%>% 
    # dcast(formula = Materia ~ Periodo, fun.aggregate = sum , value.var = "Total_Reprob")
  reprobados <- dcast(reprobados,
                      formula = Materia ~ Periodo, 
                      fun.aggregate = sum , 
                      value.var = "Total_Reprob")
  reprobados_hist <- reprobados %>% select(period_lista_aux)
  #Reprobados Real
  reprobados_real <- reprobados %>% select(c(period_lista_aux,period_aux))
  
  #Tabla 4: Porcentaje Reprobados x Materia -------------------
  porcentaje_rprb_hist <- reprobados_hist[,-1]/matriculad_hist[,-1] 
  porcentaje_rprb_hist <- porcentaje_rprb_hist  %>% mutate_all(funs(replace(., is.nan(.), NA))) #Cambiar NaN y NA por Cero
  porcentaje_rprb_hist <- data.frame("Materia"=reprobados_hist$Materia,porcentaje_rprb_hist)
  
  porcentaje_rprb_hist0 <- porcentaje_rprb_hist
  porcentaje_rprb_hist0$Materia = as.character(porcentaje_rprb_hist0$Materia)
  
  arima_na <-function(serie_aux){
    
    prediccion = mean(serie_aux,na.rm = TRUE)
    tryCatch(expr = {
      
      if(sum(!is.na(serie_aux))>=4 & n_distinct(serie_aux)>=4){
        serie_aux=ts(serie_aux)
        serie_aux = tsclean(serie_aux)
        fit = ar(serie_aux, order.max = 3)
        fit$x = serie_aux
        prediccion = forecast(fit,level = 95,h=1)$mean %>% as.numeric()
        serie_aux = as.numeric(serie_aux)
      }
      
    }, 
    error=function(e){}
    
    )
    
    # if(sum(!is.na(serie_aux))>=4 & n_distinct(serie_aux)>=4){
    #   serie_aux=ts(serie_aux)
    #   serie_aux = tsclean(serie_aux)
    #   fit = ar(serie_aux, order.max = 3)
    #   fit$x = serie_aux
    #   prediccion = forecast(fit,level = 95,h=1)$mean %>% as.numeric()
    # }else{
    #   prediccion = mean(serie_aux,na.rm = TRUE)
    # }
    
    return(prediccion)
  }
  
  porcentaje_rprb_hist$Reprob_Media <- apply(X=porcentaje_rprb_hist[,c(-1)],MARGIN=1,FUN=arima_na)
  
  #Porcentaje Real 
  porcentaje_rprb_histR <- reprobados_real[,-1]/matriculad_real[,-1] 
  porcentaje_rprb_histR <- porcentaje_rprb_histR  %>% mutate_all(funs(replace(., is.nan(.), NA))) #Cambiar NaN y NA por Cero
  porcentaje_rprb_histR <- data.frame("Materia"=reprobados_real$Materia,porcentaje_rprb_histR)
  
  porcentaje_rprb_histR$Materia = as.character(porcentaje_rprb_histR$Materia)
  
  #Tabla 5: Pronostico Macro Perdidos -------------------------
  Porcentaje_Reprob <- data.frame("Materia"=reprobados_hist$Materia, 
                                  "Porcent_Reprob_Historic"=100*porcentaje_rprb_hist$Reprob_Media)
  Porcentaje_Reprob$Materia = as.character(Porcentaje_Reprob$Materia)
  
  Reprobados_fit <- data.frame("Materia"=reprobados_hist$Materia, "Reprobados_Prediccion"=
                                 round(as.numeric(porcentaje_rprb_hist$Reprob_Media)*as.numeric(matriculad_aux[,2])))
  Reprobados_fit$Materia = as.character(Reprobados_fit$Materia)
  
  Aprobados_fit <- data.frame("Materia"=reprobados_hist$Materia, "Aprobados_Prediccion"=
                                matriculad_aux[,2]-Reprobados_fit$Reprobados_Prediccion)
  Aprobados_fit$Materia = as.character(Aprobados_fit$Materia)
  #Informe Resumen
  informe <- paralelos_aux %>% 
    inner_join(matriculad_aux, by="Materia") %>% 
    inner_join(Porcentaje_Reprob, by="Materia") %>% 
    inner_join(Reprobados_fit, by="Materia") %>% 
    inner_join(Aprobados_fit, by="Materia") %>%
    dplyr::filter(Paralelos != 0 | Matriculados != 0)
  
  #Tabla Histrico de reprobacion Materias (incluido PREDICCION)
  predic_aux = informe %>% select(Materia,Porcent_Reprob_Historic)
  predic_aux$Materia = as.character(predic_aux$Materia)
  tabla_historico <- porcentaje_rprb_hist0 %>% 
    inner_join(predic_aux ,by="Materia")
  
  names(tabla_historico)[-1] <- c(period_lista_aux0,paste(period_aux,"Predict"))
  
  #Tabla Pero con Porcentaje REAL del ultimo periodo
  mat_aux = informe %>% select(Materia)
  mat_aux$Materia = as.character(mat_aux$Materia)
  
  tabla_real <- porcentaje_rprb_histR %>% 
    inner_join(mat_aux ,by="Materia")
  
  names(tabla_historico)[-1] <- c(period_lista_aux0,paste(period_aux,"Real"))
  
  
  return(list('informe'=informe,'tabla_historico'=tabla_historico,'tabla_real'=tabla_real))
})

#Titulo de Informe MACRO -------------------------------------
output$titulo_macro2 <- renderText({ 
  paste("Informe ", input$facultad_in2 ," (",input$periodo_in2, ")")
})

# Descargar tabla Promedio -----------------------------------
output$downloadData2 <- downloadHandler(
  filename = function() {
    paste("Datos_Arima", Sys.Date(), ".csv")#, sep="")
  },
  content = function(file) {
    #write.xlsx(tabla_resumen_dowload(), file)
    write.table(informe_data_ARIMA()$informe, file, row.names=FALSE, dec = ",", sep = "|", na = "")
  }
)

#Genera Tabla Informe ----------------------------------------
output$informe_reprob2<-renderDataTable(server = FALSE,
                                        expr={
                                          informe_data_ARIMA()$informe %>%  
                                            datatable(filter = 'top',
                                                      options = list(pageLength = 10, searchHighlight = TRUE) ) %>% 
                                            formatRound(columns = 'Porcent_Reprob_Historic') %>% 
                                            formatStyle(columns = 'Porcent_Reprob_Historic',
                                                        background = styleColorBar(c(0,100),color='lightblue'),
                                                        backgroundSize = '98% 88%',
                                                        backgroundRepeat = 'no-repeat',
                                                        backgroundPosition = 'center')
                                        }
)
