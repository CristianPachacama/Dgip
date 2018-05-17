#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#==========  Informe de Reprobacion(Promedio)  ===============
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
mean_na <- function(x){mean(x,na.rm = TRUE)}
sum_na <- function(x){sum(x,na.rm = TRUE)}
# Funcion INFORME REPROBADOS ---------------------------------
informe_data3 <- eventReactive(input$Informe_boton3,{
  
  # INPUTS INFORME ----------------
  facultad_aux <- input$facultad_in3
  carrera_aux <- input$carrera_in3
  period_aux  <- input$periodo_in3
  n_periodos_aux <- as.numeric(input$n_periodo_in3)
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
  
  #Datos incluidos Prediccion LOGIT ----------------------------
  source("Code/ModeloMicro/Logit/ModeloLogit.R",local = TRUE)
  # BDD_micro contiene toda la base más prediccion LOGIT (estudiantes)
  
  #Filtro de datos por Facultad y Carrera
  BDD_aux <- BDD_micro  %>% 
    dplyr::filter(carrera==carrera_aux)
  
  #Corregir Data NULL y NA
  BDD_aux <- BDD_aux %>% mutate_all(funs(replace(., (.=="NULL")|(.=="NA"), NA)))
  
  # Tabla 1: Paralelos  ----------------------------------------
  paralelos_total<- BDD_aux %>%  
    rename(Materia=materia,Periodo=periodo)%>% 
    group_by(Materia,Periodo) %>%
    summarise(Paralelos=n_distinct(paralelo)) %>% 
    arrange(Periodo, Materia) #%>% 
    # dcast(formula =  Materia ~ Periodo,fun.aggregate =  sum, value.var="Paralelos")
  paralelos_total <- dcast(paralelos_total,
                           formula =  Materia ~ Periodo,
                           fun.aggregate =  sum, 
                           value.var="Paralelos")
  paralelos_aux <- paralelos_total %>% select(Materia,Paralelos=period_aux)
  
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
  matriculad_aux$Materia=as.character(matriculad_aux$Materia)
  matriculad_hist <- matriculados %>% select(period_lista_aux)
  
  #Matriculados Real
  matriculad_real <- matriculados
  
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
  reprobados_real <- reprobados
  Reprobados_Real = reprobados %>% select(Materia, Reprobados = period_aux)
  #Reprobados LOGIT
  reprobados_logit <- BDD_aux %>% 
    rename(Materia = materia, Periodo = periodo) %>%
    dplyr::filter(Periodo==period_aux) %>% 
    group_by(Materia,Periodo) %>% 
    summarise(Total_Reprob=sum(RepruebaPredic)) %>% 
    arrange(Periodo, Materia)
  reprobados_logit <- dcast(reprobados_logit,
                      formula = Materia ~ Periodo, 
                      fun.aggregate = sum , 
                      value.var = "Total_Reprob")
  
  #Tabla 4: Porcentaje Reprobados x Materia -------------------
  porcentaje_rprb_hist <- reprobados_hist[,-1]/matriculad_hist[,-1] 
  porcentaje_rprb_hist <- porcentaje_rprb_hist  %>% mutate_all(funs(replace(., is.nan(.), NA))) #Cambiar NaN y NA por Cero
  porcentaje_rprb_hist <- data.frame("Materia"=reprobados_hist$Materia,porcentaje_rprb_hist)
  
  porcentaje_rprb_hist0 <- porcentaje_rprb_hist
  porcentaje_rprb_hist0$Materia = as.character(porcentaje_rprb_hist0$Materia)
  
  porcentaje_rprb_hist$Reprob_Media <- apply(X=porcentaje_rprb_hist[,c(-1)],MARGIN=1,FUN=mean_na)
  
  #Porcentaje Real 
  # porcentaje_rprb_histR <- reprobados_real[,-1]/matriculad_real[,-1] 
  # porcentaje_rprb_histR <- porcentaje_rprb_histR  %>% mutate_all(funs(replace(., is.nan(.), NA))) #Cambiar NaN y NA por Cero
  # porcentaje_rprb_histR <- data.frame("Materia"=reprobados_real$Materia,porcentaje_rprb_histR)
  
  #Tabla 5: Pronostico Macro Perdidos -------------------------
  Porcentaje_Reprob <- data.frame("Materia"=reprobados_hist$Materia, 
                                  "Porcentaje Reprobacion Historico"=100*porcentaje_rprb_hist$Reprob_Media)
  names(Porcentaje_Reprob) <- c("Materia","Porcentaje Reprobacion Historico")
  Porcentaje_Reprob$Materia = as.character(Porcentaje_Reprob$Materia)
  
  # Reprobados_fit <- data.frame("Materia"=reprobados_hist$Materia, "Reprobados_Prediccion"=
  #                                round(as.numeric(porcentaje_rprb_hist$Reprob_Media)*as.numeric(matriculad_aux[,2])))
  # Reprobados_fit$Materia = as.character(Reprobados_fit$Materia)
  
  Reprobados_fit2 <- data.frame("Materia"=reprobados_logit$Materia,"Reprobados Prediccion"=reprobados_logit[,period_aux])
  Reprobados_fit2$Materia = as.character(Reprobados_fit2$Materia)
  names(Reprobados_fit2) <- c("Materia","Reprobados Prediccion")
  # Aprobados_fit <- data.frame("Materia"=reprobados_hist$Materia, "Aprobados_Prediccion"=
  #                               matriculad_aux[,2]-Reprobados_fit$Reprobados_Prediccion)
  # Aprobados_fit$Materia = as.character(Aprobados_fit$Materia)
  
  remove(porcentaje_rprb_hist,reprobados_hist) #<<<<
  #Informe Resumen
  informe <- paralelos_aux %>% 
    inner_join(matriculad_aux, by="Materia") %>% 
    inner_join(Porcentaje_Reprob, by="Materia") %>% 
    inner_join(Reprobados_Real, by= "Materia") %>%
    inner_join(Reprobados_fit2, by="Materia") %>% 
    dplyr::filter(Paralelos != 0 | Matriculados != 0)
  informe$Aprobados = informe$Matriculados-informe$Reprobados
  informe$`Aprobados Prediccion` = informe$Matriculados-informe$`Reprobados Prediccion`
  
  informe <- informe %>% select(Materia,Paralelos,Matriculados,
                                `Porcentaje Reprobacion Historico`,
                                `Reprobados Prediccion`,
                                `Aprobados Prediccion`,
                                Reprobados,Aprobados)
  #Tabla Histrico de reprobacion Materias
  tabla_historico <- informe %>% select(Materia) %>% 
    inner_join(porcentaje_rprb_hist0,by="Materia")
  
  names(tabla_historico)[-1] <- period_lista_aux0
  remove(porcentaje_rprb_hist0) #<<<<<<<<
  
  #Tabla de Prediccion LOGIT (estudiantes)
  BDD_logit = BDD_micro %>% 
    select(codigoEstudiante,sexo,estadoCivil,ciudad,
           materia,paralelo,periodo,numMatricula,
           calificacion1,calificacion2,supletorio,Estado_Aprob,
           aprueba,creditosMateria,carrera,facultadMateria,
           Reprueba,RepruebaPredic,RiesgoReprob,
           ReprobacionReal,ReprobacionPredic,
           nombreCarrera,facultadEstudiante,nombProfesor)
  BDD_logit$RiesgoReprob = 100*BDD_logit$RiesgoReprob
  remove(BDD_micro) #<<<<<<<<<<
  return(list('informe' = informe,
              'tabla_historico' = tabla_historico,
              'predic_logit' = BDD_logit,
              'precision' = precision,
              'Modelo1' = Modelo1,
              'Modelo2' = Modelo2))
})


#Titulo de Informe MICRO -------------------------------------
output$titulo_micro3 <- renderText({ 
  paste("Informe de Reprobación MICRO")
})
output$facultad_micro3 <- renderText({ 
  paste("Facultad: ", input$facultad_in3)
})
output$carrera_micro3 <- renderText({ 
  carrera_aux = input$carrera_in3
  if(is.null(carrera_aux)){carrera_aux="Selecciona Carrera"}
  texto=paste("Carrera: ", carrera_aux)
  return(texto)
})

output$periodos_micro3 <- renderText(expr={
  # INPUTS  ------- 
  facultad_aux <- input$facultad_in3
  carrera_aux <- input$carrera_in3
  period_aux  <- input$periodo_in3
  n_periodos_aux <- as.numeric(input$n_periodo_in3)
  
  #Hay que condicionar Previo a Reactividad INPUT's -----
  if(!is.null(facultad_aux)&
     !is.null(carrera_aux)&
     !is.null(period_aux)&
     !is.null(n_periodos_aux)){
    
    #Periodos Condicionada x Facultad
    period_lista <- BDDsae_spk %>% 
      dplyr::filter(facultadMateria==facultad_aux & carrera==carrera_aux) %>%
      select(periodo) %>% distinct() %>% 
      arrange(periodo)
    period_lista <- period_lista[!endsWith(period_lista$periodo,"R"),]
    #Periodos hasta la seleccionada
    ind_period = which(period_lista$periodo==period_aux)
    
    if(dim(period_lista)[1]>2 & !is_empty(ind_period)){
      period_lista_aux <- period_lista$periodo[1:(ind_period-1)]
      
      l_period <- length(period_lista_aux)
      if(!is.na(l_period) & !is.null(l_period) & 
         !is.na(n_periodos_aux) & !is.null(n_periodos_aux) &
         (l_period-n_periodos_aux+1)>0){
        period_lista_aux <- period_lista_aux[(l_period-n_periodos_aux+1):l_period]
      }else{
        period_lista_aux<- period_lista_aux[1:l_period]
      }
      
      texto=paste("Período: ", period_lista_aux[1], " / ",period_aux)
    }else{
      period_lista_aux ="Selecciona Período"
      texto=paste("Período: ", period_lista_aux)
    }
    
    
  }else{
    period_lista_aux ="Selecciona Período"
    texto=paste("Período: ", period_lista_aux)
  }
  
  return(texto)
})


#Genera Tabla Informe ----------------------------------------
output$informe_reprob3<-renderDataTable(server = FALSE,
                                       expr={
                                         informe_data3()$informe %>%  
                                           datatable(#filter = 'top',
                                                     # Extensiones Para Tabla Larga y Boton de Descarga
                                                     extensions = c('Responsive','Buttons'),#Scroller),
                                                     options = list(dom = 'Bfrtip',
                                                                    buttons = list('copy','print', list(
                                                                      extend = 'collection',
                                                                      buttons = c('csv', 'excel', 'pdf'),
                                                                      text = 'Descargar'
                                                                    )),
                                                                    #pageLength = 10,
                                                                    searching = FALSE,
                                                                    searchHighlight = TRUE
                                                                    ),
                                                     rownames = FALSE
                                                     ) %>% 
                                           formatRound(columns = 'Porcentaje Reprobacion Historico') %>% 
                                           formatStyle(columns = 'Porcentaje Reprobacion Historico',
                                                       background = styleColorBar(c(0,100),color='lightblue'),
                                                       backgroundSize = '98% 88%',
                                                       backgroundRepeat = 'no-repeat',
                                                       backgroundPosition = 'center')
                                       }
)




