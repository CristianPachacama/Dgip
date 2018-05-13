#=================================================================
#!!!!!!!!!!!!!!!!!!!!    MODELO LOGISTICO    !!!!!!!!!!!!!!!!!!!!!
#=================================================================
# facultad_aux ="INGENIERIA ELECTRICA Y ELECTRONICA"
# carrera_aux ="INGENIERIA ELECTRONICA Y CONTROL"
# periodo_aux="2017-B"
# particion=0.7
# FILTRO DE DATOS   ==============================================
BDD_micro = BDDsae_spk  %>% dplyr::filter(facultadMateria==facultad_aux)
BDD_micro = BDD_micro %>% 
  mutate_all(funs(replace(., (.=="NULL")|(.=="NA")|(.=="NaN")|is.nan(.), NA)))
#Correccion Variables (a Numericas)
BDD_micro$calificacion1 = as.numeric(BDD_micro$calificacion1)
BDD_micro$calificacion2 = as.numeric(BDD_micro$calificacion2)
BDD_micro$creditosMateria = as.numeric(BDD_micro$creditosMateria)
BDD_micro$numMatricula = as.numeric(BDD_micro$numMatricula)
BDD_micro$ingreso = as.numeric(BDD_micro$ingreso)
BDD_micro$miembrosFamilia = as.numeric(BDD_micro$miembrosFamilia)
#Crear Levels de Aprueba 
BDD_micro$Estado_Aprob = factor(BDD_micro$aprueba,
                                labels=c("NoConcluye","Falla","Supletorio",
                                         "FallaAsistencia","FallaSancion","Aprueba","Exonera"),
                                levels = c("N","F","S","FA","FS","A","E"),ordered = T)
#Corregir Nota 1
BDD_micro$calificacion1Corr = BDD_micro$calificacion1
BDD_micro$calificacion1Corr[BDD_micro$calificacion1>10 & !is.na(BDD_micro$calificacion1)] = 
  BDD_micro$calificacion1Corr[BDD_micro$calificacion1>10 & !is.na(BDD_micro$calificacion1)]/2
#Agregar Variable para LOGIT
BDD_micro = BDD_micro %>% mutate(Reprueba=if_else(aprueba %in% c("A","E"),0,1))
BDD_micro$ReprobacionReal = factor(BDD_micro$Reprueba,labels = c("Aprueba","Reprueba"),
                                   levels = c(0,1))

#Funcion de Correccion de NA's =================================
correccion_na = function(BDD_micro){
  #Calificacion 1 (Correcciones)  ------------------------------
  
  #Tabla de Calificaciones Promedio x Materia 
  resum_calif1 <- BDD_micro%>% 
    group_by(carrera,materia,periodo) %>% 
    summarise(Calif1_promedio=mean(calificacion1Corr,na.rm = TRUE)) %>% 
    mutate_all(funs(replace(., is.nan(.), NA)))
  #Reemplazar NA's por el promedio de la Materia
  ind_na <- which(is.na(BDD_micro$calificacion1Corr))
  for (i in ind_na) {
    j = which(resum_calif1$materia == BDD_micro$materia[i] &
                resum_calif1$periodo == BDD_micro$periodo[i] &
                resum_calif1$carrera == BDD_micro$carrera[i])
    BDD_micro$calificacion1Corr[i] = resum_calif1$Calif1_promedio[j]-3
  }
  
  #Creditos Materia(Correcciones)  ------------------------------
  #Tabla de Creditos Promedio x Materia 
  resum_creditos <- BDD_micro%>% 
    group_by(carrera,materia) %>% 
    summarise(Creditos_promedio=mean(creditosMateria,na.rm = TRUE)) %>% 
    mutate_all(funs(replace(., is.nan(.), NA)))
  #Reemplazar NA's por el promedio de la Materia
  ind_na <- which(is.na(BDD_micro$creditosMateria))
  for (i in ind_na) {
    j = which(resum_creditos$materia == BDD_micro$materia[i] &
                #resum_calif1$periodo == BDD_micro$periodo[i] &
                resum_creditos$carrera == BDD_micro$carrera[i])
    BDD_micro$creditosMateria[i] = resum_creditos$Creditos_promedio[j]-3
  }
  
  
  
  return(BDD_micro)
}

#Modelo LOGIT  =================================================
logit_model = function(BDD_micro,particion=0.8){
  BDD_limpia = correccion_na(BDD_micro)
  #Particion de Data
  set.seed(1)
  n=dim(BDD_micro)[1]
  inTrain = sample(1:n,size = round(n*particion))
  
  #Data Training (No titulacion) ------
  train_saew1 = BDD_micro[inTrain,] %>% dplyr::filter(creditosMateria<12) %>% 
    select(codigoEstudiante,Reprueba,ReprobacionReal,creditosMateria,sexo,
           estadoCivil,numMatricula,ingreso,calificacion1Corr,miembrosFamilia)
  #Titulacion
  train_saew2 = BDD_micro[inTrain,] %>% dplyr::filter(creditosMateria>=12) %>% 
    select(codigoEstudiante,Reprueba,ReprobacionReal,creditosMateria,sexo,
           estadoCivil,numMatricula,ingreso,calificacion1Corr,miembrosFamilia)
  
  #Modelo -------------
  modelo1 = glm(formula = Reprueba ~ creditosMateria +
                  sexo + numMatricula + ingreso+
                  calificacion1Corr + miembrosFamilia,
                family='binomial',data=train_saew1)
  
  modelo2 = glm(formula = Reprueba ~ creditosMateria +
                  sexo + numMatricula + ingreso+
                  miembrosFamilia,
                family='binomial',data=train_saew2)
  #Data Test  ---------
  # test_saew = BDD_limpia[-inTrain,] %>%
  #   select(codigoEstudiante,Reprueba,ReprobacionReal,creditosMateria,sexo,
  #          estadoCivil,numMatricula,ingreso,calificacion1Corr,miembrosFamilia)
  
  #Validacion ----------
  # proba = as.numeric(predict(modelo1,test_saew,type = "response"))
  # prediccion = as.numeric(proba > 0.5)
  # precision = 100*mean(prediccion == test_saew$Reprueba,na.rm = T)
  
  #Consolidacion ------
  BDD_micro$creditosMateria = as.numeric(BDD_micro$creditosMateria)
  
  casos1 = BDD_micro$creditosMateria<12
  casos2 = BDD_micro$creditosMateria>=12
  
  if(!is.na(casos1) && !is.na(casos2)){
    #Quitar casos NAs de Creditos
    casos1[is.na(casos1)] = FALSE
    casos2[is.na(casos2)] = FALSE
    
    BDD_micro$RiesgoReprob = NA
    BDD_micro$RiesgoReprob[casos1] = as.numeric(predict(modelo1,
                                                        BDD_limpia[casos1,], 
                                                        type = "response"))
    BDD_micro$RiesgoReprob[casos2] = as.numeric(predict(modelo2,
                                                        BDD_limpia[casos2,],
                                                        type = "response"))
    
    BDD_micro$RepruebaPredic = as.numeric(BDD_micro$RiesgoReprob >= 0.5)
    
    BDD_micro$ReprobacionPredic = factor(BDD_micro$RepruebaPredic,
                                         labels = c("Aprueba","Reprueba"),
                                         levels = c(0,1))
    
    BDD_micro = BDD_micro %>% arrange(periodo)
    
  }else{
    
    BDD_micro$RiesgoReprob = NA
    BDD_micro$RiesgoReprob = as.numeric(predict(modelo2,
                                                BDD_limpia, 
                                                type = "response"))
    
    BDD_micro$RepruebaPredic = as.numeric(BDD_micro$RiesgoReprob >= 0.5)
    
    BDD_micro$ReprobacionPredic = factor(BDD_micro$RepruebaPredic,
                                         labels = c("Aprueba","Reprueba"),
                                         levels = c(0,1))
    
    BDD_micro = BDD_micro %>% arrange(periodo)
    
  }
  
  return(list("modelo"=modelo1,
              "datosPredic"=BDD_micro))
}

BDD_micro = logit_model(BDD_micro)$datosPredic

#Precision del Modelo Logit
precision = 100*mean(BDD_micro$Reprueba==BDD_micro$RepruebaPredic,na.rm = TRUE)
