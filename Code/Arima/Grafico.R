#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#==================     ARIMA    =============================
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#Funcion que Genera Mini Gráfico ARIMA -----------------------
grafico_hist_arima = function(Tabla_historica,Tabla_real,fila){
  
  fila=fila[length(fila)]
  if(is.null(fila)){fila=1}
  Serie = Tabla_historica[fila,]
  Materia = as.character(Serie$Materia)
  Periodo_predict = input$periodo_in2
  
  #Tabla 
  SerieL0 = data.frame(Periodo=names(Serie)[-1],Porcentaje=100*as.numeric(Serie[,-1]))
  SerieL0=SerieL0[-dim(SerieL0)[1],]
  
  # Condicion -----------------------------
  periodos = as.character(SerieL0$Periodo)
  serie_aux = as.numeric(SerieL0$Porcentaje)
  
  prediccion = mean(serie_aux,na.rm = TRUE)
  superior = prediccion+1
  inferior = prediccion-1
  tryCatch(expr = {
    
    if(sum(!is.na(serie_aux))>=4 & n_distinct(serie_aux)>=4){
      serie_aux0=ts(serie_aux)
      serie_aux = tsclean(serie_aux0)
      fit = ar(serie_aux, order.max = 3)
      fit$x = serie_aux
      serie_aux = as.numeric(serie_aux0)
      
      aux = forecast(fit,level = 85,h=1)
      
      prediccion = aux$mean %>% as.numeric()
      superior = c(rep(NA,length(serie_aux)-1),serie_aux[length(serie_aux)],as.numeric(aux$upper))
      inferior = c(rep(NA,length(serie_aux)-1),serie_aux[length(serie_aux)],as.numeric(aux$lower))
      
    }
    
  }, 
  error=function(e){}
  
  )
  
  #Tabla incluida el Tipo de dato (Original, corregido, prediccion)
  SerieL = SerieL0 %>% mutate(Tipo= ifelse(is.na(Porcentaje),"Ajustado","Original"))
  SerieL$Porcentaje = serie_aux
  SerieL = SerieL %>% rbind(data.frame(Periodo=Periodo_predict,
                                       Porcentaje=prediccion,Tipo="Predicción"))
  
  #Grafico Colores HighCharter --------------------
  grafico = SerieL %>% hchart(type = "column",name="Porcentaje",
                              hcaes(x=Periodo,y=Porcentaje,color=Tipo),
                              showInLegend = FALSE) %>% 
    hc_title(text = Materia, style = list(fontSize = "15px")) %>% 
    hc_subtitle(text = paste0("Predicción Periodo: ",Periodo_predict)) %>% 
    hc_xAxis(title ="") %>% 
    hc_yAxis(title=list(text="Porcentaje de Reprobación")) %>% 
    hc_add_theme(hc_theme_gridlight())
  
  
  # #Grafico Prediccion lineas plotly -------------
  SerieReal = Tabla_real[fila,]
  SerieR0 = data.frame(Periodo=names(SerieReal)[-1],Porcentaje=100*as.numeric(SerieReal[,-1]))
  
  data = data.frame(Periodo=SerieL$Periodo,
                    Porcentaje=SerieL$Porcentaje,
                    PorcentSup=superior,
                    Prediccion=NA,
                    PorcentInf=inferior,
                    Tipo=SerieL$Tipo)
  data$Prediccion[length(serie_aux):(length(serie_aux)+1)] = SerieL$Porcentaje[length(serie_aux):(length(serie_aux)+1)]
  data$Porcentaje[length(serie_aux)+1] = NA
  data$Real = SerieR0$Porcentaje
  
  grafico2 = plot_ly(data, x = ~Periodo, y = ~PorcentSup, type = 'scatter', mode = 'lines',
                     line = list(color = 'transparent'),
                     showlegend = FALSE, name = 'IntervaloSup') %>%
    add_trace(y = ~PorcentInf, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='rgba(255,153,0,0.2)', line = list(color = 'transparent'),
              showlegend = FALSE, name = 'IntervaloInf') %>%
    add_trace(x = ~Periodo, y = ~Real, type = 'scatter', mode = 'lines',
              line = list(color='rgb(100,80,80)'),
              name = 'Valor Real') %>%
    add_trace(x = ~Periodo, y = ~Prediccion, type = 'scatter', mode = 'lines',
              line = list(color='rgb(255,153,0)'),
              name = 'PrediccionARIMA') %>%
    add_trace(x = ~Periodo, y = ~Porcentaje, type = 'scatter', mode = 'lines',
              line = list(color='rgb(0,100,80)'),
              name = 'PorcentajeReprob') %>%
    layout(title = Materia,
           xaxis = list(title = "Periodos"),
           yaxis = list(title = "Porcentaje de Reprobación"))
  
  
  
  return(grafico2)
}
#Output Mini Grafico ARIMA -----------------------------------
output$graf_mini_reprob2 = renderPlotly(expr ={
  fila=input$informe_reprob2_row_last_clicked
  info_aux = informe_data_ARIMA()
  grafico = plotly_build(grafico_hist_arima(info_aux$tabla_historico,
                                            info_aux$tabla_real,fila))
  grafico$elementId = NULL
  
  return(grafico)
})