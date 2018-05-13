#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#==========  Grafico Hist Reprobacion(Promedio)  =============
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#Funcion que Genera Mini Gráfico -----------------------------
grafico_historico3 = function(Tabla_historica,fila){
  fila=fila[length(fila)]
  if(is.null(fila)){fila=1}
  Serie = Tabla_historica[fila,]
  Materia = as.character(Serie$Materia)
  
  SerieL = data.frame(Periodo=names(Serie)[-1],Porcentaje=100*as.numeric(Serie[,-1]))
  
  grafico = SerieL %>% hchart(type = "column",name="Porcentaje",
                              hcaes(x=Periodo,y=Porcentaje),
                              showInLegend = FALSE) %>% 
    hc_title(text = Materia, style = list(fontSize = "15px")) %>% 
    #hc_subtitle(text = paste0("Predicción Periodo: ",Periodo_predict)) %>% 
    hc_xAxis(title ="") %>% 
    hc_yAxis(title=list(text="Porcentaje de Reprobación")) %>% 
    hc_add_theme(hc_theme_gridlight())
  return(grafico)
}
#Output Mini Grafico -----------------------------------------
output$graf_mini_reprob3 = renderHighchart(expr ={
  fila = input$informe_reprob3_row_last_clicked
  grafico = grafico_historico3(informe_data3()$tabla_historico,fila)
  return(grafico)
})                                       
