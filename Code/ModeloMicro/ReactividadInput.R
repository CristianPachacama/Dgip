#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#===========  Reactividad de Inputs Promedio  ================
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Reactividad Carreras Informe MACRO -------------------------
observe({
  
  #Input que condiciona el valor del resto
  facultadInp <- input$facultad_in3
  
  #>>Actualiza Lista Carreras
  updateSelectInput(session,inputId = 'carrera_in3',
                    selected = lista_carreras(facultadInp)[1],
                    label = 'Seleccione Carrera',
                    choices=lista_carreras(facultadInp)
                    
  )
  
})

# Reactividad Periodos Informe MACRO -----------------------
observe({
  
  #Input que condiciona el valor del resto
  facultadInp <- input$facultad_in3
  carreraInp  <- input$carrera_in3
  
  #>>Actualiza Lista Periodos
  updateSelectInput(session,inputId = 'periodo_in3',
                    label = 'Seleccione Periodo',
                    choices=rev(lista_periodos(facultadInp,carreraInp))
                    
  )
  
})

# Reactividad Número de Periodos -----------------------------
observe({
  
  #Input que condiciona el valor del resto
  facultadInp <- input$facultad_in3
  carreraInp  <- input$carrera_in3
  periodoInp <- input$periodo_in3
  
  #>>Actualiza Lista Periodos
 
  numeros <- lista_n_periodos(facultadInp,carreraInp,periodoInp)
  
  updateSliderInput(
    session,inputId = 'n_periodo_in3',
    label = 'Número de Periodos (Historico)',
    min = min(numeros),
    max = max(numeros),
    value = min(7,round(numeros[1]/2.5))
  )
  
})