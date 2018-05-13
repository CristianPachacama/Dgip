#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#==================     ARIMA    =============================
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Reactividad Carreras Informe MACRO --------------------------
observe({
  
  #Input que condiciona el valor del resto
  facultadInp <- input$facultad_in2
  
  #>>Actualiza Lista Carreras 
  updateSelectInput(session,inputId = 'carrera_in2',
                    selected = lista_carreras(facultadInp)[1],
                    label = 'Seleccione Carrera',
                    choices=lista_carreras(facultadInp)
                    
  )
  
})

# Reactividad Periodos Informe MACRO --------------------------
observe({
  
  #Input que condiciona el valor del resto
  facultadInp <- input$facultad_in2
  carreraInp  <- input$carrera_in2
  
  #>>Actualiza Lista Periodos 
  updateSelectInput(session,inputId = 'periodo_in2',
                    label = 'Seleccione Periodo',
                    choices=rev(lista_periodos(facultadInp,carreraInp))
                    
  )
  
})

# Reactividad Número de Periodos   ----------------------------
observe({
  
  #Input que condiciona el valor del resto
  facultadInp <- input$facultad_in2
  carreraInp  <- input$carrera_in2
  periodoInp <- input$periodo_in2
  
  #>>Actualiza Lista Periodos 
  updateSelectInput(session,inputId = 'n_periodo_in2',
                    label = 'Número de Periodos (Historico)',
                    choices= lista_n_periodos(facultadInp,carreraInp,periodoInp),
                    selected = round(lista_n_periodos(facultadInp,carreraInp,periodoInp)[1]/2)
                    
  )
  
})