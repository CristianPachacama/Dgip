#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#===========   Parametros Iniciales Inputs    ================
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

{
  facultad_listaR <- BDDsae_spk %>% select(facultadMateria) %>% distinct() %>% arrange(facultadMateria)
  facultad_lista_shy0 <- facultad_listaR$facultadMateria
  
  carrera_listaR <- BDDsae_spk %>% 
    select(carrera) %>% distinct() %>% arrange(carrera)
  carrera_lista_shy0 <- carrera_listaR$carrera
  
  period_listaR <- BDDsae_spk %>% 
    select(periodo) %>% distinct() %>% arrange(periodo)
  period_listaR <- period_listaR[!endsWith(period_listaR$periodo,"R"),]
  period_lista_shy0 <-period_listaR$periodo[-1]
}
