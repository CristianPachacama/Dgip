#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#===========  Listas de Carreras y Periodos  ================
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

lista_carreras <- function(facultadTmp=facultadInp){
  
  carrera_listaR <- BDDsae_spk %>% 
    dplyr::filter(facultadMateria==facultadTmp) %>% 
    select(carrera) %>% distinct() %>% arrange(carrera)
  carrera_lista_shy <- carrera_listaR$carrera
  
  return(carrera_lista_shy)
}

lista_periodos <- function(facultadTmp=facultadInp,carreraTmp=carreraInp){
  
  period_listaR<-BDDsae_spk %>% 
    dplyr::filter(facultadMateria==facultadTmp & carrera==carreraTmp) %>%
    select(periodo) %>% distinct() %>% #dplyr::filter(!endsWith(periodo,"R"))%>% 
    arrange(periodo)
  period_listaR <- period_listaR[!endsWith(period_listaR$periodo,"R"),]
  period_lista_shy <-period_listaR$periodo[-1]
  
  return(period_lista_shy)
}

lista_n_periodos <- function(facultadTmp=facultadInp,carreraTmp=carreraInp,periodoTmp=periodoInp){
  period_listaR <- BDDsae_spk %>% 
    dplyr::filter(facultadMateria==facultadTmp & carrera==carreraTmp) %>%
    select(periodo) %>% distinct() %>% #dplyr::filter(!endsWith(periodo,"R")) %>% 
    arrange(periodo)
  period_listaR <- period_listaR[!endsWith(period_listaR$periodo,"R"),]
  n<-max(4, which(period_listaR$periodo==periodoTmp))
  lista_n <- rev(2:n)
  return(lista_n)
}
