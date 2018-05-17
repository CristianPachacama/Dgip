#===========================================================
#!!!!!!!!!!!!!!!!    Conexión a BDD    !!!!!!!!!!!!!!!!!!!!
#===========================================================
library("RJDBC")
load("Data/NombreVariables.RData")

#Conexión a BDD ---------------------------------
options(java.parameters = "-Xmx4024m")
memory.limit(size=10000000000024)

drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver" , "/home/operador/sqljdbc42.jar" ,identifier.quote="`")
# Reemplazar IP y puerto(1433):
conn <- dbConnect(drv, "jdbc:sqlserver:// < IP > :1433;databaseName=CopyOfSAE", "sa", "Lcddtp2014")
d <- dbGetQuery(conn, "select  count (*) from vw_saebicalif;")

conn <- dbConnect(drv, "jdbc:sqlserver:// < IP > :1433;databaseName=CopyOfSAE", "sa", "Lcddtp2014")
results <- dbSendQuery(conn,"select * from vw_saebicalif;") 
partialset <- fetch(results, as.numeric(as.numeric(d))) 


# Corrección Nombres ----------------------------



# Generar archivo principal de App --------------
save(BDD_sae_spk,file="Data/Datos_SAE_Act.RData")

