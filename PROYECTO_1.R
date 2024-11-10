# Cargar librerías
library(readxl)
library(dplyr)
library(purrr)
library(arules)
# Ruta base
ruta_base <- normalizePath("C:/Users/hector.merida.gt/OneDrive - Grupo Diveco/Documentos/ANALASIS_DE_DATOS1/MINERIA 1 VERSION 2/DATA SET PROYECTO 1", winslash = "/")

# Columnas estándar para garantizar la uniformidad
columnas_estandar <- c("año", "tipo", "entidad", "programa", "monto", "modificado", "compromiso", "asignado", "departamento", "organismo", "fuente")



cargar_y_limpiar <- function(ruta, tipo, ano) {
  # Verificar si el archivo es accesible
  if (!file.exists(ruta)) {
    warning(paste("El archivo no existe en la ruta especificada:", ruta))
    return(NULL)
  }
  
  # Intentar cargar el archivo y capturar errores
  tryCatch({
    datos <- read_excel(ruta)
    
    # Convertir nombres de columnas a minúsculas
    colnames(datos) <- tolower(colnames(datos))
    
    # Homologar la columna de 'monto'
    if (tipo == "egresos-descentralizadas" || tipo == "egresos-gobierno-central") {
      if ("devengado" %in% colnames(datos)) {
        datos <- datos %>% rename(monto = devengado)
      } else {
        warning(paste("No se encontró la columna 'Devengado' en el archivo:", ruta))
      }
    } else if (tipo == "ingresos-descentralizadas" || tipo == "ingresos-gobierno-central") {
      if ("percibido" %in% colnames(datos)) {
        datos <- datos %>% rename(monto = percibido)
      } else {
        warning(paste("No se encontró la columna 'Percibido' en el archivo:", ruta))
      }
    }
    
    # Homologar columnas de entidad y programa
    if ("entidad_publica" %in% colnames(datos)) {
      datos <- datos %>% rename(entidad = entidad_publica)
    } else if ("institucion" %in% colnames(datos)) {
      datos <- datos %>% rename(entidad = institucion)
    }
    
    if ("nombre_programa" %in% colnames(datos)) {
      datos <- datos %>% rename(programa = nombre_programa)
    }
    
    # Convertir 'entidad', 'programa' y 'fuente' a character para evitar conflictos de tipo
    if ("entidad" %in% colnames(datos)) {
      datos <- datos %>% mutate(entidad = as.character(entidad))
    }
    
    if ("programa" %in% colnames(datos)) {
      datos <- datos %>% mutate(programa = as.character(programa))
    }
    
    if ("fuente" %in% colnames(datos)) {
      datos <- datos %>% mutate(fuente = as.character(fuente))
    }
    
    # Agregar nuevas columnas con valores predeterminados
    datos <- datos %>%
      mutate(
        año = ano,
        tipo = tipo,
        
        # Fuente del ingreso/egreso
        fuente_ingreso_egreso = case_when(
          tipo == "ingresos-descentralizadas" | tipo == "ingresos-gobierno-central" ~ fuente,
          tipo == "egresos-descentralizadas" | tipo == "egresos-gobierno-central" ~ "Presupuesto Interno",
          TRUE ~ "NE"
        ),
        
        # Clasificación del gasto
        clasificacion_gasto = case_when(
          tipo == "egresos-descentralizadas" | tipo == "egresos-gobierno-central" ~ 
            case_when(
              grepl("personal|funcionamiento", programa, ignore.case = TRUE) ~ "Operativo",
              grepl("inversion", programa, ignore.case = TRUE) ~ "Inversión",
              grepl("transferencia", programa, ignore.case = TRUE) ~ "Transferencias",
              TRUE ~ "NE"
            ),
          tipo == "ingresos-descentralizadas" | tipo == "ingresos-gobierno-central" ~ "NA",
          TRUE ~ "NE"
        ),
        
        # Categoría de inversión (solo para gastos de inversión)
        categoria_inversion = case_when(
          clasificacion_gasto == "Inversión" ~ 
            case_when(
              grepl("infraestructura", programa, ignore.case = TRUE) ~ "Infraestructura",
              grepl("educacion", programa, ignore.case = TRUE) ~ "Educación",
              grepl("salud", programa, ignore.case = TRUE) ~ "Salud",
              TRUE ~ "NE"
            ),
          TRUE ~ "NA"
        )
      ) %>%
      select(any_of(columnas_estandar), fuente_ingreso_egreso, clasificacion_gasto, categoria_inversion)  # Seleccionar columnas estándar y nuevas columnas
    
    return(datos)
    
  }, error = function(e) {
    # Capturar el error y mostrar un mensaje
    warning(paste("No se pudo cargar el archivo:", ruta, "\nError:", e$message))
    return(NULL)  # Retornar NULL si hay un error al cargar
  })
}





# Lista completa de archivos desde 2016 a 2023
archivos <- list(
  # Archivos de 2016
  list(ruta = file.path(ruta_base, "2016", "presupuesto-egresos-2016-descentralizadas.xlsx"), tipo = "egresos-descentralizadas", ano = 2016),
  list(ruta = file.path(ruta_base, "2016", "presupuesto-egresos-2016-gobierno-central.xlsx"), tipo = "egresos-gobierno-central", ano = 2016),
  list(ruta = file.path(ruta_base, "2016", "presupuesto-ingresos-2016-descentralizadas.xlsx"), tipo = "ingresos-descentralizadas", ano = 2016),
  list(ruta = file.path(ruta_base, "2016", "presupuesto-ingresos-2016-gobierno-central.xlsx"), tipo = "ingresos-gobierno-central", ano = 2016),
  
  # Archivos de 2017
  list(ruta = file.path(ruta_base, "2017", "presupuesto-egresos-2017-descentralizadas.xlsx"), tipo = "egresos-descentralizadas", ano = 2017),
  list(ruta = file.path(ruta_base, "2017", "presupuesto-egresos-2017-gobierno-central.xlsx"), tipo = "egresos-gobierno-central", ano = 2017),
  list(ruta = file.path(ruta_base, "2017", "presupuesto-ingresos-2017-descentralizadas.xlsx"), tipo = "ingresos-descentralizadas", ano = 2017),
  list(ruta = file.path(ruta_base, "2017", "presupuesto-ingresos-2017-gobierno-central.xlsx"), tipo = "ingresos-gobierno-central", ano = 2017),
  
  # Archivos de 2018
  list(ruta = file.path(ruta_base, "2018", "presupuesto-egresos-2018-descentralizadas.xlsx"), tipo = "egresos-descentralizadas", ano = 2018),
  list(ruta = file.path(ruta_base, "2018", "presupuesto-egresos-2018-gobierno-central.xlsx"), tipo = "egresos-gobierno-central", ano = 2018),
  list(ruta = file.path(ruta_base, "2018", "presupuesto-ingresos-2018-descentralizadas.xlsx"), tipo = "ingresos-descentralizadas", ano = 2018),
  list(ruta = file.path(ruta_base, "2018", "presupuesto-ingresos-2018-gobierno-central.xlsx"), tipo = "ingresos-gobierno-central", ano = 2018),
  
  # Archivos de 2019
  list(ruta = file.path(ruta_base, "2019", "presupuesto-egresos-2019-descentralizadas.xlsx"), tipo = "egresos-descentralizadas", ano = 2019),
  list(ruta = file.path(ruta_base, "2019", "presupuesto-egresos-2019-gobierno-central.xlsx"), tipo = "egresos-gobierno-central", ano = 2019),
  list(ruta = file.path(ruta_base, "2019", "presupuesto-ingresos-2019-descentralizadas.xlsx"), tipo = "ingresos-descentralizadas", ano = 2019),
  list(ruta = file.path(ruta_base, "2019", "presupuesto-ingresos-2019-gobierno-central.xlsx"), tipo = "ingresos-gobierno-central", ano = 2019),
  
  # Archivos de 2020
  list(ruta = file.path(ruta_base, "2020", "presupuesto-egresos-2020-descentralizadas.xlsx"), tipo = "egresos-descentralizadas", ano = 2020),
  list(ruta = file.path(ruta_base, "2020", "presupuesto-egresos-2020-gobierno-central.xlsx"), tipo = "egresos-gobierno-central", ano = 2020),
  list(ruta = file.path(ruta_base, "2020", "presupuesto-ingresos-2020-descentralizadas.xlsx"), tipo = "ingresos-descentralizadas", ano = 2020),
  list(ruta = file.path(ruta_base, "2020", "presupuesto-ingresos-2020-gobierno-central.xlsx"), tipo = "ingresos-gobierno-central", ano = 2020),
  
  # Archivos de 2021
  list(ruta = file.path(ruta_base, "2021", "presupuesto-egresos-2021-descentralizadas.xlsx"), tipo = "egresos-descentralizadas", ano = 2021),
  list(ruta = file.path(ruta_base, "2021", "presupuesto-egresos-2021-gobierno-central.xlsx"), tipo = "egresos-gobierno-central", ano = 2021),
  list(ruta = file.path(ruta_base, "2021", "presupuesto-ingresos-2021-descentralizadas.xlsx"), tipo = "ingresos-descentralizadas", ano = 2021),
  list(ruta = file.path(ruta_base, "2021", "presupuesto-ingresos-2021-gobierno-central.xlsx"), tipo = "ingresos-gobierno-central", ano = 2021),
  
  # Archivos de 2022
  list(ruta = file.path(ruta_base, "2022", "presupuesto-egresos-2022-descentralizadas.xlsx"), tipo = "egresos-descentralizadas", ano = 2022),
  list(ruta = file.path(ruta_base, "2022", "presupuesto-egresos-2022-gobierno-central.xlsx"), tipo = "egresos-gobierno-central", ano = 2022),
  list(ruta = file.path(ruta_base, "2022", "presupuesto-ingresos-2022-descentralizadas.xlsx"), tipo = "ingresos-descentralizadas", ano = 2022),
  list(ruta = file.path(ruta_base, "2022", "presupuesto-ingresos-2022-gobierno-central.xlsx"), tipo = "ingresos-gobierno-central", ano = 2022),
  
  # Archivos de 2023
  list(ruta = file.path(ruta_base, "2023", "presupuesto-egresos-2023-descentralizadas.xlsx"), tipo = "egresos-descentralizadas", ano = 2023),
  list(ruta = file.path(ruta_base, "2023", "presupuesto-egresos-2023-gobierno-central.xlsx"), tipo = "egresos-gobierno-central", ano = 2023),
  list(ruta = file.path(ruta_base, "2023", "presupuesto-ingresos-2023-descentralizadas.xlsx"), tipo = "ingresos-descentralizadas", ano = 2023),
  list(ruta = file.path(ruta_base, "2023", "presupuesto-ingresos-2023-gobierno-central.xlsx"), tipo = "ingresos-gobierno-central", ano = 2023)
)


# Cargar y consolidar todos los archivos
datos_consolidados <- archivos %>%
  purrr::map_df(~cargar_y_limpiar(.x$ruta, .x$tipo, .x$ano), .id = "source")

# Verificar la estructura final
glimpse(datos_consolidados)





# Guardar en formato CSV
write.csv(datos_consolidados, "datos_consolidados.csv", row.names = FALSE)

# Guardar en formato RDS (más eficiente y preserva las estructuras de datos de R)
saveRDS(datos_consolidados, "datos_consolidados.rds")



# Cargar el archivo RDS
datos_consolidados <- readRDS("datos_consolidados.rds")


##LIMPIEZA DE DATOS

# Filtrar filas sin NA en las columnas críticas (ajusta según el análisis)
##datos_limpios <- datos_consolidados %>%
##  filter(!is.na(monto), !is.na(entidad), !is.na(clasificacion_gasto),!is.NA(clasificacion_gasto))

# Seleccionar columnas específicas si es necesario
##datos_limpios <- datos_limpios %>%
  ##select(año, tipo, entidad, programa, monto, clasificacion_gasto, categoria_inversion)

# Verificar la estructura final
#glimpse(datos_limpios)



##APRIORI
 

# Cargar las librerías necesarias
library(arules)
library(dplyr)

# Seleccionar las columnas relevantes y convertirlas en factores
datos_apriori <- datos_consolidados %>%
  select(departamento, clasificacion_gasto, categoria_inversion, tipo, año) %>%
  mutate_all(as.factor)

# Crear una matriz binaria para cada combinación de factores
datos_binarios <- as(datos_apriori, "transactions")

# Verificar el resumen para asegurar que todo está correcto
summary(datos_binarios)



# Configurar y ejecutar el algoritmo Apriori con soporte y confianza mínimos
reglas_apriori <- apriori(datos_binarios, parameter = list(supp = 0.01, conf = 0.8))

# Ordenar las reglas por confianza y seleccionar las 10 más fuertes
reglas_interesantes <- sort(reglas_apriori, by = "confidence", decreasing = TRUE)[1:10]

# Inspeccionar las reglas interesantes
inspect(reglas_interesantes)





 
