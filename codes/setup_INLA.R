system("clang --version") # comprobar ambas
system("gfortran --version") # comprobar ambas

install.packages("INLA",
                 repos = c(getOption("repos"),
                           INLA = "https://inla.r-inla-download.org/R/stable"),
                 dep = TRUE)


library(INLA)
fit <- inla(y ~ 1, data = data.frame(y = rnorm(100)))
summary(fit)

# Verifica instalación de INLA
inla.version()
inla.list.models() %>% head(10)

# Carga inlabru
library(inlabru)

instalar_faltantes <- function(pkgs) {
  faltantes <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if (length(faltantes) == 0) {
    message("Todos los paquetes ya están instalados.")
  } else {
    message("Instalando paquetes faltantes: ", paste(faltantes, collapse = ", "))
    install.packages(faltantes)
  }
}

mis_paquetes <- c(
  "CARBayesdata",
  "DAAG",
  "dplyr",
  "FSAdata",
  "ggplot2",
  "gt",
  "lubridate",
  "magrittr",
  "mapview",
  "patchwork",
  "scico",
  "sdmTMB",
  "sf",
  "spatstat",
  "spdep",
  "terra",
  "tidyr",
  "tidyterra",
  "tidyverse"
)

instalar_faltantes(mis_paquetes)
# llamar librarias
library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(terra)
library(tidyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(patchwork)
library(mapview)
library(scico)
library(gt)
library(CARBayesdata)
library(DAAG)
library(FSAdata)
library(sdmTMB)
library(spatstat)
library(tidyterra)
library(INLA)
library(inlabru)
library(spatstat)
library(fmesher)
