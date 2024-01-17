library(readstata13)
library(tidyverse)
library(pdftools)
library(data.table)
library(dplyr)
library(readxl)
library(foreign)
library(stringi)
library(SoundexBR) #soundex
library(janitor)
library(lubridate)
library(writexl)
devtools::load_all(".")

namestand<- vitallinkage::namestand

sinan <- read.dbf('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/SINAN/VIOLENET.DBF')


sinan <- sinan |>
  vitallinkage::drop_duplicados_sinan_1() |>  #Dropa as colunas duplicadas inicialmente
  vitallinkage::criar_coluna_id("SINAN")|> # Criando colunas com ID "SINAN_1 ... "
  vitallinkage::padroniza_variaveis("SINAN")
