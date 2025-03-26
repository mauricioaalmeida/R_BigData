##############################################################
# AULA 27-10-2021 - DADOS DO INEP - 
# Mauricio A. Almeida 2021101420 - d2021101420@unifei.edu.br 
# PCO207 - Prof. Carlos Silveira
##############################################################

library(tidyverse)
library(doMC)
library(stringr)

source("fun.R")

prefix = "ESCOLAS"
year = seq(2007,2014, by = 1)
suffix = ".CSV"
sep = "_"
path = "Data/"

if(1){
  
  print("##### SCHOOL DATA INPUT #####")
  ttime1 = proc.time()[3]
  
  file_names = paste0(path, prefix, sep, year, suffix )
  #list_names = paste0(prefix, sep, year)
  
  #### CHAMADA DE FUNÇÃO ###
  base = data_input_par(file_names, ncores = 2)

  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}

