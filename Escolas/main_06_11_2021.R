##############################################################
# AULA 03-11-2021 - DADOS DO INEP - 
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

if(0){
  # Exercicio do dia 
  # Pega o primeiro arquivo como base
  base_cols = colnames(base[[1]])

  #junta todos os nomes de todos os arquivos
  for (arq in base) {
    aux = colnames(arq)
    #arq_cols = as_tibble(t(aux))
    base_cols <- union(base_cols,aux)
  }
  # Cria a tibble com todas as colunas (1a linha)
  tb_cols = as_tibble(t(base_cols))
  # Preenche os nomes das colunas
  colnames(tb_cols) = t(base_cols)
  #junta todos os nomes de todos os arquivos
  for (arq in base) {
    aux = colnames(arq)
    arq_cols = as_tibble(t(aux))
    # Preenche os nomes das colunas
    colnames(arq_cols) = t(aux)
    tb_cols <- union_all(tb_cols,arq_cols)
  }
  # Remove a primeira linha (contem todas as colunas)
  tb_cols = tb_cols[-1,]
  #Transforma linhas em colunas
 tb_cols = as_tibble(t(tb_cols))
 # Preenche os nomes das colunas
 colnames(tb_cols) = names(base)
 #Mostra o resultado
 view(tb_cols)
  
}
