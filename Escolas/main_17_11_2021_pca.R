##############################################################
# AULA 17-11-2021 - PCA - 
# Mauricio A. Almeida 2021101420 - d2021101420@unifei.edu.br 
# PCO207 - Prof. Carlos Silveira
##############################################################

library(tidyverse)
library(doMC)
library(stringr)
library(microbenchmark)
library(readxl)
library(FactoMineR)
library(factoextra)

source("fun.R")


path = "Data/"
input_file = "input.csv"

if(0){
  
  print("#####  DATA INPUT #####")
  ttime1 = proc.time()[3]
  
  file_names = paste0(path, prefix, sep, year, sufix)
  list_names = paste0(prefix, sep, year)
  #### CHAMADA DE FUNÇÃO ###
  base = list()
  base$input = read_delim(paste0(path,input_file), delim = ",")
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}

if(1){
  
  print("#####  PCA #####")
  ttime1 = proc.time()[3]
  
  file_names = paste0(path, prefix, sep, year, sufix)
  list_names = paste0(prefix, sep, year)
  #### CHAMADA DE FUNÇÃO ###
  
  base$pcainput = PCA(as.matrix(base$input[1:13]), ncp = 14, graph = F)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}

# Solucao do Prof. Caveira - 10/11
if(0){
  print("##### Comparar colunas  #####")
  ttime1 = proc.time()[3]
  
 
  
  #### CHAMADA DE FUNÇÃO ###
  #base$colunas = data_extract_column(base$escolas)
  base$colunas = data_extract_column2(base$escolas)
  #write_csv(base$colunas, "base_colunas.csv")
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}

# Benchmark entre as implementacoes
if(0){
  
    bm = microbenchmark(
      data_extract_column(base$escolas),
      data_extract_column2(base$escolas),
      times = 100
    )
    print(bm)
}

if(0){
  # Exercicio do dia 03/11 -Solucao dada por Mauricio
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


if(0){
  
  print("##### CARREGA IDEB #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUNÇÃO ###
  # Retirar a seguinte ao carregar as Escolas antes
  base = list()
  file_name = paste0(path, IDEB_file)
  
  base$ideb = ideb_input(file_name)
    
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}

if(0){
  
  print("##### INTERPOLA IDEB #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUNÇÃO ###
  base$ideb_inter = set_ideb_interpolation(base$ideb)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}


if (0){
  
  # Exercicio para 17/11
  base$ideb <- make_vl_cols(base$ideb) 
  view(base$ideb)
  #dat2 <- base$ideb[order(base$ideb$VL_OBSERVADO_2005,base$ideb$VL_OBSERVADO_2007),]
  #dat2$V_2006 <- unlist(by(dat2, dat2$VL_OBSERVADO_2007, function(df) 
  #  approx(df$VL_OBSERVADO_2007, df$VL_OBSERVADO_2009, xout = df$VL_OBSERVADO_2007)[2]))
  #make_vl_cols(base$ideb)
}
