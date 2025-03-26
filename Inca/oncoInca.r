#Processamento com a base do INCA de 2000 a 2019

library(tidyverse)
library(doMC)
library(stringr)
library(microbenchmark)
library(readxl)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(corrplot)
library(foreign)

path = "./"
prefix = "rhc"
seq1 = seq(0,19, by = 1)
seq2 = seq(88,99, by = 1)

sufix = ".dbf"



if (0) { 
  print("##### SISRHC DATA INPUT #####")
  ttime1 = proc.time()[3]
  
  base = list()
  file_names = paste0(path, prefix, 
                      formatC(seq1, width = 2, format = "d", flag = "0"), 
                      sufix)
  
  len = length(file_names)
  for (i in 1:len){
    fname = file_names[i]
    print(fname)
    base$input <- read.dbf(fname) 
  }
  #
  view(base$input)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}



if (0){
  
  print("##### Preparacao dados principais #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUN??O ###
  base$oncoPCA <- base$input %>% select(
    TPCASO, SEXO, IDADE, LOCALNAS, RACACOR, INSTRUC, 
    HISTFAMC, ALCOOLIS, TABAGISM, OCUPACAO, 
    LOCTUDET, ESTADIAM, TNM, DATAOBITO )
  
  base$oncoPCA <- base$oncoPCA %>% 
    mutate(across(.cols = everything(),as.factor)) %>% 
    mutate(across(.cols = everything(),as.numeric)) %>% replace(is.na(.), 0)
  
  view(base$oncoPCA)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
}

if(0){
  
  print("##### PCA #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUN??O ###
  base$pca = PCA(as.matrix(base$oncoPCA), ncp = 11, graph = F)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}


if(0){
  
  print("##### PCA GRAPHICS #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUN??O ###
  
  att = "DATAOBITO"
  p0 <- fviz_pca_ind(base$pca, 
                     geom.ind = "point",
                     axes = c(1,2),
                     col.ind = as.numeric(as.factor(base$input[[att]])), 
                     gradient.cols = c("#FC4E07", "#E7B800", "#00AFBB") #,
                     #repel = TRUE # Avoid text overlapping (slow if many points)
  )
  p0 = p0 + labs(color = att)
  print(p0)
  
  att = "LOCALNAS"
  p01 <- fviz_pca_ind(base$pca, 
                      geom.ind = "point",
                      axes = c(1,2),
                      col.ind = as.numeric(as.factor(base$input[[att]])), 
                      gradient.cols = c("#FC4E07", "#E7B800", "#00AFBB") #,
                      #repel = TRUE # Avoid text overlapping (slow if many points)
  )
  p01 = p01 + labs(color = att)
  print(p01)
  
  att = "SEXO"
  p02 <- fviz_pca_ind(base$pca, 
                      geom.ind = "point",
                      axes = c(1,2),
                      col.ind = as.numeric(as.factor(base$input[[att]])), 
                      gradient.cols = c("#FC4E07", "#E7B800", "#00AFBB") #,
                      #repel = TRUE # Avoid text overlapping (slow if many points)
  )
  p02 = p02 + labs(color = att)
  print(p02)
  
  p1 = fviz_eig(base$pca, addlabels = TRUE, ylim = c(0, 50))
  
  p2 = fviz_pca_var(base$pca, col.var = "black", repel=TRUE, 
                    axes = c(1,2) )
  
  p3 = fviz_pca_var(base$pca, col.var = "black", repel=TRUE,
                    select.var = list(contrib = 7),
                    axes = c(1,2) )
  p4 = fviz_pca_var(base$pca, col.var = "black", repel=TRUE, 
                    select.var = list(contrib = 7),
                    axes = c(1,3))
  p5 = fviz_pca_var(base$pca, col.var = "black", repel=TRUE,
                    select.var = list(contrib = 7),
                    axes = c(1,4))
  #p5 = fviz_pca_var(base$pca, col.var = "black", repel=TRUE, axes = c(1,5))
  var <- get_pca_var(base$pca)
  #var
  p6 = corrplot(var$cos2, is.corr=FALSE)
  
  p7 = fviz_cos2(base$pca, choice = "var", axes = 1:2)
  
  #p8 = fviz_pca_biplot(base$pca,axes = c(1,2),
  #                    col.ind = as.numeric(as.factor(base$onco$Sexo)), 
  #                     gradient.cols = c("#FC4E07", "#E7B800", "#00AFBB"))
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
  print(p6)
  print(p7)
  #print(p8)
  print(base$pca$eig)
  
  
  
  #fviz_pca_biplot()
  
  att = "ESTADIAM"
  p8 = fviz_pca_biplot(base$pca, 
                       geom.ind = "point",
                       axes = c(2,3),
                       col.ind = as.numeric(as.factor(base$input[[att]])), 
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE # Avoid text overlapping (slow if many points)
  )
  p8 = p8 + labs(color = att)
  print(p8)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}

if(0){
  
  print("##### PCA GRAPHICS 2 #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUN??O ###
  att = "ESTADIAM"
  p9 = fviz_pca_biplot(base$pca, 
                       geom.ind = "point",
                       axes = c(1,2),
                       col.ind = as.numeric(as.factor(base$input[[att]])), 
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE # Avoid text overlapping (slow if many points)
  )
  p9 = p9 + labs(color = att)
  print(p9)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}


if (1) {
  print("##### PCA GRAPHICS 3 #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUN??O ###
  
  att = "RACACOR"
  p10 = fviz_pca_biplot(base$pca,axes = c(1,2),
                       col.ind = as.numeric(as.factor(base$input[[att]])), 
                       gradient.cols = c("#FC4E07", "#E7B800", "#00AFBB"))
  p10 = p10 + labs(color = att)
  print(p10)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}
