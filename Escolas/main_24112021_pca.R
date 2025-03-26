########################################################
# AULA 24-11-2021 - PCA
########################################################

library(tidyverse)
library(doMC)
library(stringr)
library(microbenchmark)
library(readxl)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(corrplot)

source("fun.R")

path = "Data/"
input_file = "input.csv"

if(0){
  
  print("##### DATA INPUT #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUNÇÃO ###
  base = list()
  base$input = read_delim(paste0(path,input_file), delim = ",")
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}


if(0){
  
  print("##### PCA #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUNÇÃO ###
  base$pca = PCA(as.matrix(base$input[1:13]), ncp = 10, graph = F)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}


if(0){
  
  print("##### PCA GRAPHICS #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUNÇÃO ###
  att = "C14"
  nrow = nrow(base$input)
  var = base$input[["C8"]]
  col.ind = var
  #col.ind = as.numeric(as.factor(base$input[[att]]))
  col3 = colorRampPalette(c('darkred','yellow', 'darkblue'))
  p = fviz_pca_ind(base$pca, 
                   geom.ind = "point",
                   axes = c(2,7),
                   col.ind = col.ind , 
                   pointsize = 4,
                   gradient.cols = col3(nrow)
                   #,
                   #repel = TRUE # Avoid text overlapping (slow if many points)
  )
  p = p + labs(color = att)
  p = p + geom_text_repel(aes(label=var, color=col.ind)) + #, color = var)) +
    ggpubr::gradient_color(col3(nrow))
  print(p)
  #plot(base$pca$eig[,1], type = "b")
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}


if(1){
  p = fviz_pca_var(base$pca, col.var = "black", axes = c(1,2), #,
                   select.var = list(contrib = 7))
  print(p)
  #plot(base$eig[,1], col="blue", type="b", ylim=c(0,100))
}


if(0){
  col = colorRampPalette(c('white', 'steelblue'))
  corrplot(base$pca$var$cos2, is.corr=FALSE,col=col(20),mar=c(0,0,2,0),
           cl.offset = 0.2,
           title="Title...")
  
}

# if(0){
#   x = microbenchmark(
#     data_extract_column(base$escolas),
#     data_extract_column_2(base$escolas),
#     times = 100
#   )
#   print(x)
# }
