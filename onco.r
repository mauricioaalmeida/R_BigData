


library(tidyverse)
library(doMC)
library(stringr)
library(microbenchmark)
library(readxl)
library(FactoMineR)
library(factoextra)
library(corrplot)

if (1) { 
  
  print("##### DATA INPUT #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUN«√O ###
  # 1.Nunca; 2.Ex-consumidor; 3.Sim; 4.N√£o avaliado; 8.N√£o se aplica; 9.Sem informa√ß√£o
 
  #Principais Colunas
  base = list()
  col_names = c("Sexo","idade","CorPele", "HistFamiliar","HistBebida","HistTabaco",
                "CodTumor","TumorPrim")
  
  base$onco <- read_csv("../../Onco1.csv") %>%
  #Mudar nome de colunas  
  rename(idade = Idade1Consulta) %>%
  # Dividir c√≥digos e dados  
  separate(CorPele, into = c("CodCorPele","CorPele")) %>%
  separate(Sexo, into = c("CodSexo","Sexo")) %>%
  separate(HistFamiliar, into = c("CodHistFamiliar","HistFamiliar")) %>%
  # Limpar dados incorretos  
  mutate(HistFamiliar = stringr::str_replace(HistFamiliar, "Casado","N")) %>%
  mutate(HistFamiliar = replace(HistFamiliar, HistFamiliar == "N","N√O")) %>%
  separate(HistTabaco, into = c("CodHistTabaco","HistTabaco")) %>%
  mutate(HistTabaco = replace(HistTabaco, HistTabaco == "N","Ex")) %>% 
  separate(HistBebida, into = c("CodHistBebida","HistBebida")) %>%   
  mutate(HistBebida = replace(HistBebida, HistBebida == "N","Ex")) %>%    
  separate(LocalTumorPrim, into = c("CodTumor","TumorPrim","TumorSuffix"), sep = "[-,]") %>%
  mutate(TumorPrim = stringr::str_replace(TumorPrim, "[.]","")) %>%
  # Trazer as colunas principais pra frente
  select({{col_names}}, everything())
  #view(base$onco)
  ## Criar De-Para de Tipos de Tumor para agrupar
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}

if (0){    
  ## Antes retirar espa√ßos e converter para CAIZA ALTA
  Tumor <- base$onco %>% count(TumorPrim) %>% arrange(desc(n)) %>%
    mutate(TumorPrimNovo = word(TumorPrim,-1,-1)) %>%
  mutate(TumorPrimNovo = replace(TumorPrimNovo, TumorPrim == "Medula √≥ssea","Medula"))
  ### Falta limpar alguns dados e aplicar √† tabela original
  view(Tumor)
   }

if (0){
  p1 = ggplot(data = onco) +
    geom_bar( mapping = aes(x = HistTabaco, fill = Sexo )) + 
    scale_fill_manual(values = c("pink","cyan"))
  p2 =  ggplot(data = onco) +
    geom_bar( mapping = aes(x = HistBebida, fill = Sexo )) +
    scale_fill_manual(values = c("pink","cyan"))
  p3=  ggplot(data = onco) +
    geom_bar( mapping = aes(x = HistFamiliar, fill = Sexo )) +
    scale_fill_manual(values = c("pink","cyan"))
  p4=  ggplot(data = onco) +
    geom_bar( mapping = aes(x = CorPele, fill = Sexo )) +
    scale_fill_manual(values = c("pink","cyan"))
  p5=  ggplot(data = onco) +
    geom_bar( mapping = aes(x = idade, fill = Sexo )) +
    scale_fill_manual(values = c("pink","cyan"))
  p6=  ggplot(data = onco) +
   geom_bar( mapping = aes(x = TumorPrim, fill = Sexo )) +
   scale_fill_manual(values = c("pink","cyan"))
 
  Tumor20 <- onco %>% count(TumorPrim) %>% arrange(desc(n))%>%
    slice(1:20) 
  idx <- order(Tumor20$TumorPrim, decreasing = TRUE)
  levels <- Tumor20$TumorPrim[idx]#%+%Tumor$Sexo[idx]
  Tumor20$TumorPrim <- factor(Tumor20$TumorPrim, levels=levels, ordered = TRUE) #%>%
  p7 = ggplot(Tumor20, aes(x = n, y= TumorPrim, fill = TumorPrim)) +
    geom_bar(stat="identity")+
               guides(fill=FALSE)  #, fill = Sexo )) +
    #scale_fill_manual(values = c("pink","cyan"))
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
  print(p6)
  print(p7)
}

if (0){
  p = ggplot(data = onco) +
    geom_point( mapping = aes(x = HistTabaco, y = HistBebida, 
                              size = Idade1Consulta ) )
  #+
#    geom_smooth(data = filter(onco, class == as.factor("Idade1Consulta") ))
  print(p)
}

if (1){
  
  print("##### Preparacao dados principais #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUN«√O ###
  base$oncoPCA <- base$onco %>% select(
    Sexo, idade, CorPele, HistFamiliar, HistBebida, HistTabaco,
    CodTumor, TumorPrim, Escolaridade,  Ocupacao, TipoHistol, Histologia,
    Estadiamento, Estadofinal, DataObito, EstadoConjugal, CausaMorte )
  
  base$oncoPCA <- base$oncoPCA %>% 
    mutate(across(.cols = everything(),as.factor)) %>% 
    mutate(across(.cols = everything(),as.numeric)) %>% replace(is.na(.), 0)
  
  view(base$oncoPCA)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
}

if(1){
  
  print("##### PCA #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUN«√O ###
  base$pca = PCA(as.matrix(base$oncoPCA), ncp = 17, graph = F)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}


if(1){
  
  print("##### PCA GRAPHICS #####")
  ttime1 = proc.time()[3]
  
  #### CHAMADA DE FUN«√O ###
  
  att = "CausaMorte"
  p0 <- fviz_pca_ind(base$pca, 
                     geom.ind = "point",
                     axes = c(1,2),
                     col.ind = as.numeric(as.factor(base$onco[[att]])), 
                     gradient.cols = c("#FC4E07", "#E7B800", "#00AFBB") #,
                     #repel = TRUE # Avoid text overlapping (slow if many points)
  )
  p0 = p0 + labs(color = att)
  print(p0)
  
  att = "TumorPrim"
  p01 <- fviz_pca_ind(base$pca, 
                     geom.ind = "point",
                     axes = c(1,2),
                     col.ind = as.numeric(as.factor(base$onco[[att]])), 
                     gradient.cols = c("#FC4E07", "#E7B800", "#00AFBB") #,
                     #repel = TRUE # Avoid text overlapping (slow if many points)
  )
  p01 = p01 + labs(color = att)
  print(p01)
  
  att = "Sexo"
  p02 <- fviz_pca_ind(base$pca, 
                      geom.ind = "point",
                      axes = c(1,2),
                      col.ind = as.numeric(as.factor(base$onco[[att]])), 
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

  #att = "Estadofinal"
  #p = fviz_pca_ind(base$pca, 
  #                 geom.ind = "point",
  #                 axes = c(1,2),
  #                 col.ind = as.numeric(as.factor(base$input[[att]])), 
  #                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07") #,
                   #repel = TRUE # Avoid text overlapping (slow if many points)
  #)
  #p = p + labs(color = att)
  #print(p)
  
  ttime2 = proc.time()[3]
  print(paste("Time:",round((ttime2-ttime1), 1),"secs"))
  print(paste("Size of base:",format(object.size(base), units="Mb")))
  
}

if (0) {
  p8 = fviz_pca_biplot(base$pca,axes = c(1,2),
                      col.ind = as.numeric(as.factor(base$onco$Sexo)), 
                       gradient.cols = c("#FC4E07", "#E7B800", "#00AFBB"))
  print(p8)
}
