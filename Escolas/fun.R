# Arquivos de funçoess

interpolate_as_appropriate = function(year, value){
  
 # browser()  
 # if (11000465 %in% id) browser()
  len = length(na.omit(value))
  if (len > 1){
    res = approx(year, value, xout = year, method = 'linear', rule = 2)$y
  } else if (len == 1) {
    #browser()
    res = approx(year, value, xout = year, method = 'constant', rule = 2)$y
  } else {
    res = 0
  }
  res = round(res,2)
  return(res)
}

set_ideb_interpolation = function(base, 
                                  IDEB_prefix = "VL_OBSERVADO_",
                                  seq_year = seq(2005,2019,1)){
  
  # cruza os Ids de Escola com os VL_Observado (completos de 2005 a 2019)
  aux = crossing( ID_ESCOLA = unique(base$ID_ESCOLA), 
                  col_year = paste0(IDEB_prefix, seq_year))
  ## pega só o que importa agora
  df = base %>% select(ID_ESCOLA, starts_with(IDEB_prefix)) %>%
      # Transforma as colunas em linhas
      pivot_longer(!ID_ESCOLA, names_to = "col_year", values_to = "value") %>%
      # Completa os anos que faltam
      full_join(aux) %>%
      # Ordena 
      arrange(ID_ESCOLA, col_year) %>%
      mutate(year = parse_number(col_year))  %>%
      group_by(ID_ESCOLA) %>%
      mutate(value_interp = interpolate_as_appropriate(year, value)) %>%
      select(ID_ESCOLA, col_year, value_interp) %>%
      
      ungroup() %>%
               
      # Retorna para as colunas
      pivot_wider(names_from = col_year, values_from = value_interp)
  
  #browser()
  return(df)
}

set_ideb_interpolation2 = function(base, 
         IDEB_prefix = "VL_OBSERVADO_",
         seq_year = seq(2005,2019,1)){
  
  # cruza os Ids de Escola com os VL_Observado (completos de 2005 a 2019)
  # aux = crossing( ID_ESCOLA = unique(base$ID_ESCOLA), 
   #               col_year = paste0(IDEB_prefix, seq_year))
  col_names = paste0(IDEB_prefix,seq_year)
  ## pega sÃ³ o que importa agora
  df = base %>% select(ID_ESCOLA, starts_with(IDEB_prefix)) %>%
    # Transforma as colunas em linhas
    pivot_longer(!ID_ESCOLA, names_to = "col_year", values_to = "value") %>%
    # Completa os anos que faltam
    #full_join(aux) %>%
    separate(col_year, sep="_", into =c("lixo1", "lixo2", "year")) %>%
    mutate(year := as.factor(2005:2016)) %>%
    complete(nesting(ID_ESCOLA, year)) %>%
             #, fill = c(lixo1 = "VL", lixo2 = "OBSERVADO", value = "0")) %>%
    # Ordena 
    arrange(ID_ESCOLA, year) #%>%
    
    # Retorna para as colunas
    #pivot_wider(names_from = col_year, values_from = value)
  
  browser()
  
}

make_vl_cols2 = function (base){
  
  #base <- mutate(base, .after = VL_OBSERVADO_2005, VL_OBSERVADO_2006 = approx(x= VL_OBSERVADO_2005, y= VL_OBSERVADO_2007, xout = VL_OBSERVADO_2005 )) %>%
 base <- mutate(base, .after = VL_OBSERVADO_2005, VL_OBSERVADO_2006 = (VL_OBSERVADO_2005 + VL_OBSERVADO_2007 )/2) %>%
         mutate(base, .after = VL_OBSERVADO_2007, VL_OBSERVADO_2008 = (VL_OBSERVADO_2007 + VL_OBSERVADO_2009 )/2) %>%
         mutate(base, .after = VL_OBSERVADO_2009, VL_OBSERVADO_2010 = (VL_OBSERVADO_2009 + VL_OBSERVADO_2011 )/2) %>%
         mutate(base, .after = VL_OBSERVADO_2011, VL_OBSERVADO_2012 = (VL_OBSERVADO_2011 + VL_OBSERVADO_2013 )/2) %>%
         mutate(base, .after = VL_OBSERVADO_2013, VL_OBSERVADO_2014 = (VL_OBSERVADO_2013 + VL_OBSERVADO_2015 )/2) %>%
         mutate(base, .after = VL_OBSERVADO_2015, VL_OBSERVADO_2016 = (VL_OBSERVADO_2015 + VL_OBSERVADO_2017 )/2) %>%
         mutate(base, .after = VL_OBSERVADO_2017, VL_OBSERVADO_2018 = (VL_OBSERVADO_2017 + VL_OBSERVADO_2019 )/2)  
 view(base)
}


make_vl_cols = function (base){ ## NAO FUNCIONOU
  prefix = "VL_OBSERVADO_"
#           "VL_OBSERVADO_2005"
  year = seq(2005,2019, by = 1)  
  col_names = paste0(prefix,year)
  c = 2
  maxc = length(col_names)
  while ( c < maxc-2 ) {
    ant = col_names[c-1]
    nov = col_names[c]
    post = col_names[c+1]
    #print(post)
    #data_ant = select(base, {{ant}})
    #data_post = select(base, {{post}})
    base <- base %>% mutate(.after = {{ant}}, {{nov}} := (.data[[ant]] + .data[[post]]) /2 )
   #browser()
    #base2 <- mutate(base, .after = {{ant}}, 
    #                {{nov}} :=  rowMeans({ant}, {post})
    #                )
    #view(base2)
    #break
    c = c + 2
    break
    print (c)
  }
  return(base)
  #for (co in col_names){
  
  #  print(co)
  #}
  
}
make_tab_cols = function (base, tab_name) {
    
  #browser()
  col_names = colnames(base)
  tab_cols = tibble(ID = col_names,
                      {{tab_name}} := col_names)
  #tab_cols = tab_cols %>% full_join(tab_cols_i)
  
  
  return(tab_cols)
}

## funcao para extrair os nomes de colunas
data_extract_column2 = function (base) {
  
  #browser()
  tab_names = names(base)
  aux = base %>% map2(tab_names, make_tab_cols) 
  #browser()
  aux = aux %>% reduce(full_join, by = "ID")
  return(aux)
}


## funcao para extrair os nomes de colunas
data_extract_column = function (base) {
  
  
  tab_names = names(base)
  tab_name = tab_names[1]
  col_names = colnames(base[[1]])
  tab_cols = tibble(ID = col_names,
                    {{tab_name}} := col_names)
  
  #browser()
  len = length(base)
  if(len > 1){ # para evitar que execute descrescente o for 2:1...
    for (i in 2:len){
      tab_name = tab_names[i]
      col_names = colnames(base[[i]])
      tab_cols_i = tibble(ID = col_names,
                        {{tab_name}} := col_names)
      tab_cols = tab_cols %>% full_join(tab_cols_i)
    }
  }

  return(tab_cols)
}



## funcao de carregamento de arquivos 
data_input = function(base, sep = "|"){
  
  aux = read_delim(base, delim = sep)
  return(aux)
  
}



## funcao de carregamento de arquivos em paralelo
data_input_par = function(base, names_base, ncores = 0){
  
  len = length(base)
  aux = list()  
  if ((ncores) & (len>1)){
    print(paste("Doing it parallel with", ncores, "cores"))
    registerDoMC(ncores)
    aux = foreach(i = 1:len) %dopar% {
      data_input(base[i])
    }
  } else {
    print(paste("Doing it sequential..."))
    for (i in 1:len){
      aux[[i]] = data_input(base[i])
    }
  }
  names(aux) = names_base
  
  return(aux)
  
}


## funcao para transformar os nomes das colunas em tabela
Transforma_colunas = function(base){
  
  #names_base = names(base)
  names_base = names(base)
  #browser()
  len = length(base)
  aux = list()  
  if (len>1){
    
    for (i in 1:len) {
      colnam = colnames(base[i])
      aux <- as.data.frame(colnam)
      data_input(base[i])
      names(columns[[i]]) <- c(toString(names_base[[i]]))
      
    }
  }  
  
  return(aux)
  
}



ideb_input = function(file_names){
  
  ideb_tab = read_excel(file_name, skip = 9) %>% # 9 primeiras linhas são cabeçalho
    slice(1:(n()-3)) %>% # 3 últimas linhas são rodapé
    select(1:6,starts_with("VL_OBSERVADO")) %>%
    mutate(across(starts_with("VL"), ~ str_remove(.x, "^-$"))) %>%
    mutate(across(starts_with("VL"), as.double))
  return(ideb_tab)
  
}

