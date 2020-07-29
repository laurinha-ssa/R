best <- function(state, outcome) {
  ## abre documento
  dados <- read.csv("outcome-of-care-measures.csv")
  ## cria um factor com todos os estados
  listaestado <- factor(dados$State)
  x=0
  ## contabçiza quantas vezes o estado apareceu
  for(i in levels(listaestado)){
    if(state == i){
      x=x+1
    }
    else{
      x=x+0
    }
  }
  
  ## analisa se o estado existe e parte para verificar os outcomes
  if(x<1){
    stop("state inexistente")  
  }
  if(x>=1){
    linha = 0
    num = 0
    if(outcome == "heart attack"){
      ##cria uma lista com os valores de mortes para heart attack
      listado <- as.numeric(dados[, 15])
      ## transforma NA em 0
      for(n in listado){
        num=num+1
        if (is.na(n)){
          listado[num] <- 0
        }
      }
      ## cria um data frame com nome do hospital, mortes e estado
      tabela <- data.frame(dados$Hospital.Name , dados$State,listado)
      tabela2 <- tabela[order(tabela[,3], tabela[,1], decreasing=c(FALSE, TRUE)), ]
      for(j in tabela2[,2]){
        linha=linha+1
        if((tabela2[linha,3] >= 1) && (j == state)){
          stop <- TRUE
          nome <- tabela2[linha,1]
          break 
        }
        if (isTRUE(stop)){
          break
        }
        else{
          next
        }
      }
    }
    if(outcome == "heart failure"){
      ##cria uma lista com os valores de mortes para heart failure
      listado <- as.numeric(dados[, 21])
      ## transforma NA em 0
      for(n in listado){
        num=num+1
        if (is.na(n)){
          listado[num] <- 0
        }
      }
      ## cria um data frame com nome do hospital, mortes e estado
      tabela <- data.frame(dados$Hospital.Name , dados$State,listado)
      tabela2 <- tabela[order(tabela[,3], tabela[,1], decreasing=c(FALSE, TRUE)), ]
      for(j in tabela2[,2]){
        linha=linha+1
        if((tabela2[linha,3] >= 1) && (j == state)){
          stop <- TRUE
          nome <- tabela2[linha,1]
          break 
        }
        if (isTRUE(stop)){
          break
        }
        else{
          next
        }
      }
    }
    if(outcome == "pneumonia"){
      ##cria uma lista com os valores de mortes para pneumonia
      listado <- as.numeric(dados[, 27])
      ## transforma NA em 0
      for(n in listado){
        num=num+1
        if (is.na(n)){
          listado[num] <- 0
        }
      }
      ## cria um data frame com nome do hospital, mortes e estado
      tabela <- data.frame(dados$Hospital.Name , dados$State,listado)
      tabela2 <- tabela[order(tabela[,3], tabela[,1], decreasing=c(FALSE, TRUE)), ]
      for(j in tabela2[,2]){
        linha=linha+1
        if((tabela2[linha,3] >= 1) && (j == state)){
          stop <- TRUE
          nome <- tabela2[linha,1]
          break 
        }
        if (isTRUE(stop)){
          break
        }
        else{
          next
        }
      }
      }
    }
    else{
      stop("outcome inexistente")
    }
  nome
  }
