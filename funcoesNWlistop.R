###FUNCAO QUE AJEITA O SIST
ajeitar_sist <- function(sist,per)
{
  
  for(j in 1:(13-per)){
    
    sist[1,(j+per-1)] <- sist[1,j]
    
    sist[1,j] = 0
    
    sist[6,(j+per-1)] <- sist[6,j]
    
    sist[6,j] = 0
    
  }
  
  return(sist)
}

####FUNCAO QUE DESCOBRE O MES INICIAL DO ANO 01
calc_tempo <- function(matriz_1,matriz_2,matriz_3,matriz_4)
{
  
  soma = 0
  
  soma1 = 0
  
  soma2 = 0
  
  soma3 = 0
  
  soma4 = 0
  
  tempo = 0
  
  for(j in 1:ncol(matriz_1)){
    
    for(i in 1:nrow(matriz_1)){
      
      soma1 = soma1 + matriz_1[i,j]
      soma2 = soma2 + matriz_2[i,j]
      soma3 = soma3 + matriz_3[i,j]
      soma4 = soma4 + matriz_4[i,j]
      
    }
    
    soma = soma + soma1 + soma2 + soma3 + soma4
    
    if(soma == 0){
      
      tempo = tempo + 1
      
    }
    
  }
  
  return(tempo)
  
}

####FUNCAO QUE LE O ARQUIVO PATAMAR
leitura_patamar <- function(linha_comeco, pat)
{
  
  pat <- pat[linha_comeco:(linha_comeco+14),]               #seleciona as linhas 2 a 16
  
  pat <- as.numeric(as.matrix(pat))                       #transforma em vetor
  
  pat_saida <- matrix(data = NA, nrow = 15, ncol = 13)    #cria um auxiliar
  
  pat_saida <- as.numeric(as.matrix(pat_saida))         #transforma em vetor
  
  aux = 16                                                #numero de colunas mais 01
  i = 1
  
  for (i in 1:195){                                       # colunas x linhas 
    
    if(i == aux){
      
      pat_saida[i-15] = pat[i]                            #coloca o elemento em sua posicao correta
      aux = aux + 3
      
    } else {
      pat_saida[i] = pat[i]
    }
    
  }
  
  pat_aux <- matrix(data = NA, nrow = 15, ncol = 13)
  
  for(j in 1:12){
    for(i in 1:15){
      pat_aux[i,j] = pat_saida[i + (j-1)*15]              #cont(linha) + (cont(col)-1)xnum_linha
    }
  }
  
  pat_saida <- pat_aux[,1:12]                             #excluir a ultima linha que é NA
  
  return(pat_saida)
}

###FUNCAO QUE AJEITA MATRIZES E TRANSFORMA: 8K PARA 6K
OITO_PRA_MEIA <- function(ght)
{
  
  ght_modif <- matrix(data = NA, nrow = 8000, ncol = 15)
  
  aux = 1
  
  #ARMAZENA OS DADOS DO ARQUIVO cmarg.out NUMA MATRIZ
  
  for (i in 1:8000){ 
    
    if(i == aux){
      
      for (j in 1:14) {
        ght_modif[i,j] <-ght[i,j+1]  
      }
      
      aux = aux + 4
      
    } else {
      
      for (j in 1:15){
        ght_modif[i,j] <- ght[i,j]
      }
    }
    
  }
  
  #DESCONSIDERA A COLUNA DOS PATAMARES E DA MEDIA
  
  ght <- ght_modif[1:8000,2:13]
  
  ght <- ght[which(1:nrow(ght) %% 4 != 0),]
  
  return(ght)
}

###FUNCAO QUE AJEITA MATRIZES DE 6K BAGUNCADAS
ajeitar_6k <- function(cmo)
{
  
  cmo_modif <- matrix(data = NA, nrow = 6000, ncol = 15)
  
  aux = 1
  
  #ARMAZENA OS DADOS DO ARQUIVO cmarg.out NUMA MATRIZ
  
  for (i in 1:6000){ 
    
    if(i == aux){
      
      for (j in 1:14) {
        cmo_modif[i,j] <- cmo[i,j+1]  
      }
      
      aux = aux + 3
      
    } else {
      
      for (j in 1:15){
        cmo_modif[i,j] <- cmo[i,j]
      }
    }
    
  }
  
  #DESCONSIDERA A COLUNA DOS PATAMARES E DA MEDIA
  
  cmo <- cmo_modif[1:6000,2:13]
  
  return(cmo)
}

###FUNCAO QUE AJEITA MATRIZES DE 2K BAGUNCADAS

ajeita_2k <- function (earm)
{
  
  earm_f <- matrix(data = NA, nrow = 2000, ncol = 12)
  
  if(ncol(earm) == 15){
    
    for(j in 3:14){
      
      for (i in 1:2000){ 
        
        earm_f[i,j-2] <- earm[i,j]
        
      }
      
    }
    
  } else if(ncol(earm) == 14){
    
    for(j in 2:13){
      
      for (i in 1:2000){ 
        
        earm_f[i,j-1] <- earm[i,j]
        
      }
      
    }
  }
  
  
  return (earm_f)
}

###FUNCAO QUE TRANSFORMA MATRIZES DE 6K EM MATRIZES DE 2K, UTILIZANDO A PONDERACAO DO PATAMAR
MEIA_PRA_DOIS <- function(cmo, ano, pat)
{
  
  sum = 0
  
  p = 1
  i = 1
  j = 1
  
  pld <- array(0, dim = c(2000,12))
  
  
  for (j in 1:12){
    
    for (i in 1:2000){
      
      for (p in 1:3){
        
        #CALCULO DOS PLDS
        sum <- sum+cmo[p+(i-1)*3,j]*pat[p+(ano-1)*3,j]*pat_duracao[p+(ano-1)*3,j]
        
      }
      
      pld[i,j] <- sum
      sum = 0
      
      
    }
    
  }  
  
  return(pld)
}
