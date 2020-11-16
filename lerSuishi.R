## funcao que transforma valores expressos no tipo caracter em tipo numérico
transf <- function(gf)
{
  aux = as.numeric(gf)
  
  if(is.numeric(aux) && length(aux))
  {
    
    for(i in 1:length(aux))
    {
      if(is.na(aux[i]))
      {
        a8 = strsplit(gf[i],split = "")
        if(is.integer(which(a8[[1]] == ",")) && length(which(a8[[1]] == ",")) == 1)
        {
          
          a8[[1]] = a8[[1]][-which(a8[[1]] == ",")]
          
        }
        if(is.integer(which(a8[[1]] == " ")) && length(which(a8[[1]] == " ")) == 1)
        {
          
          a8[[1]] = a8[[1]][-which(a8[[1]] == " ")]
          
        }
        a10 = paste0(a8[[1]],collapse = "")
        aux[i] = as.numeric(a10)
      }
      
    }
    
  }
  
  # caso nao haja valor algum em tal premio, por o número 0
  if(is.numeric(aux) && length(aux) == 0)
  {
    aux = 0
  }
  
  gf = aux
  
  return(gf)
  
}

virgP = function(aux)
{
  for(i in 1:length(aux))
  {
    a8 = strsplit(aux[i],split = "")
    a8[[1]][which(a8[[1]] == ",")]="."
    aux[i] = paste0(a8[[1]],collapse = "")
  }
  
  return(aux)
  
}

library(readr)
library(writexl)
############################################################################################################
############################################################################################################
setwd("C:\\Estágio\\Estudo Tucurui\\cbase_a_6")
clast = read_table2("clast.eas")
clast = as.matrix(clast)

cvu = as.numeric(clast[,8])
codigo = as.numeric(clast[,1])

a = cbind(codigo, cvu)
a = a[-which(is.na(a[,1])),]


inflex = read_table2("inflex_disp.txt")			# leitura das informações originais  do NEWAVE, convertidas para o formato .txt
inflex = as.numeric(as.matrix(inflex))
inflex = inflex[-1]						# retirada do primeiro cabeçalho
na = which(is.na(inflex))					# detecção das posições dos  demais cabeçalhos
# organização das informações em formato matriz
inflex = cbind(inflex[1:(na[1]-1)],inflex[(na[1]+1):(na[2]-1)],inflex[(na[2]+1):(na[3]-1)],inflex[(na[3]+1):(na[4]-1)],inflex[(na[4]+1):(na[5]-1)],inflex[(na[5]+1):length(inflex)])

# Função para o cálculo da disponibilidade
fdisp = function(POT,FC,TEIF,IP){
  disp = POT*0.01*FC*(1 - 0.01*TEIF)*(1 - 0.01*IP)
  return(disp)
}

disp = cbind(inflex[,1],fdisp(inflex[,2],inflex[,3],inflex[,4],inflex[,5]))

ofertt = cbind(inflex,disp[,2],a[,2])
colnames(ofertt) = c("Codigo","Pot","FCMAX", "TEIF", "IP", "INFLEX","DISP","CVU")

ofertt = ofertt[order(ofertt[,8]),]

sum(ofertt[,6])

ofertt2 = cbind(ofertt,ofertt[,7]-ofertt[,6])

x = sum(ofertt[,6])

for(i in 2:nrow(ofertt2))
{
  x[i] = x[i-1] + ofertt2[i,9]
}

ofertt2 = cbind(ofertt2,x)

# plot(c(0,x),c(0,ofertt2[,8]), col = "white", main = "Curva de geração termelétrica", ylab = "CVU (R$/MWh)", xlab = "Energia (MWm)")
# lines(c(0,x),c(0,ofertt2[,8]), col = "blue")

cop = ofertt2[,9]*ofertt2[,8]*730

for(i in 2:length(cop))
{
  cop[i] = cop[i-1]+cop[i]
}

ofertt2 = cbind(ofertt2,cop)

########################################################################################################################################################
########################################################################################################################################################

setwd("C:\\Estágio\\Estudo Tucurui\\suishi")

xx = list.files(pattern = "subsis")
ler = function(x){return(read.csv(x,sep = ",",row.names = NULL))}
subsis = lapply(xx,ler)
xx = list.files(pattern = "275")
tucurui = lapply(xx,ler)

meses = c("JANEIRO","FEVEREIRO","MARÇO","ABRIL","MAIO","JUNHO","JULHO","AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO")
inter = c("SE-S","SE-NE","SE-N","SE-FIC","S-SE","S-NE","S-N","S-FIC","NE-SE","NE-S","NE-N","NE-FIC","N-SE","N-S","N-NE","N-FIC")
subs = c("SE","S","NE","N","SIN")

## CMO, GH e GT

# Limites de PLD para 2020
PLDmin = 35.97
PLDmax = 556.58

#Participação Relativa Submercados do SIN em função da Carga de Energia (Fonte ONS, 2017)

cargse = 38199
cargs = 11282
cargne = 10602
cargn = 5502
cargsin = cargse + cargs + cargne + cargn

p1 = cargse / cargsin
p2 = cargs / cargsin
p3 = cargne / cargsin
p4 = cargn / cargsin

pesos = c(p1,p2,p3,p4)

gf_flat = 55343

for(i in 1:length(subsis))
{
  subsis[[i]] = subsis[[i]][-which(subsis[[i]][,2]<2024),]
}

anoini = subsis[[1]][1,2]
anofim = subsis[[1]][nrow(subsis[[1]]),2]


cmo = array(data = NA, dim = c(12,(anofim-anoini+1),5,length(subsis),3))
gh = cmo
gt = cmo
pld = cmo
cop = cmo
gsf = cmo
cmo = provideDimnames(cmo , base = list(meses,as.character(anoini:anofim),c("SE","S","NE","N","SIN"),
                                        c("62m","66m","73m","orig"),c("Medio","5%","95%")))
gh = provideDimnames(gh , base = list(meses,as.character(anoini:anofim),c("SE","S","NE","N","SIN"),
                                        c("62m","66m","73m","orig"),c("Medio","5%","95%")))
gt = provideDimnames(gt , base = list(meses,as.character(anoini:anofim),c("SE","S","NE","N","SIN"),
                                        c("62m","66m","73m","orig"),c("Medio","5%","95%")))
pld = provideDimnames(pld , base = list(meses,as.character(anoini:anofim),c("SE","S","NE","N","SIN"),
                                        c("62m","66m","73m","orig"),c("Medio","5%","95%")))

ghsin = gtsin = NULL
cmosin = pldsin = NULL

for(ii in 1:length(subsis))
{
  subsis[[ii]][,1] = transf(subsis[[ii]][,1])
  
  for(k in 1:5) #submercado
  {
    for(i in anoini:anofim) # ano
    {
      for(j in 1:12) # mes
      {
        aux = subsis[[ii]][which((subsis[[ii]][,1]==k)&(subsis[[ii]][,2]==i)&(subsis[[ii]][,3]==j)),]
        pldAux = aux[,14]
        pldAux[which(pldAux>PLDmax)]=PLDmax
        pldAux[which(pldAux<PLDmin)]=PLDmin
        
        ghsin = rbind(ghsin,aux[,10])
        gtsin = rbind(gtsin,aux[,11])
        
        cmo[j,(i-anoini+1),k,ii,1] = mean(aux[,14])
        gh[j,(i-anoini+1),k,ii,1] = mean(aux[,10])
        gt[j,(i-anoini+1),k,ii,1] = mean(aux[,11])
        pld[j,(i-anoini+1),k,ii,1] = mean(pldAux)
        cmo[j,(i-anoini+1),k,ii,2] = quantile(aux[,14], probs = .05,na.rm = T)
        gh[j,(i-anoini+1),k,ii,2] = quantile(aux[,10], probs = .05,na.rm = T)
        gt[j,(i-anoini+1),k,ii,2] = quantile(aux[,11], probs = .05,na.rm = T)
        pld[j,(i-anoini+1),k,ii,2] = quantile(pldAux, probs = .05,na.rm = T)
        cmo[j,(i-anoini+1),k,ii,3] = quantile(aux[,14], probs = .95,na.rm = T)
        gh[j,(i-anoini+1),k,ii,3] = quantile(aux[,10], probs = .95,na.rm = T)
        gt[j,(i-anoini+1),k,ii,3] = quantile(aux[,11], probs = .95,na.rm = T)
        pld[j,(i-anoini+1),k,ii,3] = quantile(pldAux, probs = .95,na.rm = T)
        
        if(k==5)
        {
          gh[j,(i-anoini+1),k,ii,1] = mean(apply(ghsin,2,sum))
          gt[j,(i-anoini+1),k,ii,1] = mean(apply(gtsin,2,sum))
          cmo[j,(i-anoini+1),k,ii,1] = sum(cmo[j,(i-anoini+1),1:4,ii,1]*pesos)
          pld[j,(i-anoini+1),k,ii,1] = sum(pld[j,(i-anoini+1),1:4,ii,1]*pesos)
          
          gh[j,(i-anoini+1),k,ii,2] = quantile(mean(apply(ghsin,2,sum)),probs = .05,na.rm = T)
          gt[j,(i-anoini+1),k,ii,2] = quantile(mean(apply(ghsin,2,sum)),probs = .05,na.rm = T)
          cmo[j,(i-anoini+1),k,ii,2] = sum(cmo[j,(i-anoini+1),1:4,ii,2]*pesos)
          pld[j,(i-anoini+1),k,ii,2] = sum(pld[j,(i-anoini+1),1:4,ii,2]*pesos)
          
          gh[j,(i-anoini+1),k,ii,3] = quantile(mean(apply(ghsin,2,sum)),probs = .95,na.rm = T)
          gt[j,(i-anoini+1),k,ii,3] = quantile(mean(apply(ghsin,2,sum)),probs = .95,na.rm = T)
          cmo[j,(i-anoini+1),k,ii,3] = sum(cmo[j,(i-anoini+1),1:4,ii,3]*pesos)
          pld[j,(i-anoini+1),k,ii,3] = sum(pld[j,(i-anoini+1),1:4,ii,3]*pesos)
          
          ghsin = gtsin = NULL
        }
        
      }
    }
  }
  
}

for(ii in 1:length(subsis))
{
  gh[,,5,ii,1] = gh[,,1,ii,1]+gh[,,2,ii,1]+
    gh[,,3,ii,1]+gh[,,4,ii,1]
  
  gt[,,5,ii,1] = gt[,,1,ii,1]+gt[,,2,ii,1]+
    gt[,,3,ii,1]+gt[,,4,ii,1]
  
  pld[,,5,ii,1] = pld[,,1,ii,1]
  
  cmo[,,5,ii,1] = cmo[,,1,ii,1]
  
  gh[,,5,ii,2] = gh[,,1,ii,2]+gh[,,2,ii,2]+
    gh[,,3,ii,2]+gh[,,4,ii,2]
  
  gt[,,5,ii,2] = gt[,,1,ii,2]+gt[,,2,ii,2]+
    gt[,,3,ii,2]+gt[,,4,ii,2]
  
  pld[,,5,ii,2] = pld[,,1,ii,2]
  
  cmo[,,5,ii,2] = cmo[,,1,ii,2]
  
  gh[,,5,ii,3] = gh[,,1,ii,3]+gh[,,2,ii,3]+
    gh[,,3,ii,3]+gh[,,4,ii,3]
  
  gt[,,5,ii,3] = gt[,,1,ii,3]+gt[,,2,ii,3]+
    gt[,,3,ii,3]+gt[,,4,ii,3]
  
  pld[,,5,ii,3] = pld[,,1,ii,3]
  
  cmo[,,5,ii,3] = cmo[,,1,ii,3]
}

linhas = c("solid","longdash","dotted")
data_plot=seq.Date(as.Date("1/1/2024",format="%d/%m/%Y"), by = "month",length.out = 60)
aux2 = "\nCaso Oficial vs Mudança no nível mínimo operacional de Tucuruí"

setwd("C:/Estágio/Estudo Tucurui")
pdf(file = "AnaliseComplementarUHETucurui.pdf",width=10,height=10,paper='a4r', title = "Leilão A-6 de 2019")

plotar_cmo <- function(a1,a2,a3,b1,b2,b3,titulo,unidade)
{
  plot(data_plot,a3, ylim = c(min(a2),1.2*max(a3)), col = "white", main = paste(titulo,aux2,sep = "",collapse = NULL), ylab = unidade, xlab = "Tempo")
  polygon(x = c(data_plot,rev(data_plot)),y = c(a2,rev(a3)),col=gray(.3,alpha=.3),border=NA)
  lines(data_plot,a1, col = "blue", lwd = 2)
  lines(data_plot,b1, col = "red", lwd = 2)
  lines(data_plot,b3, col = "red", lty = "dotted", lwd = 2)
  lines(data_plot,b2, col = "red", lty = "dotted", lwd = 2)
  legend("topright", legend=c("Oficial", "Modificado","Limites inferior e superior - Oficial","Limites inferior e superior - Modificado","PLD máximo - R$ 556,58"), lty=c(rep(linhas[1],2),linhas[1],linhas[3],linhas[1]), col = c("blue","red","gray","red","green"), lwd = c(2,2,5,2,2))
  
}

plotar = function(xx,titulo,unidade)
{
  for(i in 1:5)
  {
    if(i==5)
    {
      cores = c("blue","red","gold","green")
      aux = paste(titulo, subs[i], sep = " ", collapse = NULL)
      plot(data_plot,xx[,,i,1,3], ylim = c(min(xx[,,i,,]),1.2*max(xx[,,i,,])), col = "white", 
           main = paste(aux,aux2,sep = "",collapse = NULL), ylab = unidade, xlab = "Tempo")
      polygon(x = c(data_plot,rev(data_plot)),y = c(xx[,,i,4,2],rev(xx[,,i,4,3])),col=gray(.3,alpha=.3),border=NA)
      lines(data_plot,xx[,,i,4,1], col = cores[1], lwd = 2)
      for(ii in 1:3)
      {
        lines(data_plot,xx[,,i,ii,1], col = cores[ii+1], lwd = 2)
        lines(data_plot,xx[,,i,ii,2], col = cores[ii+1], lty = "dotted", lwd = 2)
        lines(data_plot,xx[,,i,ii,3], col = cores[ii+1], lty = "dotted", lwd = 2)
      }
      legend("topright", legend=c("Oficial", "Mínimo operacional igual a 73m", 
                                  "Mínimo operacional igual a 62m",
                                  "Mínimo operacional igual a 66m",
                                  "Limites inferiores e superiores"), lty=c(rep("solid",4),"dotted"), col = c(cores,"gray"))
      
    }
    
  }
}

plotar(cmo,"Custo marginal de Operação -","R$/MWh")
plotar(gh,"Geração Hidrelétrica -","MWmed")
plotar(gt,"Geração Termelétrica -","MWmed")
#plotar(pld,"Preço de Liquidação das Diferenças -","R$/MWh")

library(png)

a = magick::image_read("cenario2.png")
plot(a)

dev.off()


#dev.off()



############################################################################################################
############################################################################################################

traj = 1

if(traj!=1)
{
  # gfband <- rep(55343,12)
  gfband = c(55456,55184,55714,55711,55715,55720,55727,55723,55724,55723,55724,55718)
  gfband = mean(gfband)
  gh_pch=3000
  f_ccee=1.08
  merc_band = 35331.24
  custoCop = NULL
  custogsf = NULL
  cmoT = NULL
  ghT = NULL
  gtT = NULL
  
  for(k in 1:length(subsis))
  {
    aux = subsis[[k]]
    
    pldAux = aux[which(aux[,1]==1),14]
    pldAux[which(pldAux>PLDmax)]=PLDmax
    pldAux[which(pldAux<PLDmin)]=PLDmin
    
    cmo = aux[which(aux[,1]==1),14]
    ghsin = rbind(aux[which(aux[,1]==1),10],
                  aux[which(aux[,1]==2),10],
                  aux[which(aux[,1]==3),10],
                  aux[which(aux[,1]==4),10])
    gtsin = rbind(aux[which(aux[,1]==1),11],
                  aux[which(aux[,1]==2),11],
                  aux[which(aux[,1]==3),11],
                  aux[which(aux[,1]==4),11])
    ghsin = apply(ghsin,2,sum)
    gtsin = apply(gtsin,2,sum)
    
    gsf = (ghsin+gh_pch)/(f_ccee*gfband)
    vu = (-1+gsf)*pldAux
    cgsf = vu*merc_band*730
    cop = NULL
    for(i in 1:length(cmo))
    {
      cop[i] = ofertt2[max(which(as.numeric(ofertt2[,8])<=cmo[i])),11]
    }
    
    cgsfAux = array(dim = length(cgsf)/60)
    copAux = array(dim = length(cop)/60)
    
    for(i in 1:86)
    {
      sum1 = sum2 = 0
      for(j in 1:60)
      {
        sum1 = sum1 + cop[i+86*(j-1)]
        sum2 = sum2 + cgsf[i+86*(j-1)]
      }
      copAux[i] = sum1/5
      cgsfAux[i] = sum2/5
    }
    
    cop = copAux
    cgsf = cgsfAux
    
    custoCop = rbind(custoCop,c(mean(cop),quantile(cop,probs = .05),quantile(cop, probs = .95)))
    
    custogsf = rbind(custogsf,c(mean(cgsf),quantile(cgsf,probs = .05),quantile(cgsf, probs = .95)))
    
    cmoT = rbind(cmoT,c(mean(cmo),quantile(cmo,probs = .05),quantile(cmo, probs = .95)))
    
    ghT = rbind(ghT,c(mean(ghsin),quantile(ghsin,probs = .05),quantile(ghsin, probs = .95)))
    
    gtT = rbind(gtT,c(mean(gtsin),quantile(gtsin,probs = .05),quantile(gtsin, probs = .95)))
    
  }
  
  casos = c("62m","66m","73m","ORIG")
  
  dimnames(custogsf) = list(casos,c("Media","5%","95%"))
  dimnames(custoCop) = list(casos,c("Media","5%","95%"))
  dimnames(cmoT) = list(casos,c("Media","5%","95%"))
  dimnames(ghT) = list(casos,c("Media","5%","95%"))
  dimnames(gtT) = list(casos,c("Media","5%","95%"))
  
  x = list(gsf = as.data.frame(custogsf),
           cop = as.data.frame(custoCop),
           cmo = as.data.frame(cmoT),
           gh = as.data.frame(ghT),
           gt = as.data.frame(gtT))
  
  envoltoria = x
  
}
if(traj==1)
{
  cmo = array(dim=c(86,60,5,4))
  gh = cmo
  gt = cmo
  cop = cmo
  cgsf = cmo
  gfband = c(55456,55184,55714,55711,55715,55720,55727,55723,55724,55723,55724,55718)
  gfband = mean(gfband)
  gh_pch=3000
  f_ccee=1.08
  merc_band = 35331.24
  
  for(ii in 1:length(subsis))
  {
    subsis[[ii]][,1] = transf(subsis[[ii]][,1])
    
    for(k in 1:5) #submercado
    {
      for(i in anoini:anofim) # ano
      {
        for(j in 1:12) # mes
        {
          aux = subsis[[ii]][which((subsis[[ii]][,1]==k)&(subsis[[ii]][,2]==i)&(subsis[[ii]][,3]==j)),]
          cmo[,(j+(i-2024)*12),k,ii] = aux[,14]
          gh[,(j+(i-2024)*12),k,ii] = aux[,10]
          gt[,(j+(i-2024)*12),k,ii] = aux[,11]
        }
      }
    }
    
  }
  
  cmo[,,5,] = p1*cmo[,,1,] + p2*cmo[,,2,] + p3*cmo[,,3,] + p4*cmo[,,4,]
  gh[,,5,] = gh[,,1,] + gh[,,2,] + gh[,,3,] + gh[,,4,]
  gt[,,5,] = gt[,,1,] + gt[,,2,] + gt[,,3,] + gt[,,4,]
  
  ################################################################################################################################
  ################################################################################################################################
  
  setwd("C:\\Estágio\\Estudo Tucurui")
  
  cod = read_table2("TESTE.txt")
  
  len = as.matrix(cod[,1])
  pmo = as.matrix(cod[-which(is.na(cod[,2])),2])
  
  aux = setdiff(len,pmo)
  aux = aux[c(5,9,10)]
  
  dirs = list.files(getwd(),pattern = "USIHID")
  arq0 = list.files()
  pot_exp = array(data = 0,dim = c(length(dirs),12))
  
  for(ii in 1:length(dirs))
  {
    unzip(dirs[ii])
    
    lista = list.files(getwd(),pattern = ".CSV")
    
    for(i in 1:length(aux))
    {
      usi = read.csv(lista[min(grep(aux[i],lista))],sep = ",", row.names = as.character(1:15480))
      
      for(j in 1:12)
      {
        pot_exp[ii,j] = pot_exp[ii,j]+mean(usi[(1+(j-1)*86):(86*j),15])
      }
    }
    
    file.remove(setdiff(list.files(),arq0))
    
  }
  
  pot_exp
  
  perda_gh = 1.627/100
  pcentroide_gh = 2.253/100
  part_pch = .66962
  perda_pch = .179/100
  ande = 2109
  
  pch =  c(3619,3583,3731,3441,3142,3012,2670,2398,2416,2768,3255,3645)*part_pch*(1-perda_pch)
  gf = c(55712,55708,55717,55714,55717,55722,55729,55725,55727,55725,55726,55721)
  f_ccee = 1.08
  
  ################################################################################################################################
  ################################################################################################################################
  
  for(ii in 1:length(subsis))
  {
    for(j in 1:60)
    {
      for(i in 1:86)
      {
        if(cmo[i,j,5,ii]>PLDmax){pld = PLDmax}
        if(cmo[i,j,5,ii]<PLDmin){pld = PLDmin}
        if((cmo[i,j,5,ii]<PLDmax)&&(cmo[i,j,5,ii]>PLDmin)){pld = cmo[i,j,5,ii]}
        
        #((ghModif[i,j,5]-mean(pot_exp))*(1-perda_gh)*(1-pcentroide_gh)+pch[j%%12]-ande)/mean(gf)
        if(j%%12!=0){gh_pch = pch[j%%12]}
        else{gh_pch = pch[12]}
        gsf = ((gh[i,j,5,ii]-mean(pot_exp[ii,]))*(1-perda_gh)*(1-pcentroide_gh)+gh_pch-ande)/(f_ccee*mean(gf))
        #gsf = (gh[i,j,5,ii]+gh_pch)/(f_ccee*gfband)
        cgsf[i,j,5,ii] = (-1+gsf)*pld*merc_band*730
        cop[i,j,5,ii] = ofertt2[max(which(as.numeric(ofertt2[,8])<=cmo[i,j,5,ii])),11]
        
      }
    }
  }
  
  cmoT = NULL
  ghT = NULL
  gtT = NULL
  custoCop = NULL
  custoGsf = NULL
  
  for(i in 1:4)
  {
    cmoT = rbind(cmoT,c(mean(apply(cmo[,,5,i],1,mean)),
                        quantile(apply(cmo[,,5,i],1,mean),probs = c(.05,.95))))
    
    ghT = rbind(ghT,c(mean(apply(gh[,,5,i],1,mean)),
                      quantile(apply(gh[,,5,i],1,mean),probs = c(.05,.95))))
    
    gtT = rbind(gtT,c(mean(apply(gt[,,5,i],1,mean)),
                      quantile(apply(gt[,,5,i],1,mean),probs = c(.05,.95))))
    
    custoCop = rbind(custoCop,c(mean(apply(cop[,,5,i],1,mean)),
                                quantile(apply(cop[,,5,i],1,mean),probs = c(.05,.95))))
    
    custoGsf = rbind(custoGsf,c(mean(apply(cgsf[,,5,i],1,mean)),
                                quantile(apply(cgsf[,,5,i],1,mean),probs = c(.05,.95))))
    
  }
  
  casos = c("62m","66m","73m","ORIG")
  
  dimnames(custoGsf) = list(casos,c("Media","5%","95%"))
  dimnames(custoCop) = list(casos,c("Media","5%","95%"))
  dimnames(cmoT) = list(casos,c("Media","5%","95%"))
  dimnames(ghT) = list(casos,c("Media","5%","95%"))
  dimnames(gtT) = list(casos,c("Media","5%","95%"))
  
  x = list(gsf = as.data.frame(custoGsf),
           cop = as.data.frame(custoCop),
           cmo = as.data.frame(cmoT),
           gh = as.data.frame(ghT),
           gt = as.data.frame(gtT))
  
  traj = x
  
}

resumo = list()
alt = read.csv("alteracao.csv")

alt[,2] = rep(1:3,each=2)

for(i in 1:3)
{
  aux = cbind(rev(traj$cmo[,i]),
                      rev(traj$cop[,i])/10^6,
                      rev(traj$gsf[,i])/10^6)
  
  a = alt[which(alt[,2]==i),]
  for(k in 1:nrow(a))
  {
    a[k,] = virgP(as.matrix(a[k,]))
  }
  
  aux[1:nrow(a),] = as.matrix(a[,3:5])
  
  for(k in 1:nrow(aux))
  {
    aux[k,] = round(as.numeric(aux[k,]),digits = 2)
  }
  
  resumo[[i]] = aux 
  row.names(resumo[[i]]) = c("Oficial","73m","66m","62m")
  colnames(resumo[[i]]) = c("CMO (R$/MWh)","Custo de operação (milhões de reais/mês)",
                            "Resultado da Exposição ao Risco Hidrológico (milhões de reais/mês)")
  
}

# plot(NA, main = "Cenário Médio")
# gridExtra::grid.table(resumo[[1]])
# plot(NA)
# gridExtra::grid.table(resumo[[2]])
# plot(NA)
# gridExtra::grid.table(resumo[[3]])
a = resumo[[2]][3:4,3]
resumo[[2]][3:4,3] = resumo[[3]][3:4,3]
resumo[[3]][3:4,3] = a

gridExtra::grid.table(resumo[[1]])
