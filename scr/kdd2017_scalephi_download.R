#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#
#
#         SCRIPT PARA FAZER DOWNLOAD DE TODOS OS JOGOS COM LINK
#Autora: Raquel Aoki
#Data:  11/02/2016
#------#------#------#------#------#------#------#------#------#------#------#------#------#------#------#

#check 5  139  326  352  378 1110


rm(list = ls( all = T))
require(XML)
#setwd("C://Users//Raquel Aoki//Google Drive//UFMG//Pesquisa//V028//Dados Escala Phi")
#setwd("E:\\Pesquisa\\V028\\Dados Escala Phi")
setwd("C:\\Users\\Raquel Aoki\\Documents\\Dados Escala Phi")


links = read.table("jogos_links.csv", header=T, sep=",")

links$link = as.character(links$link)
links$erro = rep("nao",dim(links)[1])
links$quant_jogos = rep(0,dim(links)[1])
links$quant_equipes = rep(0,dim(links)[1])
links$id  = rep("",dim(links)[1])
links$jogos.canc = rep(0,dim(links)[1])
links = subset(links, link!="")

for(i in 1:dim(links)[1]){
  tab = readHTMLTable(links$link[i], as.data.frame = TRUE)
  jogos = tab[[1]]
  if(dim(jogos)[1]==0) links$erro[i]="sim"
  #estruturando o resultado 
  if(links$erro[i]=="nao"){
  if(links$sport[i]=="soccer"|links$sport[i]=="handball"){
    jogos = subset(jogos, selec=c(V1, V2, V6))
    names(jogos)=c("V1","V2","V6")
  }
  if(links$sport[i]=="basketball"|links$sport[i]=="voleyball"){
      jogos = subset(jogos, selec=c(V1, V2, V5))
      names(jogos)=c("V1","V2","V6")
    }
      jogos$V1 = as.character(jogos$V1)
      jogos$V2 = as.character(jogos$V2)
	jogos.cancelados = dim(subset(jogos,V2=="CAN."))[1]
      jogos = subset(jogos, V2!="CAN.")
      jogos = subset(jogos, V2 != "")
      jogos$V2 = gsub(" AWA.","",jogos$V2)
      jogos$V2 = gsub(" ET","",jogos$V2)
	jogos$V2 = gsub(" WO.","",jogos$V2)
	#quando tem o resultado + cancelado 
	aux = strsplit(jogos$V2, split=" ")
	aux1 = c()
	for( j in 1:dim(jogos)[1]) aux1[j]=length(aux[[j]])
	jogos = data.frame(jogos, aux1)
	jogos = subset(jogos, aux1==1)
      equipes = strsplit(jogos$V1, split=" - ")
      scores = strsplit(jogos$V2, split = ":")
      time1 = c();time2 = c()
      score1 = c();score2=c()
      for(j in 1:dim(jogos)[1]){
        time1[j] = equipes[[j]][1]
        time2[j] = equipes[[j]][2]
        score1[j] = scores[[j]][1]
        score2[j] = scores[[j]][2]
      }
      jogos = data.frame(MANDANTE = time1, VISITANTE = time2, gol_man=score1, gol_vis= score2, data = jogos$V6)
      jogos = subset(jogos, !is.na(gol_vis))
      jogos$MANDANTE = as.character(jogos$MANDANTE)
      jogos$VISITANTE = as.character(jogos$VISITANTE)
      jogos$gol_man = as.numeric(as.character(jogos$gol_man))
      jogos$gol_vis = as.numeric(as.character(jogos$gol_vis))
    }
  
  if(dim(jogos)[1]==0) links$erro[i]="sim"
  jogos$sport = rep(links$sport[i],dim(jogos)[1])
  id = paste(links$sport[i],links$country[i],links$gender[i],links$season_finalyear[i],sep="_")
  jogos$id = rep(id, dim(jogos)[1])
  write.table(jogos,paste(id,".txt",sep=""),row.names=F, sep=";")
  links$quant_jogos[i]=dim(jogos)[1]
  links$quant_equipes[i] = length(unique(c(jogos$MANDANTE,jogos$VISITANTE)))
  links$id[i] = id
  links$jogos.canc[i] = jogos.cancelados
}


write.table(links,"jogos_download.csv",row.names=FALSE, sep=";")
