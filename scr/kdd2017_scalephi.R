rm(list = ls( all = T))
#setwd("C://Users//Raquel Aoki//Google Drive//UFMG//Pesquisa//V028//Dados Escala Phi")
#setwd("E:\\Pesquisa\\V028\\Dados Escala Phi")
setwd("C:\\Users\\Raquel Aoki\\Documents\\Dados Escala Phi")
links = read.table("kdd2017_data_scalephi_download.csv",header=T,sep=";")
links$quant_ideal = links$quant_equipes*(links$quant_equipes-1)
links = subset(links, quant_jogos>=quant_ideal)

jogos = read.table(paste(links$id[1],".txt",sep=""),header = T,sep=";")

funcao.escala_phi<-function(jogos,esporte,alpha){
  jogos$MANDANTE = as.character(jogos$MANDANTE)
  jogos$VISITANTE = as.character(jogos$VISITANTE)	
  jogos$gol_man = as.numeric(as.character(jogos$gol_man))
  jogos$gol_vis = as.numeric(as.character(jogos$gol_vis))
  
  # selecionando apenas dois jogos entre as equipes 	
  jogos$id = paste(jogos$MANDANTE, jogos$VISITANTE, sep="_")
  jogos = subset(jogos, !duplicated(id, fromLast = F)	)
  jogos = subset(jogos, select=-c(id))
  
  jogos$dif = jogos$gol_man-jogos$gol_vis
  jogos =  subset(jogos, !is.na(dif))
  equipes = unique(c(jogos$MANDANTE, jogos$VISITANTE))
  equipes = equipes[order(equipes)]
  
  #probabilidade de vencer em casa, fora de casa 
  if(esporte=="voleyball"){	
    ph1 = sum(jogos$dif>=2)/length(jogos$dif)  
    ph2 = sum(jogos$dif==1)/length(jogos$dif)  
    pa1 = sum(jogos$dif<=-2)/length(jogos$dif)  
    pa2 = sum(jogos$dif==-1)/length(jogos$dif)  
    tab1 = data.frame(p =c("p1","p2","p3","p4"),prob=c(ph1,ph2,pa1,pa2))
    #Media e variancia dos pontos ganhos em casa e fora de casa
    mh = 3*ph1+2*ph2+1*pa2 ; vh = 9*ph1+4*ph2+pa2 - mh^2 
    ma = 3*pa1+2*pa2+1*ph2 ; va = 9*pa1+4*pa2+ph2-  ma^2
    
    pontuacao_final = data.frame(equipes = equipes, pontos=rep(0,length(equipes)))
    for(i in 1:dim(jogos)[1]){
      if(jogos$dif[i]>=2) pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +3
      if(jogos$dif[i]<=-2) pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +3
      if(jogos$dif[i]==1){
        pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +2
        pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +1
      }
      if(jogos$dif[i]==-1){
        pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +1
        pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +2
      }
    } #for
  }# if voley
  
  if(esporte=="handball"){	
    ph = sum(jogos$dif>0)/length(jogos$dif)  
    pa = sum(jogos$dif<0)/length(jogos$dif)  
    pe = sum(jogos$dif==0)/length(jogos$dif)  
    p.aux  = 0 
    tab1 = data.frame(p =c("p1","p2","p3","p4"),prob=c(ph,pa,pe,p.aux))
    #Media e variancia dos pontos ganhos em casa e fora de casa
    mh = 2*ph + pe ; vh = 4*ph+pe - mh^2 
    ma = 2*pa + pe ; va = 4*pa+pe - ma^2
    
    pontuacao_final = data.frame(equipes, pontos=rep(0,length(equipes)))
    for(i in 1:dim(jogos)[1]){
      if(jogos$dif[i]>0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +2
      if(jogos$dif[i]<0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +2
      if(jogos$dif[i]==0){
        pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +1
        pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +1
      }
    }#for
  }# if hand
  
  if(esporte=="basketball"){
    ph = sum(jogos$dif>0)/length(jogos$dif)  
    pa = sum(jogos$dif<0)/length(jogos$dif)
    p.aux  = 0
    tab1 = data.frame(p =c("p1","p2","p3","p4"),prob=c(ph,pa,p.aux,p.aux))
    #Media e variancia dos pontos ganhos em casa e fora de casa
    mh = ph  ; vh = ph - mh^2 
    ma = pa  ; va = pa-  ma^2
    
    pontuacao_final = data.frame(equipes = equipes, pontos=rep(0,length(equipes)))
    for(i in 1:dim(jogos)[1]){
      if(jogos$dif[i]>0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +2
      if(jogos$dif[i]<0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +2
    } #for
  }#if basket
  
  if(esporte=="soccer"){
    ph = sum(jogos$dif>0)/length(jogos$dif)  
    pa = sum(jogos$dif<0)/length(jogos$dif)  
    pe = sum(jogos$dif==0)/length(jogos$dif)
    p.aux =0
    tab1 = data.frame(p =c("p1","p2","p3","p4"),prob=c(ph,pa,pe,p.aux))
    #Media e variancia dos pontos ganhos em casa e fora de casa
    mh = 3*ph + pe ; vh = 9*ph+pe - mh^2 
    ma = 3*pa + pe ; va = 9*pa+pe - ma^2
    
    pontuacao_final = data.frame(equipes, pontos=rep(0,length(equipes)))
    for(i in 1:dim(jogos)[1]){
      if(jogos$dif[i]>0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +3
      if(jogos$dif[i]<0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +3
      if(jogos$dif[i]==0){
        pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +1
        pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +1
      }
    }#for
  }#if soccer
  
  pontuacao_final = pontuacao_final[order(pontuacao_final$pontos,decreasing = T),]
  campeao = pontuacao_final$equipes[1]
  # Media e variancia de yk
  mk = (dim(jogos)[1]/length(equipes))*(mh+ma)
  vk = (dim(jogos)[1]/length(equipes))*(vh+va)
  
  media.amostral = mean(pontuacao_final$pontos)
  var.amostral = var(pontuacao_final$pontos)
  cv = sd(pontuacao_final$pontos)/mean(pontuacao_final$pontos)

  #--------------------------- Escala 
  #intervalo de confianca
  variancia.simulacao = c()
  for(ind in 1:10000){ 
    aux = rnorm(length(equipes), mk, sd = sqrt(vk))
    variancia.simulacao[ind] = var(aux)
  }


if(var.amostral>=vk) valor.phi = (var.amostral-vk)/var.amostral
if(var.amostral<vk) valor.phi = (var.amostral-vk)/vk
quantile1 = quantile(variancia.simulacao, alpha/2)
quantile2 = quantile(variancia.simulacao, 1-alpha/2)
#pch 18 dentro do intervalo e 19 fora
pch = 19
if(var.amostral>=quantile1 & var.amostral<=quantile2) pch = 18


  
  #quantos times devem ser retirados atÃ© ficar aleatÃ³rio?
  # codigo repete etapa acima
  if(pch == 19 ) {
    equipes.2 = equipes
    pch.2 = pch
    retirar.2 = c()
    sinal.2 = c()
    jogos.2 = jogos
    while(pch.2 == 19 & (length(retirar.2)+2)<length(equipes)){
      #selecionando a equipe mais distante da mÃ©dia
      pontuacao_final = pontuacao_final[order(pontuacao_final$pontos, decreasing=T),]
      pontuacao_final$distancia = abs(pontuacao_final$pontos - mean(pontuacao_final$pontos))
      retirar = as.character(subset(pontuacao_final, distancia == max(distancia))$equipes)
      if(length(retirar)>1) retirar = retirar[1]
      
      #retirando a equipe
      equipes.2 = equipes.2[equipes.2!=retirar]
      jogos.2 = subset(jogos.2, MANDANTE!= retirar)
      jogos.2 = subset(jogos.2, VISITANTE != retirar)
      
      #Verificando se Ã© o melhor ou pior time do campeonato
      aux = as.character(pontuacao_final$equipes[1])
      aux2 = as.character(pontuacao_final$equipes[2])
      if(aux==retirar | aux2 == retirar) {retirar = paste("+", retirar, sep=""); sinal = "+"}
      aux2 = as.character(pontuacao_final$equipes[dim(pontuacao_final)[1]-1])
      aux = as.character(pontuacao_final$equipes[dim(pontuacao_final)[1]])
      if(aux==retirar| aux2 == retirar) {retirar = paste("-", retirar, sep=""); sinal = "-"}
      
      cat(length(retirar.2)," retirando ", retirar, "\n")
      
      #probabilidade de vencer em casa, fora de casa 
      if(esporte=="voleyball"){	
        ph1 = sum(jogos.2$dif>=2)/length(jogos.2$dif)  
        ph2 = sum(jogos.2$dif==1)/length(jogos.2$dif)  
        pa1 = sum(jogos.2$dif<=-2)/length(jogos.2$dif)  
        pa2 = sum(jogos.2$dif==-1)/length(jogos.2$dif)  
        #Media e variancia dos pontos ganhos em casa e fora de casa
        mh = 3*ph1+2*ph2+1*pa2 ; vh = 9*ph1+4*ph2+pa2 - mh^2 
        ma = 3*pa1+2*pa2+1*ph2 ; va = 9*pa1+4*pa2+ph2-  ma^2
        
        pontuacao_final = data.frame(equipes.2 , pontos=rep(0,length(equipes.2)))
        for(i in 1:dim(jogos.2)[1]){
          if(jogos.2$dif[i]>=2) pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] +3
          if(jogos.2$dif[i]<=-2) pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] +3
          if(jogos.2$dif[i]==1){
            pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] +2
            pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] +1
          }
          if(jogos.2$dif[i]==-1){
            pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] +1
            pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] +2
          }
        } #for
      }# if voley
      if(esporte=="basketball"){
        ph = sum(jogos.2$dif>0)/length(jogos.2$dif)  
        pa = sum(jogos.2$dif<0)/length(jogos.2$dif)  
      #  tab1 = data.frame(p =c("p_H","p_A"),prob=c(ph,pa))
        #Media e variancia dos pontos ganhos em casa e fora de casa
        mh = ph  ; vh = ph - mh^2 
        ma = pa  ; va = pa-  ma^2
        
        pontuacao_final = data.frame(equipes.2 = equipes.2, pontos=rep(0,length(equipes.2)))
        for(i in 1:dim(jogos.2)[1]){
          if(jogos.2$dif[i]>0) pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] +2
          if(jogos.2$dif[i]<0) pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] +2
        } #for
      }#if basket
      
      if(esporte=="handball"){	
        ph = sum(jogos.2$dif>0)/length(jogos.2$dif)  
        pa = sum(jogos.2$dif<0)/length(jogos.2$dif)  
        pe = sum(jogos.2$dif==0)/length(jogos.2$dif)  	
      #  tab1 = data.frame(p =c("p_H","p_A","p_E"),prob=c(ph,pa,pe))
        #Media e variancia dos pontos ganhos em casa e fora de casa
        mh = 2*ph + pe ; vh = 4*ph+pe - mh^2 
        ma = 2*pa + pe ; va = 4*pa+pe - ma^2
        
        pontuacao_final = data.frame(equipes.2, pontos=rep(0,length(equipes.2)))
        for(i in 1:dim(jogos.2)[1]){
          if(jogos.2$dif[i]>0) pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] +2
          if(jogos.2$dif[i]<0) pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] +2
          if(jogos.2$dif[i]==0){
            pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] +1
            pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] +1
          }
        }#for
      }# if hand
      
      if(esporte=="soccer"){
        ph = sum(jogos.2$dif>0)/length(jogos.2$dif)  
        pa = sum(jogos.2$dif<0)/length(jogos.2$dif)  
        pe = sum(jogos.2$dif==0)/length(jogos.2$dif)  	
    #    tab1 = data.frame(p =c("p_H","p_A","p_E"),prob=c(ph,pa,pe))
        #Media e variancia dos pontos ganhos em casa e fora de casa
        mh = 3*ph + pe ; vh = 9*ph+pe - mh^2 
        ma = 3*pa + pe ; va = 9*pa+pe - ma^2
        
        pontuacao_final = data.frame(equipes.2, pontos=rep(0,length(equipes.2)))
        for(i in 1:dim(jogos.2)[1]){
          if(jogos.2$dif[i]>0) pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] +3
          if(jogos.2$dif[i]<0) pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] +3
          if(jogos.2$dif[i]==0){
            pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$MANDANTE[i]] +1
            pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes.2==jogos.2$VISITANTE[i]] +1
          }
        }#for
      }#if soccer 
      
      
      # Media e variancia de yk
      mk = (dim(jogos.2)[1]/length(equipes.2))*(mh+ma)
      vk = (dim(jogos.2)[1]/length(equipes.2))*(vh+va)   
      media.amostral = mean(pontuacao_final$pontos)
      var.amostral =  var(pontuacao_final$pontos)
      
      #--------------------------- escala
      #intervalo de confianca     
      nequipes = length(equipes.2)
      variancia.simulacao = c()
	if(vk==0) vk=0.0001
      for(ind in 1:1000){ 
        aux = rnorm(nequipes, mk, sd = sqrt(vk))
        variancia.simulacao[ind] = var(aux)
      }

	if(var.amostral>=vk) valor.phi.2 = (var.amostral-vk)/var.amostral
	if(var.amostral<vk) valor.phi.2 = (var.amostral-vk)/vk
	quantile1 = quantile(variancia.simulacao, alpha/2)
	quantile2 = quantile(variancia.simulacao, 1-alpha/2)
	#pch 18 dentro do intervalo e 19 fora
	pch.2 = 19
	if(var.amostral>=quantile1 & var.amostral<=quantile2) pch.2 = 18

      retirar.2 = c(retirar.2, retirar)
      sinal.2 = paste(sinal.2, sinal,sep="")
    }
    
  }else{
    retirar.2 = c()
    sinal.2 = c()
    valor.phi.2 = valor.phi
  }
  #juntando os nomes de todos os times em 1 elemento	
  aux3 = retirar.2[1]
  if(length(retirar.2)>1){
    for(i in 2:length(retirar.2)) aux3 = paste(aux3, ",", retirar.2[i],sep="")
  }
  if(is.null(aux3)) {aux3=".";sinal.2="."}

  return(list(valor.phi,cv, dim(jogos)[1],length(equipes), pch,length(retirar.2),aux3,valor.phi.2,sinal.2,tab1,campeao)) #valor.phi.2 Ã© a valor.phi,var apos retirar
}

#tab1 tem as probabilidades de cada resultado:
#soccer: p1=home; p2=visitante;p3 = empate
#handball: p1=home; p2=visitante;p3 = empate
#basketball: p1=home;p2=visitante
#voley: p1=home3.01 ; p2=home3.2 ; p3=visitante01.3 ; p4 = visitante2:3
links = subset(links, id!="")
alpha = 0.05
for(k in 1:dim(links)[1]){
  esporte = links$sport[k]
  jogos = read.table(paste(links$id[k],".txt",sep=""),header=T,sep=";")
  resul = funcao.escala_phi(jogos,esporte,alpha)
  if(k==1){
  tab  = data.frame(sport=links$sport[k],country = links$country[k],league = links$name_ligue[k],year =links$season_finalyear[k],
                    gender = links$gender[k], campeao = links$campeao[k], jogos.original = links$quant_jogos[k],
                    jogos = resul[[3]], equipes = resul[[4]], jogos.equipe = resul[[3]]*2/resul[[4]],
                   valor.phi = resul[[1]], cv = 100*resul[[2]], pch = resul[[5]],retirados = resul[[6]],
                   equipes.retiradas = resul[[7]],valor.phi.2 = resul[[8]],sinal = resul[[9]],prob=t(resul[[10]][2]),
                   campeao.func = resul[[11]])
  }else{
    tab0  = data.frame(sport=links$sport[k],country = links$country[k],league = links$name_ligue[k],year =links$season_finalyear[k],
                      gender = links$gender[k], campeao = links$campeao[k], jogos.original = links$quant_jogos[k],
                      jogos = resul[[3]], equipes = resul[[4]], jogos.equipe = resul[[3]]*2/resul[[4]],
                      valor.phi = resul[[1]], cv = 100*resul[[2]], pch = resul[[5]],retirados = resul[[6]],
                      equipes.retiradas = resul[[7]],valor.phi.2 = resul[[8]],sinal = resul[[9]],prob=t(resul[[10]][2]),
                      campeao.func = resul[[11]])
    
    tab = rbind(tab,tab0)
  }
}

tab$pch = as.character(tab$pch)
tab$pch[tab$pch=="19"]="Different"
tab$pch[tab$pch=="18"]="Equal"
write.table(tab, "escala_phi_jogos4.csv",row.names = F, sep=";")

#o 3. é sem retirar jogos 
# o 4. é retirando os jogos (mais de um "casal")
k
