#---------#---------#---------#---------#---------#---------#---------#---------#---------#
#			ANALISE %VARIANCIA FUTEBOL
#AUTORA: RAQUEL AOKI
#DATA:  10/07/2016
#---------#---------#---------#---------#---------#---------#---------#---------#---------#

rm(list = ls( all = T))
require(XML)
require(xtable)

#---------#---------#---------#---------#---------#---------#---------#---------#---------#
#------ FUNCOES QUE COLETAM OS JOGOS DE UM ENDERECO E FAZ AS ANALISES 

funcao.futebol<-function(u){
	if(u=="http://www.betexplorer.com/soccer/japan/j-league-2015/results/"){
	 tables1 = readHTMLTable("http://www.betexplorer.com/soccer/japan/j-league-2015/results/?stage=IeoiSOWB", as.data.frame = TRUE)
 	 jogos1 = tables1[[1]]		
	 tables2 = readHTMLTable("http://www.betexplorer.com/soccer/japan/j-league-2015/results/?stage=zssYlsNI", as.data.frame = TRUE)
 	 jogos2 = tables2[[1]]	
	 jogos = rbind(jogos1, jogos2)
	}else{
		tables = readHTMLTable(u, as.data.frame = TRUE)
		jogos = tables[[1]]
	}
	#estruturando o resultado 
	jogos = subset(jogos, selec=c(V1, V2, V6))
	jogos$V1 = as.character(jogos$V1)
	jogos$V2 = as.character(jogos$V2)
	equipes = strsplit(jogos$V1, split=" - ")
	scores = strsplit(jogos$V2, split = ":")
	time1 = c();time2 = c()
	score1 = c();score2=c()
	for(i in 1:dim(jogos)[1]){
		time1[i] = equipes[[i]][1]
		time2[i] = equipes[[i]][2]
		score1[i] = scores[[i]][1]
		score2[i] = scores[[i]][2]
	}
	jogos = data.frame(MANDANTE = time1, VISITANTE = time2, gol_man=score1, gol_vis= score2, data = jogos$V6)
	jogos = subset(jogos, !is.na(gol_vis))
	jogos$MANDANTE = as.character(jogos$MANDANTE)
	jogos$VISITANTE = as.character(jogos$VISITANTE)
	jogos$gol_man = as.numeric(jogos$gol_man)
	jogos$gol_vis = as.numeric(jogos$gol_vis)
	jogos$dif = jogos$gol_man-jogos$gol_vis
	#probabilidade de vencer em casa, fora de casa e de empatar
	ph = sum(jogos$dif>0)/length(jogos$dif)  
	pa = sum(jogos$dif<0)/length(jogos$dif)  
	pe = sum(jogos$dif==0)/length(jogos$dif)  

	#Teste se todas as equipes tem a mesma quantidade de jogos dentro e fora de casa
	equipes = unique(as.character(jogos$MANDANTE))
	resp = c()
	for(i in 1:length(equipes)){
	  resp = c(resp, dim(subset(jogos, MANDANTE==equipes[i]))[1]==dim(subset(jogos, VISITANTE==equipes[i]))[1])
	}
	if(sum(resp)==length(equipes)) cat("\n Todos tem a mesma quantidade de jogos dentro e fora de casa: SIM \n")
	if(sum(resp)!=length(equipes)) cat("\n Todos tem a mesma quantidade de jogos dentro e fora de casa: NAO \n")

	#Media e variancia dos pontos ganhos em casa e fora de casa
	mh = 3*ph + pe ; vh = 9*ph+pe - mh^2 
	ma = 3*pa + pe ; va = 9*pa+pe - ma^2

	# Media e variancia de yk
	mk = (dim(jogos)[1]/length(equipes))*(mh+ma)
	vk = (dim(jogos)[1]/length(equipes))*(vh+va)
	tab1 = data.frame(p =c("p_H","p_A","p_E"),prob=c(ph,pa,pe))
	#--------------percentual variancia 
	#variancia teorica: vk
	#variancia amostral 
	pontuacao_final = data.frame(equipes, pontos=rep(0,length(equipes)))
	for(i in 1:dim(jogos)[1]){
		if(jogos$dif[i]>0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +3
		if(jogos$dif[i]<0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +3
		if(jogos$dif[i]==0){
			 pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +1
			 pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +1
		}
	}
	cat("\n")
	print(pontuacao_final[order(pontuacao_final$pontos,decreasing=T),])

	media.amostral = sum(pontuacao_final$pontos)/length(pontuacao_final$pontos)
	var.amostral = sum(pontuacao_final$pontos^2)/length(pontuacao_final$pontos) - media.amostral^2
	cv = sd(pontuacao_final$pontos)/mean(pontuacao_final$pontos)
	#---------------------------percentual escalado 
	partidas = dim(jogos)[1]
	nequipes = length(equipes)
	partidas.nequipes = partidas*2/nequipes
	var.min = sd(rep(partidas.nequipes*2*1,nequipes))# todas empatam
	var.max = seq(partidas.nequipes*3,partidas.nequipes*3-2*3*(nequipes-1),-6)
	var.max = var(var.max)
	#ideia do pedro 
	if(var.amostral>vk) perc = (var.amostral- vk)/(var.max-vk)
	if(var.amostral<=vk) perc =(var.amostral-vk)/vk
	cat("\n\n\n")
	cat("Dim Jogos ",dim(jogos)[1]," numero equipes " ,length(equipes))
	cat("\n\n\n")
	return(list(tab1, perc,cv, dim(jogos)[1],length(equipes)))
}

#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#			BRASILEIRAO SERIE A
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
U = paste("http://www.betexplorer.com/soccer/brazil/serie-a-",2006:2015,"/results/",sep="")
anos = 2006:2015
for(ind in 1:length(U)){
	variavel = funcao.futebol(U[ind])
	if(ind==1){
	tab.a = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
		prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
	}else{
		tab.aux = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
		prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
		tab.a = rbind(tab.a, tab.aux)
	}
}

#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#			BRASILERIAO SERIE B
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#

U = paste("http://www.betexplorer.com/soccer/brazil/serie-b-",2006:2015,"/results/",sep="")
anos = 2006:2015
for(ind in 1:length(U)){
	variavel = funcao.futebol(U[ind])
	if(ind==1){
	tab.b = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
		prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
	}else{
		tab.aux = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
		prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
		tab.b = rbind(tab.b, tab.aux)
	}
}

#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#			SUPER LEAGUE - CHINA
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
U = paste("http://www.betexplorer.com/soccer/china/super-league-",2006:2015,"/results/",sep="")
anos = 2006:2015
for(ind in 1:length(U)){
	variavel = funcao.futebol(U[ind])
	if(ind==1){
	tab.b = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
		prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
	}else{
		tab.aux = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
		prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
		tab.b = rbind(tab.b, tab.aux)
	}
}






#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#			MAJOR LEAGUE SOCCER - MLS
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#

U = rbind("http://www.betexplorer.com/soccer/usa/mls-2006/results/?stage=65HPyDbH",
	"http://www.betexplorer.com/soccer/usa/mls-2007/results/?stage=UFl5WCtj",
	"http://www.betexplorer.com/soccer/usa/mls-2008/results/?stage=IsxHSx6l",
	"http://www.betexplorer.com/soccer/usa/mls-2009/results/?stage=WSEswaLf",
	"http://www.betexplorer.com/soccer/usa/mls-2010/results/?stage=r3IoxJz1",
	"http://www.betexplorer.com/soccer/usa/mls-2011/results/?stage=CzPzA98h",
	"http://www.betexplorer.com/soccer/usa/mls-2012/results/?stage=YLhUdZys",
	"http://www.betexplorer.com/soccer/usa/mls-2013/results/?stage=M7EMGoSE",
	"http://www.betexplorer.com/soccer/usa/mls-2014/results/?stage=SW92ri50",
	"http://www.betexplorer.com/soccer/usa/mls-2015/results/?stage=hM3ZHhHC")
anos = 2006:2015
for(ind in 1:length(U)){
	variavel = funcao.futebol(U[ind])
	if(ind==1){
	tab.c = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
		prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
	}else{
		tab.aux = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
		prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
		tab.c = rbind(tab.c, tab.aux)
	}
}

#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#			PRIMEIRA DIVISION - SPAIN
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#

U = paste("http://www.betexplorer.com/soccer/spain/primera-division-",2006:2015,"-",2007:2016, "/results/",sep="")
anos = 2006:2015
for(ind in 1:length(U)){
	variavel = funcao.futebol(U[ind])
	if(ind==1){
	tab.d = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
		prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
	}else{
		tab.aux = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
		prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
		tab.d = rbind(tab.d, tab.aux)
	}
}



#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#			SERIE A ITALY
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#


U = paste("http://www.betexplorer.com/soccer/italy/serie-a-",2005:2014,"-",2006:2015,"/results/",sep="")
anos = 2006:2015
for(ind in 1:length(U)){
  variavel = funcao.futebol(U[ind])
  if(ind==1){
    tab.e = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
                       prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
  }else{
    tab.aux = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
                         prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
    tab.e = rbind(tab.e, tab.aux)
  }
}


#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#			DIVISION 1 - ALGERIA
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#

U = paste("http://www.betexplorer.com/soccer/algeria/division-1-",2006:2015,"-",2007:2016, "/results/",sep="")
anos = 2006:2015
for(ind in 1:length(U)){
  variavel = funcao.futebol(U[ind])
  if(ind==1){
    tab.f = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
                       prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
  }else{
    tab.aux = data.frame(ano = anos[ind], jogos = variavel[[4]], equipes = variavel[[5]], jogos.equipe = variavel[[4]]*2/variavel[[5]],
                         prob = t(variavel[[1]][2]), perc.var = variavel[[2]], cv = 100*variavel[[3]])
    tab.f = rbind(tab.f, tab.aux)
  }
}


#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#			SALVANDO RESULTADAOS PARA NAO TER QUE RODAR NOVAMENTE
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#

write.table(tab.a,"resul_brasil_seriea.txt", row.names=F, sep=";")
write.table(tab.b,"resul_brasil_serieb.txt", row.names=F, sep=";")
write.table(tab.c,"resul_eua_mls.txt", row.names=F, sep=";")
write.table(tab.d,"resul_espanha_primeira_division.txt", row.names=F, sep=";")
write.table(tab.e,"resul_italy_seriea.txt", row.names=F, sep=";")
write.table(tab.f,"resul_argelia_division1.txt", row.names=F, sep=";")


tab.a = read.table("resul_brasil_seriea.txt", header=T, sep=";")
tab.b = read.table("resul_brasil_serieb.txt", header=T, sep=";")
tab.c = read.table("resul_eua_mls.txt", header=T, sep=";")
tab.d = read.table("resul_espanha_primeira_division.txt", header=T, sep=";")
tab.e = read.table("resul_italy_seriea.txt", header=T, sep=";")
tab.f = read.table("resul_argelia_division1.txt", header=T, sep=";")




#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#           GRAFICOS
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#

#------- perc da variância 6 CAMPEONATOS 
par(mar=c(1,1,1,1))
tabela1 = cbind(tab.a[,8],tab.b[,8],tab.c[,8],tab.d[,8],tab.e[,8],tab.f[,8])
campeonatos = c("Série A - Brasil", "Série B - Brasil", "Major League Soccer - EUA", 
                "Primeira División - Espanha", "Serie A - Italy","Ligue 1 - Argélia")
col= c("green3","lightgreen","blue","darkgoldenrod2","red","darkgreen")
pch = c(15,16,17,15,16,17)
par(mar=c(4.4,4,1,2))
plot(tabela1[,1], type="b", lwd = 2, col = col[1],pch=pch[1], axes=F,main="",
     xlab="Temporadas", ylab="Escala de Variabilidade Devido ao Acaso", ylim =c(-0.7,0.40), cex.lab=1)
box()
axis(1, 1:10, 2006:2015,cex.axis=1)
axis(2, seq(-0.7,0.30,0.1),round(seq(-0.7,0.30,0.1),2),cex.axis=1)
for(i in 2:6) points(tabela1[,i], type="b", lwd=2, col=col[i],pch = pch[i])
legend("topleft",ncol=3, lwd=2, col=col, pch=pch,bty="n", cex=1, legend=campeonatos)
abline(h=seq(-0.7,0.30,0.1), col="grey", lty=3)
abline(h=0, col="black", lty=1)

savePlot(filename="artigo_grafico1", type="png", device=dev.cur())

#----- probabildiade dos resultados 

#windows(10,6)
par(mfrow=c(2,3))
#par(mar=c(4.4,4,1,2))

for(i in 1:6){
  if(i==1) tab = tab.a
  if(i==2) tab = tab.b
  if(i==3) tab = tab.c
  if(i==4) tab = tab.d
  if(i==5) tab = tab.e
  if(i==6) tab = tab.f
  plot(tab[,5],col="blue",type="b",ylim=c(0.15,0.75),axes=F,main=campeonatos[i], xlab="Temporadas",
     ylab="Probabilidade",pch=1,lwd=2)
axis(1,1:10,2006:2015)
axis(2,seq(0.15,0.75,0.1),seq(0.15,0.75,0.1))
points(tab[,6],col="orange",type="b",pch=2,lwd=2)
points(tab[,7],col="green",type="b",pch=3,lwd=2)
legend("topleft",lwd=c(2,2,2),pch=c(1,2,3),col=c("blue","orange","green"),ncol=1,
       legend=c("Prob. Mandante vencer","Prob. Visitante vencer ", "Prob. Empate"),bty="n")
}
savePlot(filename="artigo_grafico2", type="png", device=dev.cur())

#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#       ANALISES ANEXO
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
t = jogos

#VARIANCIA MÁXIMA 
times = unique(jogos$MANDANTE)
timesA = times[1:10]
timesB = times[11:20]
jogos$dif = rep(100, dim(jogos)[1])
for(i in 1:dim(jogos)[1]){
	if(length(subset(timesA, timesA==jogos$MANDANTE[i]))!=0 & length(subset(timesA, timesA==jogos$VISITANTE[i]))!=0){
		if(jogos$MANDANTE[i]<jogos$VISITANTE[i]){
			jogos$dif[i] = 1
		}else{
			jogos$dif[i] = -1 
		} 
	}
	if(length(subset(timesA, timesA==jogos$MANDANTE[i]))!=0 & length(subset(timesA, timesA==jogos$VISITANTE[i]))==0){
		jogos$dif[i] = 1 }
	if(length(subset(timesA, timesA==jogos$MANDANTE[i]))==0 & length(subset(timesA, timesA==jogos$VISITANTE[i]))!=0){
		jogos$dif[i] = -1 }
	if(length(subset(timesA, timesA==jogos$MANDANTE[i])) ==0 & length(subset(timesA, timesA==jogos$VISITANTE[i]))==0){
		jogos$dif[i] = 0 }
}
pontuacao_final = data.frame(equipes = times, pontos=rep(0,length(times)))
for(i in 1:dim(jogos)[1]){
	if(jogos$dif[i]>0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +3
	if(jogos$dif[i]<0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +3
	if(jogos$dif[i]==0){
		 pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +1
		 pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +1
	}
}


#380 partidas #20 equipes #38 jogos por equipes
# máximo quando tem A e B grupos, A sempre vence B e os jogos entre A e B são empates
# grupo A: 20 jogos ganhos e 18 empates
# grupo B: 18 empates
partidas = 380
nequipes = 20
partidas.nequipes = partidas/nequipes
if(partidas.nequipes%%2==0) var.max = (c(rep(((partidas.nequipes/2)-1)*2*1,nequipes/2),rep(((partidas.nequipes/2)-1)*2*1+((partidas.nequipes/2)+1)*2*3,nequipes/2)))
if(partidas.nequipes%%2!=0) var.max = (c(rep(((partidas.nequipes/2)-0.5)*2*1,nequipes/2),rep(((partidas.nequipes/2)-0.5)*2*1+((partidas.nequipes/2)+0.5)*2*3,nequipes/2)))
sd( var.max);30.77




#VARIANCIA MÁXIMA 
times = unique(jogos$MANDANTE)
jogos$dif = rep(100, dim(jogos)[1])
for(i in 1:dim(jogos)[1]){
		if(jogos$MANDANTE[i]<jogos$VISITANTE[i]){
			jogos$dif[i] = 1
		}else{
			jogos$dif[i] = -1 
		} 
	}
pontuacao_final = data.frame(equipes = times, pontos=rep(0,length(times)))
for(i in 1:dim(jogos)[1]){
	if(jogos$dif[i]>0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +3
	if(jogos$dif[i]<0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +3
	if(jogos$dif[i]==0){
		 pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +1
		 pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +1
	}
}
sd( pontuacao_final$pontos)
 a = c(rep(18,10),seq(from=114-54,to=114,by=6))



d = c(rep(16,9),seq(from=114-10*6,to=114,by=6))
e = seq(from=114-6*19,to=114,by=6)
f = c(rep(18,10),rep(18+60,10))

plot(f+rnorm(20,0,0.9),pch=18,col="red",ylim=c(0,116))
abline(h=mean(f),col="red")
points(e+rnorm(20,0,0.9),pch=18,col="green")
abline(h=mean(e),col="green")
points(d+rnorm(20,0,0.9),pch=18,col="blue")
abline(h=mean(d),col="blue")
legend("topleft",pch=18,col=c("red","green", "blue"), legend=c(sd(f),sd(e),sd(d)),bty="n")

savePlot(filename="variancia_maxima", type="png", device=dev.cur())










#VARIANCIA MINIMA 
funcao.sd<-function(jogos){
times = unique(jogos$MANDANTE)
jogos$dif = rep(100, dim(jogos)[1])
pontuacao_final = data.frame(equipes = times, pontos=rep(0,length(times)))
for(i in 1:dim(jogos)[1]){
	if(jogos$dif[i]>0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +3
	if(jogos$dif[i]<0) pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +3
	if(jogos$dif[i]==0){
		 pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$MANDANTE[i]] +1
		 pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] = pontuacao_final$pontos[pontuacao_final$equipes==jogos$VISITANTE[i]] +1
	}
}
#380 partidas #20 equipes #38 jogos por equipes
# minima quando 1) todos os mandantes vencem, 2)todos os visitantes vencem, 3) empate
partidas = 380
nequipes = 20
partidas.nequipes = partidas/nequipes
var.min = rep(partidas.nequipes*2*1,nequipes)# todas empatam
if(partidas.nequipes%%2==0) var.max = (c(rep(((partidas.nequipes/2)-1)*2*1,nequipes/2),rep(((partidas.nequipes/2)-1)*2*1+((partidas.nequipes/2)+1)*2*3,nequipes/2)))
if(partidas.nequipes%%2!=0) var.max = (c(rep(((partidas.nequipes/2)-0.5)*2*1,nequipes/2),rep(((partidas.nequipes/2)-0.5)*2*1+((partidas.nequipes/2)+0.5)*2*3,nequipes/2)))









