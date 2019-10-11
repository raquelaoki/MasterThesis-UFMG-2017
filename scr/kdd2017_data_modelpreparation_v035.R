#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#
#		criando as features do pedro 
#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------#


rm(list = ls( all = T))
setwd("C://Users//Raquel Aoki//Google Drive//UFMG//Pesquisa//V035//Modelo")
equipes = read.table("equipes_legendas.csv", header=T, sep=";")

#packages
require(igraph)
require(xtable)
require(plotrix)


#fazendo grafo
anos = 2012:2016


network_effects<-function(anos,i){
#------#------#------# 1) CALCULANDO O TEAM VOLABILITY 
#------ INTERVALO DE DOIS ANOS, E CADA ANO CONSIDERA AS 6 TEMPORADAS ANTERIORES 
#------ Selecionando o grafo para (anos-2)
#lendo as bases de dados
anos.anteriores = (anos[i]-8):(anos[i]-3)
bd1 = read.table(paste("salarios_nba_",anos.anteriores[1],".csv",sep=""),sep=";",header=T)
bd2 = read.table(paste("salarios_nba_",anos.anteriores[2],".csv",sep=""),sep=";",header=T)
bd3 = read.table(paste("salarios_nba_",anos.anteriores[3],".csv",sep=""),sep=";",header=T)
bd4 = read.table(paste("salarios_nba_",anos.anteriores[4],".csv",sep=""),sep=";",header=T)
bd5 = read.table(paste("salarios_nba_",anos.anteriores[5],".csv",sep=""),sep=";",header=T)
bd6 = read.table(paste("salarios_nba_",anos.anteriores[6],".csv",sep=""),sep=";",header=T)

names(bd1)=names(bd2)=names(bd3)=names(bd4)=names(bd5)=names(bd6) = c("Rk","Player", "Salary","team")

# ordenando 
bd1 = bd1[order(bd1$Player),]
bd2 = bd2[order(bd2$Player),]
bd3 = bd3[order(bd3$Player),]
bd4 = bd4[order(bd4$Player),]
bd5 = bd5[order(bd5$Player),]
bd6 = bd6[order(bd6$Player),]

#criando um vetor com as setas do grafo 
setas1 = c();setas2 = c()
setas3 = c();setas4 = c()
setas5 = c();setas6 = c()

for(j in 1:length(unique(bd1$team))){
	aux = as.character(subset(bd1, team==unique(bd1$team)[j])$Player)
	aux = c(aux, as.character(unique(bd1$team)[j]))
	setas1 = c(setas1,as.character(unlist(data.frame(combn(aux,m=2)))))
}
for(j in 1:length(unique(bd2$team))){
	aux = as.character(subset(bd2, team==unique(bd2$team)[j])$Player)
	aux = c(aux, as.character(unique(bd2$team)[j]))
	setas2 = c(setas2,as.character(unlist(data.frame(combn(aux,m=2)))))
}
for(j in 1:length(unique(bd3$team))){
	aux = as.character(subset(bd3, team==unique(bd3$team)[j])$Player)
	aux = c(aux, as.character(unique(bd3$team)[j]))
	setas3 = c(setas3,as.character(unlist(data.frame(combn(aux,m=2)))))
}
for(j in 1:length(unique(bd4$team))){
	aux = as.character(subset(bd4, team==unique(bd4$team)[j])$Player)
	aux = c(aux, as.character(unique(bd4$team)[j]))
	setas4 = c(setas4,as.character(unlist(data.frame(combn(aux,m=2)))))
}
for(j in 1:length(unique(bd5$team))){
	aux = as.character(subset(bd5, team==unique(bd5$team)[j])$Player)
	aux = c(aux, as.character(unique(bd5$team)[j]))
	setas5 = c(setas5,as.character(unlist(data.frame(combn(aux,m=2)))))
}
for(j in 1:length(unique(bd6$team))){
	aux = as.character(subset(bd6, team==unique(bd6$team)[j])$Player)
	aux = c(aux, as.character(unique(bd6$team)[j]))
	setas6 = c(setas6,as.character(unlist(data.frame(combn(aux,m=2)))))
}
#unindo todos os anos
setas.aux = c(setas1,setas2,setas3,setas4,setas5,setas6)
setas.aux = data.frame(matrix(setas.aux, ncol=2, byrow=T))
setas.2anos.ant.aux = setas.aux
setas.aux = setas.aux[order(setas.aux$X2),]
setas.aux = setas.aux[order(setas.aux$X1),]
setas.2anos.ant = unique(setas.aux )
setas.2anos.ant = as.character(unlist(t(setas.2anos.ant)))

#calculando o grafo para o ano corrente (igual o anterior)
anos.anteriores = (anos[i]-6):(anos[i]-1)
bd1 = read.table(paste("salarios_nba_",anos.anteriores[1],".csv",sep=""),sep=";",header=T)
bd2 = read.table(paste("salarios_nba_",anos.anteriores[2],".csv",sep=""),sep=";",header=T)
bd3 = read.table(paste("salarios_nba_",anos.anteriores[3],".csv",sep=""),sep=";",header=T)
bd4 = read.table(paste("salarios_nba_",anos.anteriores[4],".csv",sep=""),sep=";",header=T)
bd5 = read.table(paste("salarios_nba_",anos.anteriores[5],".csv",sep=""),sep=";",header=T)
bd6 = read.table(paste("salarios_nba_",anos.anteriores[6],".csv",sep=""),sep=";",header=T)

names(bd1)=names(bd2)=names(bd3)=names(bd4)=names(bd5)=names(bd6) = c("Rk","Player", "Salary","team")

bd1 = bd1[order(bd1$Player),]
bd2 = bd2[order(bd2$Player),]
bd3 = bd3[order(bd3$Player),]
bd4 = bd4[order(bd4$Player),]
bd5 = bd5[order(bd5$Player),]
bd6 = bd6[order(bd6$Player),]

setas1 = c();setas2 = c()
setas3 = c();setas4 = c()
setas5 = c();setas6 = c()

for(j in 1:length(unique(bd1$team))){
	aux = as.character(subset(bd1, team==unique(bd1$team)[j])$Player)
	aux = c(aux, as.character(unique(bd1$team)[j]))
	setas1 = c(setas1,as.character(unlist(data.frame(combn(aux,m=2)))))
}
for(j in 1:length(unique(bd2$team))){
	aux = as.character(subset(bd2, team==unique(bd2$team)[j])$Player)
	aux = c(aux, as.character(unique(bd2$team)[j]))
	setas2 = c(setas2,as.character(unlist(data.frame(combn(aux,m=2)))))
}
for(j in 1:length(unique(bd3$team))){
	aux = as.character(subset(bd3, team==unique(bd3$team)[j])$Player)
	aux = c(aux, as.character(unique(bd3$team)[j]))
	setas3 = c(setas3,as.character(unlist(data.frame(combn(aux,m=2)))))
}
for(j in 1:length(unique(bd4$team))){
	aux = as.character(subset(bd4, team==unique(bd4$team)[j])$Player)
	aux = c(aux, as.character(unique(bd4$team)[j]))
	setas4 = c(setas4,as.character(unlist(data.frame(combn(aux,m=2)))))
}
for(j in 1:length(unique(bd5$team))){
	aux = as.character(subset(bd5, team==unique(bd5$team)[j])$Player)
	aux = c(aux, as.character(unique(bd5$team)[j]))
	setas5 = c(setas5,as.character(unlist(data.frame(combn(aux,m=2)))))
}
for(j in 1:length(unique(bd6$team))){
	aux = as.character(subset(bd6, team==unique(bd6$team)[j])$Player)
	aux = c(aux, as.character(unique(bd6$team)[j]))
	setas6 = c(setas6,as.character(unlist(data.frame(combn(aux,m=2)))))
}

setas.aux = c(setas1,setas2,setas3,setas4,setas5,setas6)
setas.aux = data.frame(matrix(setas.aux, ncol=2, byrow=T))
setas.ano.corrente.aux = setas.aux
setas.aux = setas.aux[order(setas.aux$X2),]
setas.aux = setas.aux[order(setas.aux$X1),]
setas.ano.corrente = unique(setas.aux )
setas.ano.corrente = as.character(unlist(t(setas.ano.corrente)))


#bases do ano corrente e de 2 anos antes
bd.cor = read.table(paste("salarios_nba_",anos[i],".csv",sep=""),sep=";",header=T)[,-c(1,3)]
bd.ant = read.table(paste("salarios_nba_",(anos[i]-2),".csv",sep=""),sep=";",header=T)[,-c(1,3)]

aux.cor = data.frame(Player=data.frame(table(bd.cor$Team))$Var1,
	Team = data.frame(table(bd.cor$Team))$Var1)
aux.ant = data.frame(Player=data.frame(table(bd.ant$Team))$Var1,
	Team = data.frame(table(bd.ant$Team))$Var1)

bd.cor = rbind(bd.cor, aux.cor)
bd.ant = rbind(bd.ant, aux.ant)
names(bd.ant)=names(bd.cor) = c("Player", "Team")


#alocando os jogadores do grafo para os times deles 
setas.2anos.ant.aux = merge(setas.2anos.ant.aux, bd.ant, by.x="X1", by.y="Player",all.x=T)
setas.2anos.ant.aux = merge(setas.2anos.ant.aux, bd.ant, by.x="X2", by.y="Player",all.x=T)
setas.2anos.ant.aux  = subset(setas.2anos.ant.aux , !is.na(Team.x))
setas.2anos.ant.aux  = subset(setas.2anos.ant.aux , !is.na(Team.y))

setas.ano.corrente.aux = merge(setas.ano.corrente.aux, bd.cor, by.x="X1", by.y="Player",all.x=T)
setas.ano.corrente.aux = merge(setas.ano.corrente.aux, bd.cor, by.x="X2", by.y="Player",all.x=T)
setas.ano.corrente.aux = subset(setas.ano.corrente.aux, !is.na(Team.x))
setas.ano.corrente.aux = subset(setas.ano.corrente.aux, !is.na(Team.y))

#calculando a soma dos graus de cada time 
bd.and.d1  = subset(setas.2anos.ant.aux  , as.character(X2)==as.character(Team.y), select=c(X2,Team.y))
bd.and.d2  = subset(setas.2anos.ant.aux  , as.character(X1)==as.character(Team.x), select=c(X1,Team.x))
names(bd.and.d1)=names(bd.and.d2)=c("Player", "Team")
bd.and.d = rbind(bd.and.d1, bd.and.d2);rm(bd.and.d1, bd.and.d2)
bd.and.d = data.frame(table(bd.and.d$Team))

bd.cor.d1  = subset(setas.ano.corrente.aux, as.character(X2)==as.character(Team.y), select=c(X2,Team.y))
bd.cor.d2  = subset(setas.ano.corrente.aux, as.character(X1)==as.character(Team.x), select=c(X1,Team.x))
names(bd.cor.d1)=names(bd.cor.d2)=c("Player", "Team")
bd.cor.d = rbind(bd.cor.d1, bd.cor.d2);rm(bd.cor.d1, bd.cor.d2)
bd.cor.d = data.frame(table(bd.cor.d$Team))


#team volabilitY;
team.volability = merge(bd.cor.d, bd.and.d, all=T, by="Var1")
team.volability$value = team.volability$Freq.x - team.volability$Freq.y 
team.volability = team.volability[,-c(2,3)]
names(team.volability)=c("team","team.vol")

#------#------#------# 2)  ROSTER AGGREGATE VOLABILITY 

bd1 = bd1[,-c(1,3)];names(bd1) = c("Player", "team1")
bd2 = bd2[,-c(1,3)];names(bd2) = c("Player", "team2")
bd3 = bd3[,-c(1,3)];names(bd3) = c("Player", "team3")
bd4 = bd4[,-c(1,3)];names(bd4) = c("Player", "team4")
bd5 = bd5[,-c(1,3)];names(bd5) = c("Player", "team5")
bd6 = bd6[,-c(1,3)];names(bd6) = c("Player", "team6")

#fazendo um arquivo com a historia pregressa de cada jogador 
players = merge(bd.cor, bd6, all.x=T, by="Player")
players = merge(players, bd5, all.x=T, by="Player")
players = merge(players, bd4, all.x=T, by="Player")
players = merge(players, bd3, all.x=T, by="Player")
players = merge(players, bd2, all.x=T, by="Player")
players = merge(players, bd1, all.x=T, by="Player")

# calculando quanto tempo o jogador esta no time 
p1 = subset(players , Team!=team6 |is.na(team6)) 
p2 = subset(players , Team==team6& (Team!=team5 | is.na(team5))  )
p3 = subset(players , Team==team6& Team==team5 & (Team!=team4| is.na(team4))  )
p4 = subset(players , Team==team6& Team==team5& Team==team4 & (Team!=team3| is.na(team3))  )
p5 = subset(players , Team==team6& Team==team5& Team==team4 & Team==team3 & (Team!=team2| is.na(team2))  )
p6 = subset(players , Team==team6& Team==team5& Team==team4 & Team==team3 & Team==team2 & (Team!=team1| is.na(team1)) )
p7 = subset(players , Team==team6& Team==team5& Team==team4 & Team==team3 & Team==team2 & Team==team1)

dim(players)[1] ==dim(p1)[1]+dim(p2)[1]+dim(p3)[1]+dim(p4)[1]+dim(p5)[1]+dim(p6)[1]+dim(p7)[1]

p1$y = rep(1, dim(p1)[1]) ;p1 = p1[,c(1,2,9)]
p2$y = rep(2, dim(p2)[1]) ;p2 = p2[,c(1,2,9)]
p3$y = rep(3, dim(p3)[1]) ;p3 = p3[,c(1,2,9)]
p4$y = rep(4, dim(p4)[1]) ;p4 = p4[,c(1,2,9)]
p5$y = rep(5, dim(p5)[1]) ;p5 = p5[,c(1,2,9)]
p6$y = rep(6, dim(p6)[1]) ;p6 = p6[,c(1,2,9)]
p7$y = rep(7, dim(p7)[1]) ;p7 = p7[,c(1,2,9)]

players = rbind(p1,p2,p3,p4,p5,p6,p7)
players.anos = unique(players )

#grau de cada jogador 
players.degree = (data.frame(table(as.character(setas.ano.corrente.aux[,1]))))

#retirando os nos das equipes
equipes = data.frame(table(bd.cor$Team)); names(equipes) = c("Team", "Delete")
players.degree = merge(players.degree, equipes, all.x=T, by.x="Var1", by.y="Team")
players.degree = subset(players.degree, is.na(Delete))
players.degree = players.degree[,-3]

#roster agregate volability 
roster.av = merge(players.anos, players.degree,by.x="Player",by.y="Var1",all=T)
roster.av$Freq[is.na(roster.av$Freq)]=0 #novos jogadores tem 0 grau
roster.av$value = roster.av$Freq/roster.av$y

roster.agreg.vol = data.frame(tapply(roster.av$value,roster.av$Team,sum))
team = row.names(roster.agreg.vol); row.names(roster.agreg.vol)=NULL
roster.agreg.vol = data.frame(team,roster.agreg.vol)
names(roster.agreg.vol) = c("team", "roster.agreg.vol")

#------#------#------# CLUSTERING COEFICIENTE

# data.frame com a conexao entre dois times e o atual time de cada
setas.aux = data.frame(matrix(setas.ano.corrente, ncol=2, byrow=T))
names(setas.aux) =c("player1","player2")
setas.aux = merge(setas.aux,bd.cor,by.x="player1",by.y="Player",all.x=T)
setas.aux = merge(setas.aux,bd.cor,by.x="player2",by.y="Player",all.x=T)
setas.aux = subset(setas.aux , Team.x==Team.y)

teams = unique(c(as.character(setas.aux$Team.x),as.character(setas.aux$Team.y)))
cc = c()
for(k in 1:length(teams)){
	setas = subset(setas.aux, Team.x==teams[k])[,-c(3,4)]
	setas  = as.character(unlist(t(setas )))
	grafo <- graph(setas)
	cc = c(cc,transitivity(grafo, type="global"))
}
cc[is.na(cc)]=0
cc= data.frame(cbind(teams,cc))
names(cc)=c("team","cc")
cc$cc= as.numeric(as.character(cc$cc))
#------#------#------# ROSTER AGGREGATE COHERENCE

media.anos = tapply(players.anos$y,players.anos$Team,mean)
cc = cc[order(cc$team),]
roster.agreg.coh=data.frame(cc$cc*media.anos)
teams = rownames(roster.agreg.coh);rownames(roster.agreg.coh)=NULL
roster.agreg.coh = data.frame(teams,roster.agreg.coh)
names(roster.agreg.coh)= c("team","roster.agreg.coh")

#------#------#------# TEAM SIZE
team.size = data.frame(table(bd.cor$Team))
names(team.size) = c("team","size")

resumo = merge(team.volability,roster.agreg.vol,all=T)
resumo = merge(resumo,cc,all=T)
resumo = merge(resumo,roster.agreg.coh,all=T)
resumo = merge(resumo,team.size,all=T)

#save as Network Effects
write.table(resumo,paste("resumo_network_effects_nba_new",anos[i],".csv",sep=""),row.names=F,sep=";")

}

for(i in 1:length(anos)) network_effects(anos,i)






