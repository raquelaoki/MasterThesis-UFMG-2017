########################################################################
###   		TESTE COM HABILIDADES REAIS
### RAQUEL AOKI ## DATA: 12/09/2016 
########################################################################
#--------#--------#--------#--------#--------#--------#--------#--------
#--------  PROCEDIMENTOS INICIAIS
#--------#--------#--------#--------#--------#--------#--------#--------
rm(list = ls( all = T))
setwd("C:\\Users\\Raquel Aoki\\Google Drive\\UFMG\\Pesquisa\\V035\\Modelo")
require(Rcpp)
require(far)
Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
Sys.setenv("PKG_LIBS"="-fopenmp")
sourceCpp("kdd2017_mcmc.cpp")

#--------#--------#--------#--------#--------#--------#--------#--------
#----- INICIO : 
#--------#--------#--------#--------#--------#--------#--------#--------

temporadas = c("1112","1213","1314","1415","1516") 
anos = 2012:2016 
N = 10000   #NUMERO DE ITERACOES 
sd = 0.15 #0.15
sd_ea = 3 #5
vpriori = 2 #

#-------- leitura da base de dados (so vou usar a combinacao dos jogos, nao o resultado)
playof = read.table("inicio_playoff.csv", header=T, sep=",")
#indice = 0

dic= c()

for(indice in 1:length(temporadas)){


#indice = indice+1

temp1 = temporadas[indice]

jogo = read.table(paste("jogos_basket_nba_eua_",temp1,".csv",sep=""),header=T,sep=";")
jogo = jogo[order(jogo$data2),]  
jogo$data2 = as.numeric(as.character(jogo$data2))
jogo=  subset(jogo, data2<playof$inicio_playof[playof$NBA==as.numeric(as.character(temp1))])

covariaveis.completo = read.table(paste("resumo",anos[indice],".csv",sep=""), header=T,sep=";")
covariaveis = subset(covariaveis.completo, select=c("Team", "conf", "posicao", "salary_mean", "salary_sd", 
	"salary_sd", "salary5_mean", "salary610_mean", "per_mean"))

covariaveis.completo2 = read.table(paste("resumo_network_effects_nba_new",anos[indice],".csv",sep=""), header=T,sep=";")
covariaveis = merge(covariaveis,covariaveis.completo2,by.x="Team",by.y="team",all=T)


for(kk in 4:(dim(covariaveis)[2])){
	covariaveis[,kk] = as.numeric(as.character(covariaveis[,kk]))
	covariaveis[,kk] = (covariaveis[,kk]-mean(covariaveis[,kk]))/sd(covariaveis[,kk])
}
covariaveis[,2] = as.character(covariaveis[,2])
covariaveis[,2][covariaveis[,2]=="E"]=0
covariaveis[,2][covariaveis[,2]=="W"]=1
covariaveis[,2] = as.numeric(as.character(covariaveis[,2]))
covariaveis = covariaveis[order(covariaveis$Team),]
covariaveis$ind = 0:(dim(covariaveis )[1]-1)

equipes = subset(covariaveis, select=c("Team", "conf", "posicao","ind"))
eq = equipes
names(eq)[1]="eq"

covariaveis = covariaveis[,c(1,15,2,7,9,10,13:14)]
nomes = names(covariaveis[3:dim(covariaveis)[2]])
names(covariaveis)= c("eq","ind","cov1", "cov2", "cov3", "cov4", "cov5","cov6")

#--------#--------#--------#--------#--------#--------#--------#--------
#----- colocando indices como nome das equipes
#--------#--------#--------#--------#--------#--------#--------#--------

#-------- selecionando somente as variaveis de interesse
jogo$N = jogo$score_x+jogo$score_y
jogo = subset(jogo, selec=c(home, visitor, N,score_x,score_y))
names(jogo) = c("time1.aux", "time2.aux", "N","score1","score2")
aux = eq[,c(1,4)];names(aux) = c("time1.aux", "time1")
jogo = merge(jogo,aux,by.x="time1.aux", by.y="time1.aux",all.x=T)
aux = eq[,c(1,4)];names(aux) = c("time2.aux", "time2")
jogo = merge(jogo,aux,by.x="time2.aux", by.y="time2.aux",all.x=T)
jogo0  = subset(jogo, selec=c(time1,time2, N,score1,score2))

#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----
#  METROPOLIS-HASTING
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----	
  
#--- semente (a habilidade que desejamos estimar correta)
nome.coef = c("intercept","cov1","cov2" ,"cov3", "cov4", "cov5","cov6","ea")
semente = c(rnorm(length(nome.coef),mean = 0 ,sd = sd))

covariaveis = covariaveis[order(covariaveis$eq),]

out = metropolis(N,vpriori, sd,sd_ea,jogo0$time1,jogo0$time2,jogo0$score1,jogo0$score2,
	semente,covariaveis$cov1, covariaveis$cov2, 
	covariaveis$cov3, covariaveis$cov4,covariaveis$cov5, 
	covariaveis$cov6)

mu = out$beta_corrente_vetor
coef = out$coef_corrente_vetor
t1 = colSums(out$contador_aux)
t2 = colSums(out$total_testes_aux)

write.table(mu, paste("v035_betas_completo",anos[indice],".csv",sep=""),sep=";", row.names=T)
write.table(coef, paste("v035_coef_completo",anos[indice],".csv",sep=""),sep=";", row.names=T)
write.table(t1/t2, paste("v035_aceitacao",anos[indice],".csv",sep=""),sep=";", row.names=T)


#------ Retirando o burn in
semente = mu[1,]
mu = mu[-c(1:2000),]
semente.coef = coef[1,]
coef = coef[-c(1:2000),]
#write.table(coef, paste("coef_serie_",anos[indice],".csv" ,sep=""),row.names=F, sep=";")
#plot(coef[,1],type="l")
 
#----- Número de vitórias
jogo$dif = jogo$score1-jogo$score2
aux1 = subset(jogo, dif>0)
aux2 = subset(jogo, dif<0)
aux1 = data.frame(table(as.character(aux1$time1.aux)))
aux2 = data.frame(table(as.character(aux2$time2.aux)))
t3 = merge(aux1,aux2,by = "Var1",all=T)
t3$vit = t3[,2]+t3[,3]
t3 = t3[,c(1,4)]
t3$Var1 = as.character(t3$Var1)
t3 = t3[order(t3$Var1),]

cor = subset(covariaveis.completo, select=c("Team","posicao"))
cor$cores =c()
cor$cores[cor[,2]<=4]="red"
cor$cores[cor[,2]==8]="orange"
cor$cores[cor[,2]==16]="green"
cor$cores[cor[,2]>16]="blue"
cor = cor[order(cor$Team),]

beta.final = colMeans(mu)
if(indice==1) {
	coef0 = colMeans(coef)
	habilidade = beta.final 
	skill = data.frame(t3,beta.final)
	names(skill) = c("team", paste("won",anos[indice],sep=""),paste("hab",anos[indice],sep=""))
	names(cor) = c("team", paste("pos",anos[indice],sep=""),paste("cor",anos[indice],sep=""))
	skill = merge(skill, cor, by.x="team",by.y="team",all=T)
}
if(indice!=1){
	 coef0 = rbind(coef0,colMeans(coef))
	 habilidade = rbind(habilidade )
	 skill0 = data.frame(t3,beta.final)
	 names(skill0) = c("team", paste("won",anos[indice],sep=""),paste("hab",anos[indice],sep=""))
	 names(cor) = c("team", paste("pos",anos[indice],sep=""),paste("cor",anos[indice],sep=""))
	 skill0 = merge(skill0, cor, by.x="team",by.y="team",all=T)
	 skill = merge(skill, skill0, by.x="team",by.y="team",all=T)
}

#CALCULANDO O DIC 
dbarra = c()
for(i in 1:dim(mu)[1]){
 mu1 = exp(mu[i,])
 ee1 = coef[i,dim(coef)[2]]
 jogo1.2 =  (ee1+jogo0$N*(mu1[jogo0$time1+1]/(mu1[jogo0$time1+1]+mu1[jogo0$time2+1])))
 jogo1.2[jogo1.2<=0]=0.0001 	
 jogo1.1 = jogo0$score1*log(jogo1.2)
 dbarra[i] = -2*(sum(jogo1.1)-sum(jogo1.2))	
}
dbarra1 = mean(dbarra)

mu2 = exp(colMeans(mu))
ee2 = mean(coef[,dim(coef)[2]])
jogo2.2 =  (ee2+jogo0$N*(mu2[jogo0$time1+1]/(mu2[jogo0$time1+1]+mu2[jogo0$time2+1])))
jogo2.2[jogo2.2 <=0]=0.0001
jogo2.1 = jogo0$score1*log(jogo2.2)
dbarra2 = -2*(sum(jogo2.1)-sum(jogo2.2))
dic[indice] = 2*dbarra1 - dbarra2
if(is.na(dic[indice] )) erro = c(dbarra1,dbarra2)
}

nomes
write.table(skill, "v035_skill.csv", row.names=F, sep=";")
write.table(coef0, "v035_coeficientes_estimados.csv",row.names=F,sep=";")


