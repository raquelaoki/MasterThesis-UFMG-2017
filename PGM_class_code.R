#############################################################################################
## RAQUEL AOKI
## TRABALHO FINAL PGM
#############################################################################################

require(XML)
u = "http://www.tabeladobrasileirao.net/2014/"
tables = readHTMLTable(u, as.data.frame = TRUE)
jogos = tables[[1]]

names(jogos) = c("RODADA","DATA","DIA","HORA","MANDANTE",  "GOL.MAND" ,"x","GOL.VIS",
			"VISITANTE" ,"ESTÃDIO", "CIDADE","TV") 
jogos = subset(jogos, select=c(RODADA  ,DATA, MANDANTE, VISITANTE, GOL.MAND, GOL.VIS))
jogos$g = c(1:dim(jogos)[1])
times = unique(c(as.character(jogos$MANDANTE), as.character(jogos$VISITANTE)))
times = data.frame(times, times)
names(times) = c("time", "cod")
times$cod = as.numeric(times$cod)

time.h = times
time.a = times 
rm(times)
names(time.a) = c("time", "a")
names(time.h) = c("time", "h")

jogos = merge(jogos, time.a, by.x="VISITANTE", by.y="time", all.x=T)
jogos = merge(jogos, time.h, by.x="MANDANTE", by.y="time", all.x=T)

jogos1 = subset(jogos, select = c(g, MANDANTE, VISITANTE, h,a,  GOL.MAND, GOL.VIS))
names(jogos1 ) = c(names(jogos1)[-c(6,7)],"y1","y2")
jogos1 =jogos1[order(jogos1$g),]
rownames(jogos1) =NULL

#write.table(jogos1, "jogos_brasileirao_2014.csv",row.names=F, sep=";") 
#write.table(jogos1, "jogos_brasileirao_2014.txt",row.names=F, sep=";")

#############################################################################################
## 				DADOS JA COLETADOS, COMEÇAR AQUI
#############################################################################################

jogos = read.table("jogos_brasileirao_2014.txt", sep=";", header=T)

#### analise descritiva 
y = jogos$y1 + jogos$y2 

summary(y)
summary(jogos$y1)
summary(jogos$y2)

gols.mandante = table(jogos$MANDANTE, jogos$y1)
gols.visitante = table(jogos$VISITANTE, jogos$y2)
times1 = rownames(gols.mandante);rownames(gols.mandante)=NULL
times2 = rownames(gols.visitante);rownames(gols.visitante)=NULL
gols1 = colnames(gols.mandante)
gols2 = colnames(gols.visitante)

gols.mandante = data.frame(cbind(times1, gols.mandante))
gols.visitante= data.frame(cbind(times1, gols.visitante))
names(gols.mandante ) = c("equipe", gols1)
names(gols.visitante) = c("equipe", gols2)


media.mandante = data.frame(tapply(jogos$y1,as.character(jogos$MANDANTE), mean))
media.visitante= data.frame(tapply(jogos$y2,as.character(jogos$VISITANTE), mean))
times1 = rownames(media.mandante); rownames(media.mandante)=NULL
times2 = rownames(media.visitante); rownames(media.visitante)=NULL
media.mandante = cbind(times1, media.mandante)
media.visitante = cbind(times2, media.visitante)
names(media.mandante) =c("equipe","media.m")
names(media.visitante) =c("equipe","media.v")
media = merge(media.mandante, media.visitante,by.y ="equipe",by.x="equipe", all=T)

t.test(media$media.m, media$media.v, paired=T)

#############################################################################################
##   			MODELO 1 -BASIC MODEL FOR THE HYPERPARAMETERS
#############################################################################################
rm(list = ls( all = T))
jogos = read.table("jogos_brasileirao_2014.txt", sep=";", header=T)

times = unique(c(as.character(jogos$MANDANTE), as.character(jogos$VISITANTE)))
att=rep(0, length(times))
def=rep(0, length(times))
att.star=rep(0, length(times))
def.star=rep(0, length(times))
times = data.frame(times, att, def,att.star,def.star)


# Trick to code the ‘‘sum-to-zero’’ constraint
for (t in 1:dim(times)[1]){
	# priors on the random effects
	mu.att = rnorm(1,0,0.0001)
	mu.def = rnorm(1,0,0.0001)
	tau.att = rgamma(1,.01,.01)
	tau.def = rgamma(1,.01,.01)
	times$att.star[t] = rnorm(1,mu.att,tau.att)
	times$def.star[t] = rnorm(1,mu.def,tau.def)
	times$att[t] = times$att.star[t] - mean(times$att.star)
	times$def[t] = times$def.star[t] - mean(times$def.star)
}

# prior on the home effect
home = rnorm(1,0,0.0001)

#--- teste funcao 
require(hSDM)
theta = matrix(rep(0,2*dim(jogos)[1]),ncol=2)
for (g in 1:dim(jogos)[1]){
	theta[g,1] = exp(home+times$att[times==as.character(jogos$MANDANTE[g])]+
				times$def[times==as.character(jogos$VISITANTE[g])])
	theta[g,2] = exp(times$def[times==as.character(jogos$MANDANTE[g])]+
				times$att[times==as.character(jogos$VISITANTE[g])])
	
}

bd1 = data.frame(jogos$y1, theta[,1]); names(bd1) = c("gols.mandante", "theta")
bd2 = data.frame(jogos$y2, theta[,2]); names(bd2) = c("gols.visitante", "theta")

mod1 <- hSDM.poisson(counts=bd1$gols.mandante, suitability=~theta,
	data=bd1, suitability.pred=NULL, burnin=1000, mcmc=1000, thin=1,
	beta.start=0,mubeta=0, Vbeta=1,seed=1234, verbose=1,save.p=0)

mod2 <- hSDM.poisson(counts=bd2$gols.visitante, suitability=~theta,
	data=bd2, suitability.pred=NULL, burnin=1000, mcmc=1000, thin=1,
	beta.start=0,mubeta=1, Vbeta=1,seed=1234, verbose=1,save.p=0)

summary(mod1$lambda.pred)
summary(mod2$lambda.pred)

bd1.aux = data.frame(jogos$y1, theta[,1]); names(bd1.aux) = c("gols", "theta")
bd2.aux = data.frame(jogos$y2, theta[,2]); names(bd2.aux) = c("gols", "theta")
bd3 = rbind(bd1.aux, bd2.aux) 
mod3 <- hSDM.poisson(counts=bd3$gols, suitability=~theta,
	data=bd3, suitability.pred=NULL, burnin=1000, mcmc=1000, thin=1,
	beta.start=0,mubeta=0, seed=13, verbose=1,save.p=0)
summary(mod3$lambda.pred[1:dim(bd1.aux)[1]])
summary(mod3$lambda.pred[dim(bd1.aux)[1]:(dim(bd1.aux)[1]+dim(bd2.aux)[1])])

mod1.gols.mandante = mod3$lambda.pred[1:dim(bd1.aux)[1]]
mod1.gols.visitante= mod3$lambda.pred[(1+dim(bd1.aux)[1]):(dim(bd1.aux)[1]+dim(bd2.aux)[1])]


sd(mod1$lambda.pred);sd(mod2$lambda.pred)

#############################################################################################
##   			MODELO 2 - MIXTURE MODEL FOR THE HYPERPARAMETERS
#############################################################################################

jogos = read.table("jogos_brasileirao_2014.txt", sep=";", header=T)

times2 = unique(c(as.character(jogos$MANDANTE), as.character(jogos$VISITANTE)))
att=rep(0, length(times2))
def=rep(0, length(times2))
att.star=rep(0, length(times2))
def.star=rep(0, length(times2))
times2 = data.frame(times2, att, def,att.star,def.star)

p.att = matrix(c(rep(0,3*dim(times2)[1])),ncol=3)
p.def= matrix(c(rep(0,3*dim(times2)[1])),ncol=3)
grp.att = rep(0, dim(times2)[1])
grp.def = rep(0, dim(times2)[1])


require(gtools) #para a dirichilet 
require(truncnorm) # normal truncada
require(mixAK) # student t outra parametricazao 

mu.att = c( rtruncnorm(1,mean=0,sd=0.001,a=-3,b=0),0, rtruncnorm(1,mean=0,sd=0.001,a=0,b=3))
tau.att= rgamma(3,0.1, 0.1)
mu.def = c( rtruncnorm(1,mean=0,sd=0.001,a=0,b=3),0,rtruncnorm(1,mean=0,sd=0.001,a=-3,b=0))
tau.def =  rgamma(3,0.1,0.1)

# Trick to code the ‘‘sum-to-zero’’ constraint
for (t in 1:dim(times2)[1]){
	p.att[t,1:3] = rdirichlet(1, alpha=c(1,1,1))
	p.def[t,1:3] = rdirichlet(1, alpha=c(1,1,1))

	grp.att[t] = sample(c(1,2,3),size=1,replace=T,prob=p.att[t,])
	grp.def[t] = sample(c(1,2,3),size=1,replace=T,prob=p.def[t,])
	
	times2$att.star[t] = rMVT(1, df=4, mu=mu.att[grp.att[t]],Sigma=tau.att[grp.att[t]])$x #dt(4)
	times2$def.star[t] = rMVT(1, df=4, mu=mu.def[grp.def[t]],Sigma=tau.def[grp.def[t]])$x
	times2$att[t] = times2$att.star[t] - mean(times2$att.star)
	times2$def[t] = times2$def.star[t] - mean(times2$def.star)
}

# prior on the home effect
home = rnorm(1,0,0.0001)

#--- teste funcao 
theta.m = matrix(rep(0,2*dim(jogos)[1]),ncol=2)
for (g in 1:dim(jogos)[1]){
	theta.m[g,1] = exp(home+times2$att[times2$times2==as.character(jogos$MANDANTE[g])]+
					 times2$def[times2$times2==as.character(jogos$VISITANTE[g])])
	theta.m[g,2] = exp(times2$def[times2$times2==as.character(jogos$MANDANTE[g])]+
				  times2$att[times2$times2==as.character(jogos$VISITANTE[g])])
	
}

bd1.m = data.frame(jogos$y1, theta.m[,1]); names(bd1.m) = c("gols.mandante", "theta")
bd2.m = data.frame(jogos$y2, theta.m[,2]); names(bd2.m) = c("gols.visitante", "theta")

mod1.m <- hSDM.poisson(counts=bd1.m$gols.mandante, suitability=~theta,
	data=bd1.m, suitability.pred=NULL, burnin=1000, mcmc=1000, thin=1,
	beta.start=0,mubeta=0, Vbeta=1.0E6,seed=1234, verbose=1,save.p=0)

mod2.m <- hSDM.poisson(counts=bd2.m$gols.visitante, suitability=~theta,
	data=bd2.m, suitability.pred=NULL, burnin=1000, mcmc=1000, thin=1,
	beta.start=0,mubeta=0, Vbeta=1.0E6,seed=1234, verbose=1,save.p=0)

summary(mod1.m$lambda.pred)
summary(mod2.m$lambda.pred)

bd1.aux.m = data.frame(jogos$y1, theta.m[,1]); names(bd1.aux.m) = c("gols", "theta")
bd2.aux.m = data.frame(jogos$y2, theta.m[,2]); names(bd2.aux.m) = c("gols", "theta")
bd3.m = rbind(bd1.aux.m, bd2.aux.m) 
mod3.m <- hSDM.poisson(counts=bd3.m$gols, suitability=~theta,
	data=bd3.m, suitability.pred=NULL, burnin=1000, mcmc=1000, thin=1,
	beta.start=0,mubeta=0, seed=12, verbose=1,save.p=0)
summary(mod3.m$lambda.pred[1:dim(bd1.aux.m)[1]])
summary(mod3.m$lambda.pred[dim(bd1.aux.m)[1]:(dim(bd1.aux.m)[1]+dim(bd2.aux.m)[1])])

mod1.gols.mandante.m = mod3.m$lambda.pred[1:dim(bd1.aux.m)[1]]
mod1.gols.visitante.m= mod3.m$lambda.pred[(1+dim(bd1.aux.m)[1]):(dim(bd1.aux.m)[1]+dim(bd2.aux.m)[1])]




#############################################################################################
##   			COMPARAÇÃO DOS RESULTADOS
#############################################################################################

##-- Observado
pts.obs = data.frame(times$times, pts = rep(0,dim(times)[1]), vit = rep(0,dim(times)[1]),der = rep(0,dim(times)[1]),emp= rep(0,dim(times)[1]))
names(pts.obs) = c("time", "pts","vit","der", "emp")

for(i in 1:dim(jogos)[1]){
	if(jogos$y1[i]>jogos$y2[i]){
		pts.obs$pts[pts.obs$time==jogos$MANDANTE[i]]=pts.obs$pts[pts.obs$time==jogos$MANDANTE[i]]+3
		pts.obs$vit[pts.obs$time==jogos$MANDANTE[i]]=pts.obs$vit[pts.obs$time==jogos$MANDANTE[i]]+1
		pts.obs$der[pts.obs$time==jogos$VISITANTE[i]]=pts.obs$der[pts.obs$time==jogos$VISITANTE[i]]+1
	}
	if(jogos$y1[i]<jogos$y2[i]){
		pts.obs$pts[pts.obs$time==jogos$VISITANTE[i]]=pts.obs$pts[pts.obs$time==jogos$VISITANTE[i]]+3
		pts.obs$der[pts.obs$time==jogos$MANDANTE[i]]=pts.obs$der[pts.obs$time==jogos$MANDANTE[i]]+1
		pts.obs$vit[pts.obs$time==jogos$VISITANTE[i]]=pts.obs$vit[pts.obs$time==jogos$VISITANTE[i]]+1

	}
	if(jogos$y1[i]==jogos$y2[i]){
		pts.obs$pts[pts.obs$time==jogos$MANDANTE[i]]=pts.obs$pts[pts.obs$time==jogos$MANDANTE[i]]+1
		pts.obs$pts[pts.obs$time==jogos$VISITANTE[i]]=pts.obs$pts[pts.obs$time==jogos$VISITANTE[i]]+1
		pts.obs$emp[pts.obs$time==jogos$MANDANTE[i]]=pts.obs$emp[pts.obs$time==jogos$MANDANTE[i]]+1
		pts.obs$emp[pts.obs$time==jogos$VISITANTE[i]]=pts.obs$emp[pts.obs$time==jogos$VISITANTE[i]]+1
	}
}
##-- Simulação 1
#simulado = data.frame(jogos$MANDANTE, jogos$VISITANTE, mod1$lambda.pred,mod2$lambda.pred)
simulado = data.frame(jogos$MANDANTE, jogos$VISITANTE,mod1.gols.mandante,mod1.gols.visitante) 
names(simulado) = c("MANDANTE", "VISITANTE", "y1","y2")
pts.sim = data.frame(times$times, pts = rep(0,dim(times)[1]), vit = rep(0,dim(times)[1]),der = rep(0,dim(times)[1]),emp= rep(0,dim(times)[1]))
names(pts.sim) = c("time", "pts","vit","der", "emp")

for(i in 1:dim(simulado)[1]){
	if(simulado$y1[i]>simulado$y2[i]){
		pts.sim$pts[pts.sim$time==simulado$MANDANTE[i]]=pts.sim$pts[pts.sim$time==simulado$MANDANTE[i]]+3
		pts.sim$vit[pts.sim$time==simulado$MANDANTE[i]]=pts.sim$vit[pts.sim$time==simulado$MANDANTE[i]]+1
		pts.sim$der[pts.sim$time==simulado$VISITANTE[i]]=pts.sim$der[pts.sim$time==simulado$VISITANTE[i]]+1
	}
	if(simulado$y1[i]<simulado$y2[i]){
		pts.sim$pts[pts.sim$time==simulado$VISITANTE[i]]=pts.sim$pts[pts.sim$time==simulado$VISITANTE[i]]+3
		pts.sim$der[pts.sim$time==simulado$MANDANTE[i]]=pts.sim$der[pts.sim$time==simulado$MANDANTE[i]]+1
		pts.sim$vit[pts.sim$time==simulado$VISITANTE[i]]=pts.sim$vit[pts.sim$time==simulado$VISITANTE[i]]+1

	}
	if(simulado$y1[i]==simulado$y2[i]){
		pts.sim$pts[pts.sim$time==simulado$MANDANTE[i]]=pts.sim$pts[pts.sim$time==simulado$MANDANTE[i]]+1
		pts.sim$pts[pts.sim$time==simulado$VISITANTE[i]]=pts.sim$pts[pts.sim$time==simulado$VISITANTE[i]]+1
		pts.sim$emp[pts.sim$time==simulado$MANDANTE[i]]=pts.sim$emp[pts.sim$time==simulado$MANDANTE[i]]+1
		pts.sim$emp[pts.sim$time==simulado$VISITANTE[i]]=pts.sim$emp[pts.sim$time==simulado$VISITANTE[i]]+1
	}
}

##-- Simulação 2
#simulado.m = data.frame(jogos$MANDANTE, jogos$VISITANTE, mod1.m$lambda.pred,mod2.m$lambda.pred)
simulado.m = data.frame(jogos$MANDANTE, jogos$VISITANTE,mod1.gols.mandante.m,mod1.gols.visitante.m) 
names(simulado.m) = c("MANDANTE", "VISITANTE", "y1","y2")
pts.sim.m = data.frame(times2$times, pts = rep(0,dim(times2)[1]), vit = rep(0,dim(times2)[1]),der = rep(0,dim(times2)[1]),emp= rep(0,dim(times2)[1]))
names(pts.sim.m) = c("time", "pts","vit","der", "emp")

for(i in 1:dim(simulado.m)[1]){
	if(simulado.m$y1[i]>simulado.m$y2[i]){
		pts.sim.m$pts[pts.sim.m$time==simulado.m$MANDANTE[i]]=pts.sim.m$pts[pts.sim.m$time==simulado.m$MANDANTE[i]]+3
		pts.sim.m$vit[pts.sim.m$time==simulado.m$MANDANTE[i]]=pts.sim.m$vit[pts.sim.m$time==simulado.m$MANDANTE[i]]+1
		pts.sim.m$der[pts.sim.m$time==simulado.m$VISITANTE[i]]=pts.sim.m$der[pts.sim.m$time==simulado.m$VISITANTE[i]]+1
	}
	if(simulado.m$y1[i]<simulado.m$y2[i]){
		pts.sim.m$pts[pts.sim.m$time==simulado.m$VISITANTE[i]]=pts.sim.m$pts[pts.sim.m$time==simulado.m$VISITANTE[i]]+3
		pts.sim.m$der[pts.sim.m$time==simulado.m$MANDANTE[i]]=pts.sim.m$der[pts.sim.m$time==simulado.m$MANDANTE[i]]+1
		pts.sim.m$vit[pts.sim.m$time==simulado.m$VISITANTE[i]]=pts.sim.m$vit[pts.sim.m$time==simulado.m$VISITANTE[i]]+1

	}
	if(simulado.m$y1[i]==simulado.m$y2[i]){
		pts.sim.m$pts[pts.sim.m$time==simulado.m$MANDANTE[i]]=pts.sim.m$pts[pts.sim.m$time==simulado.m$MANDANTE[i]]+1
		pts.sim.m$pts[pts.sim.m$time==simulado.m$VISITANTE[i]]=pts.sim.m$pts[pts.sim.m$time==simulado.m$VISITANTE[i]]+1
		pts.sim.m$emp[pts.sim.m$time==simulado.m$MANDANTE[i]]=pts.sim.m$emp[pts.sim.m$time==simulado.m$MANDANTE[i]]+1
		pts.sim.m$emp[pts.sim.m$time==simulado.m$VISITANTE[i]]=pts.sim.m$emp[pts.sim.m$time==simulado.m$VISITANTE[i]]+1
	}
}


pts.obs = pts.obs[order(pts.obs$pts,decreasing=T),]
pts.sim = pts.sim[order(pts.sim$pts,decreasing=T),]
pts.sim.m = pts.sim.m[order(pts.sim.m$pts,decreasing=T),]

pts.obs
pts.sim
pts.sim.m

sum(pts.obs$time==pts.sim$time)
sum(pts.obs$time==pts.sim.m$time)

a1 = data.frame(pts.obs$time, col = c(1:20)); names(a1) = c("observado", "col")
a2 = data.frame(pts.sim$time, col = c(1:20)); names(a2) = c("simulado", "col")
a3 = data.frame(pts.sim.m$time, col = c(1:20)); names(a3) = c("simulado.m", "col")

par(mfrow=c(1,2))
a4 = merge(a1, a2, by.x = "observado",by.y = "simulado",all=T)
a4[order(a4$col.x),]
plot(a4$col.x ~a4$col.y)
lines(lowess(a4$col.x,a4$col.y), col = 2)
abline(h=c(5,10,15),col = "lightgray", lty = 3)
abline(v=c(5,10,15),col = "lightgray", lty = 3)


a5 = merge(a1, a3, by.x = "observado",by.y = "simulado.m",all=T)
a5[order(a5$col.x),]
plot(a5$col.x ~a5$col.y)
lines(lowess(a5$col.x,a5$col.y), col = 2)
abline(h=c(5,10,15),col = "lightgray", lty = 3)
abline(v=c(5,10,15),col = "lightgray", lty = 3)
