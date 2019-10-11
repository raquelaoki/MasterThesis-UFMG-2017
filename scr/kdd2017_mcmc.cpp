#include <cstdlib>
#include <iostream>
#include <Rcpp.h>
#include <omp.h>
#include <math.h> 

using namespace std;
// [[Rcpp::export]]
Rcpp::List metropolis(float N, float vpriori,float sd, float sd_ea, Rcpp::NumericVector time1, Rcpp::NumericVector time2, 
	Rcpp::NumericVector score1, Rcpp::NumericVector score2, Rcpp::NumericVector coeficientes_semente,
	Rcpp::NumericVector coef_cov1,Rcpp::NumericVector coef_cov2, Rcpp::NumericVector coef_cov3, 
	Rcpp::NumericVector coef_cov4, Rcpp::NumericVector coef_cov5,Rcpp::NumericVector coef_cov6){

  /* INICIALIZANDO VARIAVEIS */
  // contadores  
  Rcpp::NumericVector contador_aux(coef_cov1.size()), total_testes_aux(coef_cov1.size());	
  // matrizes com resultado final
  Rcpp::NumericMatrix mu(N+1,coef_cov1.size());
  Rcpp::NumericMatrix coef(N+1,coeficientes_semente.size());
  Rcpp::NumericMatrix contador(N,coef_cov1.size());
  Rcpp::NumericMatrix total(N,coef_cov1.size());
 // variaveis auxiliar  
 Rcpp::NumericVector beta_corrente_vetor(coef_cov1.size()),coef_corrente_vetor(coeficientes_semente.size());
  int t1, t2, s1, nk;
  float intercept,cov1,cov2,cov3,cov4,cov5,cov6,ea;	
  float beta_correntei,beta_correntej,beta_propostoi,beta_propostoj;		
 float priori, vero, proposto,corrente,razao;	

// calculando o beta_corrente a partir da semente
 for(int i = 0; i<=beta_corrente_vetor.size()-1;i++){
	mu(0,i) = (coeficientes_semente[0]+coef_cov1[i]*coeficientes_semente[1]+coef_cov2[i]*coeficientes_semente[2]+
		coef_cov3[i]*coeficientes_semente[3]+ coef_cov4[i]*coeficientes_semente[4]+coef_cov5[i]*coeficientes_semente[5]+
		coef_cov6[i]*coeficientes_semente[6])/7; 
  }
// preenchendo a primeira linha da matriz coef com a semente
for(int k = 0; k<= coeficientes_semente.size()-1;k++) coef(0,k)=coeficientes_semente[k];
for(int k = 0; k<coef_cov1.size();k++) beta_corrente_vetor[k]= mu(0,k);
for(int k = 0; k<coeficientes_semente.size();k++) coef_corrente_vetor[k]= coef(0,k);	

float aux = 0;

for(int j = 0; j<N;j++){
	for(int k = 0; k<coef_cov1.size();k++) total_testes_aux[k]=0;
	for(int k = 0; k<coef_cov1.size();k++) contador_aux[k] = 0;   

	for(int i=0; i<=time1.size();i++){
			t1 = time1[i];t2 = time2[i];
      		s1 = score1[i]; nk = score1[i]+score2[i];
      		beta_correntei = beta_corrente_vetor[t1];
      		beta_correntej = beta_corrente_vetor[t2];
			intercept = Rcpp::rnorm(1,coef_corrente_vetor[0], sd)[0];	
      		cov1 = Rcpp::rnorm(1,coef_corrente_vetor[1], sd)[0];	
      		cov2 = Rcpp::rnorm(1,coef_corrente_vetor[2], sd)[0];
      		cov3 = Rcpp::rnorm(1,coef_corrente_vetor[3], sd)[0];	
      		cov4 = Rcpp::rnorm(1,coef_corrente_vetor[4], sd)[0];
      		cov5 = Rcpp::rnorm(1,coef_corrente_vetor[5], sd)[0];
      		cov6 = Rcpp::rnorm(1,coef_corrente_vetor[6], sd)[0];
      		ea =Rcpp::rnorm(1,coef_corrente_vetor[7], sd_ea)[0];
      		beta_propostoi = (intercept + coef_cov1[t1]*cov1+coef_cov2[t1]*cov2+coef_cov3[t1]*cov3+ coef_cov4[t1]*cov4 +coef_cov5[t1]*cov5+
				coef_cov6[t1]*cov6)/7; 
      		beta_propostoj = (intercept + coef_cov1[t2]*cov1+coef_cov2[t2]*cov2+coef_cov3[t2]*cov3+ coef_cov4[t2]*cov4 +coef_cov5[t2]*cov5+
				coef_cov6[t2]*cov6)/7;
      		//PRIORI
      		priori = -(1/(2*vpriori))*(intercept*intercept +cov1*cov1+cov2*cov2+cov3*cov3+cov4*cov4+cov5*cov5+cov6*cov6+ea*ea-
			  coef_corrente_vetor[0]*coef_corrente_vetor[0]-coef_corrente_vetor[1]*coef_corrente_vetor[1]-
			  coef_corrente_vetor[2]*coef_corrente_vetor[2]-coef_corrente_vetor[3]*coef_corrente_vetor[3]-
			  coef_corrente_vetor[4]*coef_corrente_vetor[4]-coef_corrente_vetor[5]*coef_corrente_vetor[5]-
			  coef_corrente_vetor[6]*coef_corrente_vetor[6]-coef_corrente_vetor[7]*coef_corrente_vetor[7]);
      		proposto = exp(beta_propostoi)/(exp(beta_propostoi)+exp(beta_propostoj));
      		corrente = exp(beta_correntei)/(exp(beta_correntei)+exp(beta_correntej));
      		//VEROSSIMILHANÇA
      		vero = s1*(log(nk*proposto+ea)-log(nk*corrente+coef_corrente_vetor[7]))-nk*(proposto-corrente)-ea+coef_corrente_vetor[7];
      		razao = exp(priori+vero) ;
      		//MINIMO
      		if(razao>=1) razao = 1;
     		//TAXA DE ACEITACAO
	        if( Rcpp::runif(1,0,1)[0]<= razao) {
           		contador_aux[t1]=contador_aux[t1]+1;
		        contador_aux[t2]=contador_aux[t2]+1;
           		beta_corrente_vetor[t1]=beta_propostoi;
	           	beta_corrente_vetor[t2]=beta_propostoj;
		   		coef_corrente_vetor[0] = intercept;
				coef_corrente_vetor[1] = cov1;
		   		coef_corrente_vetor[2] = cov2;
		   		coef_corrente_vetor[3] = cov3;
		   		coef_corrente_vetor[4] = cov4;
		   		coef_corrente_vetor[5] = cov5;   
		   		coef_corrente_vetor[6] = cov6; 
		   		coef_corrente_vetor[7] = ea;
      		}
      		total_testes_aux[t1]=total_testes_aux[t1]+1;
	        total_testes_aux[t2]=total_testes_aux[t2]+1;		
     }
    for(int k = 0; k<coef_cov1.size();k++) contador(j,k)=contador_aux[k];
    for(int k = 0; k<coef_cov1.size();k++) total(j,k)=total_testes_aux[k];
    for(int k = 0; k<coef_cov1.size();k++) mu(j+1,k)=beta_corrente_vetor[k];
    for(int k = 0; k<coef_corrente_vetor.size();k++) coef(j+1,k)=coef_corrente_vetor[k];
}

return Rcpp::List::create(Rcpp::Named("contador_aux")=contador,
                          Rcpp::Named("total_testes_aux")=total,
                          Rcpp::Named("beta_corrente_vetor")=mu,
      			 Rcpp::Named("coef_corrente_vetor")=coef);
  
}
