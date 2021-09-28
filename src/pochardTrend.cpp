#include <TMB.hpp>


template<class Type>
Type objective_function<Type>::operator() ()
{
    /* data section */
    DATA_VECTOR(y);               // number of counted pochards
    DATA_FACTOR(whichsite);        // which site on which count ?
    DATA_FACTOR(whichquad);        // which quadrat on which count?
    DATA_VECTOR(x);               // year
    DATA_VECTOR(latitude);               // latitude
    
    
    /* Parameter section */
    PARAMETER(beta0);                // effets fixes
    PARAMETER(latitude_effect);                // latitude effect
    PARAMETER_VECTOR(site_effect);      // fixed site effect (alpha_i)
    PARAMETER(log_sigmabqi);           // sig random effect quadrats on slope
    PARAMETER(log_sigmadi);          // sig random effect sites on slope
    PARAMETER(log_sigmae);        // log de l'écart-type des résidus surdisp
    PARAMETER_VECTOR(bqi);             // random effects of quadrats on slopes
    PARAMETER_VECTOR(di);            // random effects of sites on slope
    PARAMETER_VECTOR(eit);          // overdispersion residuals
    
    
    /* Procedure section */
    
    /* Likelihood of random effects */
    Type lambda=0;
    Type nll=0; // Log-vraisemblance initialisée
    int lsize=latitude.size();
    vector<Type> bend(lsize);
    
    nll -= dnorm(di, Type(0), Type(exp(log_sigmadi)), true).sum();
    nll -= dnorm(bqi, Type(0), Type(exp(log_sigmabqi)), true).sum();
    nll -= dnorm(eit, Type(0), Type(exp(log_sigmae)), true).sum();
    
    /* likelihood of observations */
    for (int j=0; j<latitude.size(); j++) {
      bend[j] = (latitude_effect * latitude[j]) + bqi[j];      
    }
    
    /* For each site-year, add the contribution of the likelihood */
    for (int i=0; i< y.size(); i++){
		
      /* Expectation of the Poisson distribution (includes the overdispersion residual */
      lambda = exp(site_effect[whichsite[i]] +
                   (beta0 + di[whichsite[i]]+bend[whichquad[i]])*x[i] + eit[i]);
	
	/* Poisson Distribution */
	nll-=dpois(y[i], lambda, true);
    }
	
    /* Result */
    return nll;
}
    
	
