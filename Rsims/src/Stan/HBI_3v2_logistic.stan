//Nested concurrent hierarchical estimation and inference for logistic regression
data{
  // number of participants
  int<lower=1> P;
  // Number of actual choices 
  int<lower=1> M;
  // Number of regressors to enter the model
  int<lower=1> O;
  // Matrix of regressors (MxO)
  vector[O] Reg[M];
  // Index for participant number per data point
  int<lower=1> Pn[M];
  // Data of choices
  int<lower=0> y[M];
}
parameters{
  real alpha3;                         // Population level intercept
  row_vector[O] beta3;                 // Population level slopes
  vector[P] z_par_alpha3;              // Standardized participant level intercept
  row_vector[O] z_par_beta3[P];        // Standardized participant level slope
  real<lower=0> sigma_a3;              // Rescaler of participant level intercept
  row_vector<lower=0>[O] sigma_b3;     // Rescaler of participant level slopes
  
  real alpha2;                         // Population level intercept
  row_vector[O-1] beta2;               // Population level slopes
  vector[P] z_par_alpha2;              // Standardized participant level intercept
  row_vector[O-1] z_par_beta2[P];      // Standardized participant level slope
  real<lower=0> sigma_a2;              // Rescaler of participant level intercept
  row_vector<lower=0>[O-1] sigma_b2;   // Rescaler of participant level slopes
  
  simplex[2] r[P];                     // Mixing proportions per participant
}
transformed parameters{
  vector[P] par_alpha3;          // Proper participant level intercept
  row_vector[O] par_beta3[P];    // Proper participant level slope
  //vector[M] theta_hat3;          // Predicted choices per trial
  vector[P] par_alpha2;
  row_vector[O-1] par_beta2[P];
  //vector[M] theta_hat2;
  
  par_alpha3 = z_par_alpha3 * sigma_a3;
  par_alpha2 = z_par_alpha2 * sigma_a2;
  for(p in 1:P){
    par_beta3[p] = z_par_beta3[p] .* sigma_b3;
    par_beta2[p] = z_par_beta2[p] .* sigma_b2;
  }
  //for(n in 1:M){
  //  theta_hat3[n] = inv_logit(alpha3 + par_alpha3[Pn[n]] + (beta3 + par_beta3[Pn[n]])*Reg[n]);
  //  theta_hat2[n] = inv_logit();
  //}
}
model{
  vector[2] logR[P];
  for(p in 1:P){
    logR[p] = log(r[p]);
  }
  target += normal_lpdf(sigma_a3|0,1) - normal_lccdf(0|0,1);
  target += normal_lpdf(sigma_b3|0,1) - O*normal_lccdf(0|0,1);
  target += normal_lpdf(sigma_a2|0,1) - normal_lccdf(0|0,1);
  target += normal_lpdf(sigma_b2|0,1) - (O-1)*normal_lccdf(0|0,1);
  
  target += normal_lpdf(alpha3|0,10);
  target += normal_lpdf(beta3|0,10);
  target += normal_lpdf(alpha2|0,10);
  target += normal_lpdf(beta2|0,10);
  
  target += std_normal_lpdf(z_par_alpha3);
  target += std_normal_lpdf(z_par_alpha2);
  for(p in 1:P){
    target += std_normal_lpdf(z_par_beta3[p]);
    target += std_normal_lpdf(z_par_beta2[p]);
  }
  
  for(n in 1:M){
    target += log_sum_exp(logR[Pn[n]][1] + bernoulli_logit_lpmf(y[n]|alpha3 + par_alpha3[Pn[n]] + (beta3 + par_beta3[Pn[n]])*Reg[n]),
                          logR[Pn[n]][2] + bernoulli_logit_lpmf(y[n]|alpha2 + par_alpha2[Pn[n]] + (beta2 + par_beta2[Pn[n]]*Reg[n,1:2])));   
  }
}
generated quantities{
  vector[2] Nbar;
  vector[2] Mfreq;
  for(k in 1:2){
    Nbar[k] = sum(r[:,k]);
    Mfreq[k] = Nbar[k]/P;
  }
}