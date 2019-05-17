data{
  int numsteps;
  
  // Mix Prior Vectors: mu[MSH,MSP], sigma[MSH,MSP], theta
  vector[5] N_mix;
  vector[5] T_mix;
  vector[5] Bt_mix;
  
  // mu[MSH], sigma[MSH], Min[MSP], max[MSP]
  vector[4] Clock_mix;
  
  vector[numsteps] By;
  vector[numsteps] Bz;
  vector[numsteps] Bt;
  vector[numsteps] N;
  vector[numsteps] T_perp;
  vector[numsteps] T_para;
  vector<lower=0, upper=255>[numsteps] Priority;
}
transformed data{
  vector[numsteps] N_log;
  vector[numsteps] T_log;
  vector[numsteps] Clock_Angle;
  
  for (i in 1:numsteps){
    N_log[i] = log(N[i]);
    T_log[i] = log((T_para[i] + 2 * T_perp[i]) / 3);
    Clock_Angle[i] = atan2(By[i], Bz[i]);
  }
}
parameters{
  // Mixture Model
  vector<lower=0, upper=1>[numsteps] Bt_Mixture;
  vector<lower=0, upper=1>[numsteps] N_Mixture;
  vector<lower=0, upper=1>[numsteps] T_Mixture;
  vector<lower=0, upper=1>[numsteps] Clock_Mixture;
  real<lower=0> Clock_sigma;
  
  // Auto Regression
  real<lower=0> Bt_mix_sigma;
  real<lower=0> N_mix_sigma;
  real<lower=0> T_mix_sigma;
  real<lower=0> Clock_mix_sigma;
  
  // Linear Regression
  real mixture_alpha;
  real<lower=0> mixture_sigma;
  real Bt_beta;
  real N_beta;
  real T_beta;
  real Clock_beta;
}
model{
  // Mixture model
  for (n in 1:numsteps){
    target += log_mix(Bt_Mixture[n],
                      normal_lpdf(Bt[n] | Bt_mix[1], Bt_mix[3]),
                      normal_lpdf(Bt[n] | Bt_mix[2], Bt_mix[4]));
    
    target += log_mix(N_Mixture[n],
                      normal_lpdf(N_log[n] | N_mix[1], N_mix[3]),
                      normal_lpdf(N_log[n] | N_mix[2], N_mix[4]));
    
    target += log_mix(T_Mixture[n],
                      normal_lpdf(T_log[n] | T_mix[1], T_mix[3]),
                      normal_lpdf(T_log[n] | T_mix[2], T_mix[4]));
    
    target += log_mix(Clock_Mixture[n],
                      uniform_lpdf(Clock_Angle[n] | Clock_mix[3], Clock_mix[4]),
                      normal_lpdf(Clock_Angle[n] | Clock_mix[1], Clock_sigma));
  }
  
  // Auro-Regression
  for (n in 2:numsteps){
    Bt_Mixture[n] ~ normal(Bt_Mixture[n-1], Bt_mix_sigma);
    N_Mixture[n] ~ normal(N_Mixture[n-1], N_mix_sigma);
    T_Mixture[n] ~ normal(T_Mixture[n-1], T_mix_sigma);
    Clock_Mixture[n] ~ normal(Clock_Mixture[n-1], Clock_mix_sigma);
  }
  
  // Linear Regression
  Priority ~ normal(mixture_alpha + Bt_beta * Bt_Mixture +
                    N_beta * N_Mixture + T_beta * T_Mixture +
                    Clock_beta * Clock_Mixture, mixture_sigma);
}

