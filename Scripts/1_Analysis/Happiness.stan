data {
  int<lower=1> N;				// Number of participants
  int<lower=1> T;				// Maximum number of trials completed by any participant
  int<lower=1, upper=T> Tsubj[N];			// Number of trials completed by each subject (vector)

  real<lower=0> gamble_opt1[N, T];		// The first option for the gamble on each trial (matrix)
  real<lower=0> gamble_opt2[N, T];		// The second option for the gamble on each trial (matrix)
  real cert[N, T];				// The sure-bet on each trial (matrix)
  real outcome[N, T];				// The gamble outcome on each trial (matrix)
  real<lower=-5> rpe[N,T]; // The rpe on each trial (matrix) 
  int<lower=-1, upper=1> type_cert[N, T]; 		// Index variable that equals 1 when sure-bet was chosen, 0 otherwise (matrix)
  int<lower=-1, upper=1> type_gamb[N, T]; 		// Index variable that equals 1 when gamble was chosen, 0 otherwise

  int<lower=1> Max_Rated_trials;      // Maximum number of rated trials completed by any participant
  int<lower=0, upper=T> Rated_trials[N];     // Number of rated trials completed by each participant (vector) 
  real rating[N, Max_Rated_trials];     // Rating on each trial (matrix)
  int<lower=0> rated_trials_idx[N, Max_Rated_trials]; // Trial number (index) of trials that were rated by each participant (matrix) 
  int<lower=1> Trials_back;//real<lower=1> Trials_back;     // Number of past trials to integrate into calculation
}

transformed data {
}

parameters {
  vector[6] mu_pr;
  vector<lower=0>[6] sigma;
  vector[N] w0_pr;
  vector[N] w1_pr;
  vector[N] w2_pr;
  vector[N] w3_pr;
  vector[N] gam_pr;
  vector[N] sig_pr;
}

transformed parameters {
  vector[N] w0;
  vector[N] w1;
  vector[N] w2;
  vector[N] w3;
  vector<lower=0, upper=1>[N] gam;
  vector<lower=0>[N] sig;

  w0 = mu_pr[1] + sigma[1] * w0_pr;
  w1 = mu_pr[2] + sigma[2] * w1_pr;
  w2 = mu_pr[3] + sigma[3] * w2_pr;
  w3 = mu_pr[4] + sigma[4] * w3_pr;

  for (i in 1:N) {
    gam[i] = Phi_approx(mu_pr[5] + sigma[5] * gam_pr[i]);
  }
  sig = exp(mu_pr[6] + sigma[6] * sig_pr);
}

model {
  mu_pr ~ normal(0, 1.0);
  sigma ~ normal(0, 1.0);

  // individual parameters w/ Matt trick
  w0_pr    ~ normal(0, 1.0);
  w1_pr    ~ normal(0, 1.0);
  w2_pr    ~ normal(0, 1.0);
  w3_pr    ~ normal(0, 1.0);
  gam_pr   ~ normal(0, 1.0);
  sig_pr   ~ normal(0, 1.0);

  for (i in 1:N) {
    real cert_sum;
    real ev_sum;
    real ev_sum_tmp;
    real rpe_sum;
    real rpe_sum_tmp;
    int rating_ticker;

    rating_ticker = 1;

    for (t in 1:Tsubj[i]) {

      if (rating_ticker < Rated_trials[i] && t == rated_trials_idx[i, rating_ticker]) {

    	cert_sum = 0;
    	ev_sum   = 0;
    	rpe_sum  = 0;

    	if (t < Trials_back) {

    	  for (j in 1:t) {

    	     cert_sum += type_cert[i,t+1-j] * (gam[i]^(j-1)) * cert[i,t+1-j];

    	     ev_sum_tmp  = 0.5 * (gamble_opt1[i,t+1-j] + gamble_opt2[i,t+1-j]);
    	     ev_sum += type_gamb[i,t+1-j] * (gam[i]^(j-1)) * ev_sum_tmp;

    	     rpe_sum_tmp = outcome[i,t+1-j] - 0.5 * (gamble_opt1[i,t+1-j] + gamble_opt2[i,t+1-j]);
    	     rpe_sum += type_gamb[i,t+1-j] * (gam[i]^(j-1)) * rpe_sum_tmp; 
    	  }

    	} else if (t >= Trials_back) {

    	  for (j in 1:Trials_back) {

    	     cert_sum += type_cert[i,t+1-j] * (gam[i]^(j-1)) * cert[i,t+1-j];

    	     ev_sum_tmp = 0.5 * (gamble_opt1[i,t+1-j] + gamble_opt2[i,t+1-j]); 
    	     ev_sum += type_gamb[i,t+1-j] * (gam[i]^(j-1)) * ev_sum_tmp;

    	     rpe_sum_tmp = outcome[i,t+1-j] - 0.5 * (gamble_opt1[i,t+1-j] + gamble_opt2[i,t+1-j]); 
    	     rpe_sum += type_gamb[i,t+1-j] * (gam[i]^(j-1)) * rpe_sum_tmp; 
    	  }
    	}

    	rating[i,rating_ticker] ~ normal((w0[i] + (w1[i] * cert_sum) + (w2[i] * ev_sum) + (w3[i] * rpe_sum)), sig[i]);

          rating_ticker += 1;      

        }
      } // Trial loop
    } // Subject loop
}

generated quantities {
  real mu_w0;
  real mu_w1;
  real mu_w2;
  real mu_w3;
  real<lower=0, upper=1> mu_gam;
  real<lower=0> mu_sig;

  real log_lik[N];

  // For posterior predictive check
  real y_pred[N, T];

  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i, t] = -1;
    }
  }

  mu_w0    = mu_pr[1];
  mu_w1    = mu_pr[2];
  mu_w2    = mu_pr[3];
  mu_w3    = mu_pr[4];
  mu_gam   = Phi_approx(mu_pr[5]);
  mu_sig   = exp(mu_pr[6]);


  { // local section, this saves time and space
    for (i in 1:N) {
      real cert_sum;
      real ev_sum;
      real ev_sum_tmp;
      real rpe_sum;
      real rpe_sum_tmp;
      int rating_ticker;

      rating_ticker = 1;
      log_lik[i] = 0;

      for (t in 1:Tsubj[i]) {

        if (rating_ticker < Rated_trials[i] && t == rated_trials_idx[i, rating_ticker]) {

	  cert_sum = 0;
	  ev_sum   = 0;
	  rpe_sum  = 0;
	  
	  if (t < Trials_back) {

	    for (j in 1:t) {

	       cert_sum += type_cert[i,t+1-j] * (gam[i]^(j-1)) * cert[i,t+1-j];

	       ev_sum_tmp  = 0.5 * (gamble_opt1[i,t+1-j] + gamble_opt2[i,t+1-j]); 
	       ev_sum += type_gamb[i,t+1-j] * (gam[i]^(j-1)) * ev_sum_tmp;

	       rpe_sum_tmp = outcome[i,t+1-j] - 0.5 * (gamble_opt1[i,t+1-j] + gamble_opt2[i,t+1-j]); 
	       rpe_sum += type_gamb[i,t+1-j] * (gam[i]^(j-1)) * rpe_sum_tmp; 
	    }

	  } else if (t >= Trials_back) {

	    for (j in 1:Trials_back) {

	       cert_sum += type_cert[i,t+1-j] * (gam[i]^((j)-1)) * cert[i,t+1-j];

	       ev_sum_tmp = 0.5 * (gamble_opt1[i,t+1-j] + gamble_opt2[i,t+1-j]);
	       ev_sum += type_gamb[i,t+1-j] * (gam[i]^(j-1)) * ev_sum_tmp;

	       rpe_sum_tmp = outcome[i,t+1-j] - 0.5 * (gamble_opt1[i,t+1-j] + gamble_opt2[i,t+1-j]);
	       rpe_sum += type_gamb[i,t+1-j] * (gam[i]^(j-1)) * rpe_sum_tmp;
	    }
	  }

          log_lik[i] += normal_lpdf(rating[i, rating_ticker] | (w0[i] + (w1[i] * cert_sum) + (w2[i] * ev_sum) + (w3[i] * rpe_sum)), sig[i]);
          y_pred[i, t] = normal_rng((w0[i] + (w1[i] * cert_sum) + (w2[i] * ev_sum) + (w3[i] * rpe_sum)), sig[i]);
      
          rating_ticker += 1;

        }
      }  // Trial loop
    } // Individual loop    
  }
}

