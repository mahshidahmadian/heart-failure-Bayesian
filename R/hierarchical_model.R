#===========================================================#
#  Hierarchical Bayesian Logistic Regression
#===========================================================#
#
#  Model:
#    logit(p_ij) = b0[j] + b1[j]*EF + b2[j]*HBP + fixed effects
#
#  where j = 1,...,5 age groups
#
#  Random effects (vary by age group):
#    - b0[j]: intercept
#    - b1[j]: ejection fraction effect
#    - b2[j]: high blood pressure effect
#
#  Fixed effects (same across groups):
#    - smoking, serum creatinine, serum sodium, CPK
#    - sex, platelets, anemia, diabetes
#
#===========================================================#

#' __________________ *Packages* _____________________ #
library(R2jags)

#===========================================================#

mode <- "model{
  
  # Loop over age groups
  for (j in 1:g1) {
    
    # Loop over patients in each group
    for (i in 1:n1[j]) {
      
      # Likelihood: death is Bernoulli
      deathj[i,j] ~ dbern(p[i,j])
      
      # Logistic regression with random + fixed effects
      logit(p[i,j]) <- b0[j] + b1[j]*ejfj[i,j] + b2[j]*HBPj[i,j]
                       + b3*smokej[i,j] + b4*SCj[i,j] + b5*SSj[i,j]
                       + b6*CPKj[i,j] + b7*sexj[i,j] + b8*platetesj[i,j]
                       + b9*anemiaj[i,j] + b10*diabetesj[i,j]
    }
    
    # Random effects (hierarchical priors)
    b0[j] ~ dnorm(b0mu, b0tau)
    b1[j] ~ dnorm(b1mu, b1tau)
    b2[j] ~ dnorm(b2mu, b2tau)
  }
  
  # Fixed effects priors (weakly informative)
  b3  ~ dnorm(0, 0.01)  # smoking
  b4  ~ dnorm(0, 0.01)  # serum creatinine
  b5  ~ dnorm(0, 0.01)  # serum sodium
  b6  ~ dnorm(0, 0.01)  # CPK
  b7  ~ dnorm(0, 0.01)  # sex
  b8  ~ dnorm(0, 0.01)  # platelets
  b9  ~ dnorm(0, 0.01)  # anemia
  b10 ~ dnorm(0, 0.01)  # diabetes
  
  # Hyper-priors for random effects
  b0mu ~ dnorm(0, 0.1)
  b1mu ~ dnorm(0, 0.1)
  b2mu ~ dnorm(0, 0.1)
  
  b0tau ~ dgamma(1, 1)
  b1tau ~ dgamma(1, 1)
  b2tau ~ dgamma(1, 1)
  
  # Convert precision to variance for interpretation
  b0var <- 1/b0tau
  b1var <- 1/b1tau
  b2var <- 1/b2tau
}"

# Write model to file
writeLines(model, "hierarchical_model.jags")

#===========================================================#--------

jags_data <- list(
  g1 = g1,
  n1 = n1,
  deathj = deathj,
  ejfj = ejfj,
  HBPj = HBPj,
  smokej = smokej,
  SCj = SCj,
  SSj = SSj,
  CPKj = CPKj,
  sexj = sexj,
  platetesj = platetesj,
  anemiaj = anemiaj,
  diabetesj = diabetesj
)

#===========================================================#

params <- c("b0", "b1", "b2",           # random effects
            "b3", "b4", "b5", "b6",     # fixed effects
            "b7", "b8", "b9", "b10",
            "b0mu", "b1mu", "b2mu",     # hyper-parameters
            "b0var", "b1var", "b2var")

#===========================================================#

init_fn <- function() {
  list(
    b0 = rep(0, g1),
    b1 = rep(0, g1),
    b2 = rep(0, g1),
    b3 = 0, b4 = 0, b5 = 0, b6 = 0,
    b7 = 0, b8 = 0, b9 = 0, b10 = 0
  )
}

#===========================================================#
#'                     *Run MCMC*
#===========================================================#

cat("Running JAGS...\n")

fit <- jags(
  data = jags_data,
  inits = init_fn,
  parameters.to.save = params,
  model.file = "hierarchical_model.jags",
  n.chains = 2,
  n.iter = 10000,
  n.burnin = 1000,
  n.thin = 1
)

#===========================================================#
#'                     *Results*
#===========================================================#

print(fit)

# Trace plots to check convergence
traceplot(fit)

#===========================================================#

cat("\n=== Fixed Effects (Odds Ratios) ===\n")

fixed_names <- c("Smoking", "Serum Creatinine", "Serum Sodium", "CPK",
                 "Sex", "Platelets", "Anemia", "Diabetes")
fixed_params <- c("b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10")

for (i in seq_along(fixed_params)) {
  est <- fit$BUGSoutput$mean[[fixed_params[i]]]
  odds_ratio <- exp(est)
  cat(sprintf("%s: OR = %.3f (log-OR = %.3f)\n", 
              fixed_names[i], odds_ratio, est))
}

#===========================================================#

cat("\n=== Random Effects by Age Group ===\n")

cat("\nIntercepts (b0):\n")
print(round(fit$BUGSoutput$mean$b0, 3))

cat("\nEjection Fraction effect (b1):\n")
print(round(fit$BUGSoutput$mean$b1, 3))

cat("\nHigh BP effect (b2):\n")
print(round(fit$BUGSoutput$mean$b2, 3))
