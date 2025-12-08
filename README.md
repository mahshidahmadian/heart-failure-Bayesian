# Heart Failure Prediction with Hierarchical Bayesian Modeling

This is a class project for Course: STAT 591 - Bayesian Statistics, Virginia Commonwealth University, year 2022.

Predicting mortality in heart failure patients using a hierarchical logistic regression model fitted with MCMC (JAGS).

## The Problem

Heart failure patients have varying risk factors depending on their age. A standard logistic regression treats all patients the same, but a **hierarchical model** lets the coefficients vary by age group while sharing information across groups.

## Data

299 patients. 96 patients died during the study period.

**Variables:**
- Death event (response)
- Age, sex, smoking status
- Anemia, diabetes, high blood pressure
- Ejection fraction (% blood pumped per heartbeat)
- Serum creatinine, serum sodium, CPK enzyme, platelets

**Citation:**
Source: [UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/519/heart+failure+clinical+records)

## Approach

1. **Cluster patients by age** using PAM (k-medoids) into 5 groups
2. **Fit hierarchical logistic regression** where intercepts and some slopes vary by age group
3. **Estimate with MCMC** using JAGS (Gibbs sampling)

## Model

```
logit(p_ij) = β₀ⱼ + β₁ⱼ·EF + β₂ⱼ·HBP + γ'z

β₀ⱼ ~ N(μ₀, σ₀²)
β₁ⱼ ~ N(μ₁, σ₁²)    j = 1,...,5 age groups
β₂ⱼ ~ N(μ₂, σ₂²)
```

Ejection fraction (EF) and high blood pressure (HBP) get random slopes because their effects vary with age. Other predictors have fixed effects.

