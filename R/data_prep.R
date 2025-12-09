#===========================================================#
#  Heart Failure Data Preparation
#===========================================================#
#
#'    *1*. Loads dataset
#'    *2*. Clusters patients into 5 age groups
#'    *3*. Reorganizes data into matrices for JAGS
#
#===========================================================#

#' __________________ *Packages* _____________________ #
library(cluster)

#===========================================================#
#'                    *Load Data*
#===========================================================#

# Data from UCI: https://archive.ics.uci.edu/dataset/519/heart+failure+clinical+records
heart <- read.csv("heart_failure_clinical_records_dataset.csv", header = TRUE)

#===========================================================#

heart_scaled <- scale(heart)

mypam <- pam(heart_scaled[, 1], k = 5) # Cluster based on age only (column 1)
cluster <- mypam$clustering # Add cluster assignment to data

for (j in 1:5) {
  ages_in_cluster <- heart$age[cluster == j]
  cat(sprintf("  Cluster %d: n=%d, ages %d-%d\n", 
              j, sum(cluster == j), min(ages_in_cluster), max(ages_in_cluster)))
}

#===========================================================#
# Rename the variables
#===========================================================#

death    <- heart_scaled[, "DEATH_EVENT"]
age      <- heart_scaled[, "age"]
anemia   <- heart_scaled[, "anaemia"]
sex      <- heart_scaled[, "sex"]
smoke    <- heart_scaled[, "smoking"]
CPK      <- heart_scaled[, "creatinine_phosphokinase"]
SC       <- heart_scaled[, "serum_creatinine"]
SS       <- heart_scaled[, "serum_sodium"]
platetes <- heart_scaled[, "platelets"]
HBP      <- heart_scaled[, "high_blood_pressure"]
ejf      <- heart_scaled[, "ejection_fraction"]
diabetes <- heart_scaled[, "diabetes"]

#===========================================================#
# Reorganize:
# Each column = one age cluster
# Rows = patients

g1 <- 5  # number of groups

# Count patients per cluster
n1 <- sapply(1:5, function(j) sum(cluster == j))
max_n <- max(n1)

# Function to create matrix for a variable
make_cluster_matrix <- function(var, cluster, n_groups, max_rows) {
  mat <- matrix(NaN, nrow = max_rows, ncol = n_groups)
  for (j in 1:n_groups) {
    vals <- var[cluster == j]
    mat[1:length(vals), j] <- vals
  }
  return(mat)
}

# Create matrices for each variable
deathj    <- make_cluster_matrix(death, cluster, g1, max_n)
anemiaj   <- make_cluster_matrix(anemia, cluster, g1, max_n)
sexj      <- make_cluster_matrix(sex, cluster, g1, max_n)
smokej    <- make_cluster_matrix(smoke, cluster, g1, max_n)
SCj       <- make_cluster_matrix(SC, cluster, g1, max_n)
SSj       <- make_cluster_matrix(SS, cluster, g1, max_n)
CPKj      <- make_cluster_matrix(CPK, cluster, g1, max_n)
HBPj      <- make_cluster_matrix(HBP, cluster, g1, max_n)
platetesj <- make_cluster_matrix(platetes, cluster, g1, max_n)
ejfj      <- make_cluster_matrix(ejf, cluster, g1, max_n)
diabetesj <- make_cluster_matrix(diabetes, cluster, g1, max_n)

