# Check importance tracker convergence

devtools::load_all("~/repos/usaid-mch-ml/r-package")

# Create an importance tracker for 10 arbitrary values with small standard deviation
importance_tracker <- mch.ml::ImportanceTracker$new(n_var = 10)
ii <- 1
converged <- FALSE

while((ii < 1e5) & !converged){
  if(ii %% 1000 == 0) message('.', appendLF = F)
  importance_tracker$update(1:10 + rnorm(10))
  if(ii > 2) converged <- importance_tracker$converged()
  ii <- ii + 1
}

message("Importance tracker converged after ", ii, " iterations.")
