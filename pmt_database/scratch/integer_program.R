
library(lpSolve)
library(nloptr)

# Problem data
emplb <- c(0, 5, 10, 20, 50, 100, 250, 500, 1000)
empub <- c(4, 9, 19, 49, 99, 249, 499, 999, 2000)


# Dutchess 523 ----
estabs_initial <- c(59.8, 9.45, 4.72, rep(0, 6))
estabs_tot <- 74
emptot <- 269


# New York 332 ----
estabs_initial <- c(19.3, 3.86, 3.86, rep(0, 6))    # initial establishment estimates
estabs_tot <- 27         
emptot <- 286                                       # total employment


### STAGE 1: Integer Programming for Establishment Counts ###

# Calculate midpoint employment for each group
emp_mid <- (emplb + empub)/2

# Create constraint matrix
# Constraints: 1) Total establishments, 2) Total employment
f.con <- rbind(
  rep(1, 9),                    # Total establishments
  emp_mid                        # Expected employment
)

# Constraint directions and values
f.dir <- c("==", "<=")           # Exactly 74 estabs, employment <= 269
f.rhs <- c(estabs_tot, emptot)

# Objective: Minimize deviation from initial estimates
f.obj <- rep(1, 9)               # Dummy objective (we'll customize the real objective)

# Custom solve function to minimize deviation from initial estimates
solve_ip <- function() {
  best_solution <- NULL
  best_deviation <- Inf
  
  # Try different objective functions to find solution closest to initial estimates
  for (attempt in 1:3) {
    if (attempt == 1) {
      # First try: minimize absolute deviation
      f.obj <- 1/abs(estabs_initial + 0.1)  # +0.1 to avoid division by zero
    } else if (attempt == 2) {
      # Second try: minimize squared deviation
      f.obj <- 1/(estabs_initial^2 + 0.1)
    } else {
      # Third try: equal weights
      f.obj <- rep(1, 9)
    }
    
    res <- lp(
      "min",
      f.obj,
      f.con,
      f.dir,
      f.rhs,
      all.int = TRUE,
      presolve = TRUE
    )
    
    if (res$status == 0) {
      current_dev <- sum(abs(res$solution - estabs_initial))
      if (current_dev < best_deviation) {
        best_deviation <- current_dev
        best_solution <- res$solution
      }
    }
  }
  
  return(best_solution)
}

# Solve the integer program
estabs_int <- solve_ip()

# If no solution found, relax employment constraint
if (is.null(estabs_int)) {
  f.dir[2] <- "<="
  f.rhs[2] <- emptot * 1.1  # Allow 10% over employment
  estabs_int <- solve_ip()
}

### STAGE 2: Optimize Employment Numbers ###

# Only optimize employment for groups with establishments
active_groups <- which(estabs_int > 0)

obj_emp <- function(emp, estabs, emptot, emplb, empub) {
  # Penalty for missing total employment
  employment_penalty <- (sum(emp * estabs) - emptot)^2
  
  # Penalty for being outside bounds
  bounds_penalty <- sum(
    ifelse(emp < emplb, (emplb - emp)^2, 0) +
      ifelse(emp > empub, (emp - empub)^2, 0)
  )
  
  employment_penalty + bounds_penalty
}

# Initial employment estimates (midpoints of active groups)
emp_init <- (emplb[active_groups] + empub[active_groups])/2

# Optimize employment
emp_res <- optim(
  par = emp_init,
  fn = obj_emp,
  method = "L-BFGS-B",
  lower = emplb[active_groups],
  upper = empub[active_groups],
  estabs = estabs_int[active_groups],
  emptot = emptot,
  emplb = emplb[active_groups],
  empub = empub[active_groups]
)

# Create full employment vector
final_emp <- numeric(9)
final_emp[active_groups] <- emp_res$par

### POST-PROCESSING ###

# Adjust to exactly match total employment
current_emptot <- sum(final_emp * estabs_int)
scale_factor <- emptot / current_emptot
final_emp_adj <- pmin(pmax(final_emp * scale_factor, emplb), empub)

# Final results
results <- data.frame(
  Group = 1:9,
  Emp_LB = emplb,
  Emp_UB = empub,
  Estabs_Initial = round(estabs_initial, 2),
  Estabs_Final = estabs_int,
  Emp_per_Estab = round(final_emp_adj, 2),
  Total_Employment = round(estabs_int * final_emp_adj, 2)
)

# Verification
cat("Total establishments:", sum(results$Estabs_Final), 
    "(Target:", estabs_tot, ")\n")
cat("Total employment:", sum(results$Total_Employment), 
    "(Target:", emptot, ")\n\n")

# Print results
print(results)

