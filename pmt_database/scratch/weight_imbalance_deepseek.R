


# Dutchess 523 ----
estabs_initial <- c(59.8, 9.45, 4.72, rep(0, 6))
estabs_tot <- 74
emptot <- 269


# New York 332 ----
estabs_initial <- c(19.3, 3.86, 3.86, rep(0, 6))    # initial establishment estimates
estabs_tot <- 27         
emptot <- 286                                       # total employment



# Orange County 484  ------------------------------------------------------
estabs_initial <- c(98.1, 19.9, 9.57, 11.2, 8.78, 2.39, 0, 0, 0)    # initial establishment estimates
estabs_tot <- 150         
emptot <- 2304 



# Queens County 322 -------------------------------------------------------
estabs_initial <- c(6.21368698595097, 1.29876824997049, 0.749439219235764, 0.479379009090551, 
                    0.136004092715753, 0.072960528904805, 0.0269174766833261, 0.0143441816536146, 
                    0.00850025579473456)
estabs_tot <- 9
emptot <- 20                                       # total employment



# Richmond County 525 -----------------------------------------------------
estabs_initial <- c(6, rep(0, 8))
estabs_tot <- 6
emptot <- 5



# Rockland County 457 -----------------------------------------------------
estabs_initial <- c(47.0967741935484, 22.3709677419355, 3.53225806451613, 0, 0, 
                    0, 0, 0, 0)
estabs_tot <- 73
emptot <- 339



# Suffolk County 531 ------------------------------------------------------
estabs_initial <- c(1360.45616973758, 142.281407035176, 50.0619765494137, 17.5656058068118, 
                    2.63484087102178, 0, 0, 0, 0)
estabs_tot <- 1573
emptot <- 5685



# Westchester County 333  -------------------------------------------------
estabs_initial <- c(11, 5.5, 5.5, 0, 0, 0, 0, 0, 0)
estabs_tot <- 22
emptot <- 257



# problem setup -----------------------------------------------------------

# prob <- list(
#   emplb = c(0, 5, 10, 20, 50, 100, 250, 500, 1000),
#   empub = c(4, 9, 19, 49, 99, 249, 499, 999, 2000),
#   estabs_initial = estabs_initial,
#   emptot = emptot,
#   estabs_tot = estabs_tot,
#   weights = c(
#     employment_match = 500,  # Increased from 100
#     estabs_match = 500,      # Increased from 100
#     initial_estabs = 0.5,    # Reduced from 1
#     emp_bounds = 5,
#     small_estabs = 10,
#     emp_without_estabs = 5
#   )
# )



# start of optimized version DJB USE THIS! ----------------------------------------------

library(nloptr)
library(nloptr)

# Problem data
# prob <- list(
#   emplb = c(0, 5, 10, 20, 50, 100, 250, 500, 1000),
#   empub = c(4, 9, 19, 49, 99, 249, 499, 999, 2000),
#   estabs_initial = c(47.096774, 22.370968, 3.532258, rep(0, 6)),
#   emptot = 339,
#   estabs_tot = 73,
#   weights = c(50, 100, 20, 5, 10, 5)  # employment, estabs_total, etc.
# )

prob <- list(
  emplb = c(0, 5, 10, 20, 50, 100, 250, 500, 1000),
  empub = c(4, 9, 19, 49, 99, 249, 499, 999, 2000),
  estabs_initial = estabs_initial,
  emptot = emptot,
  estabs_tot = estabs_tot,
  weights = c(50, 100, 20, 5, 10, 5)  # employment, estabs_total, etc.
)

# Robust initialization ensuring bounds
x0 <- local({
  emp_mid <- (prob$emplb + prob$empub)/2
  target_ratio <- prob$emptot / max(1e-6, sum(emp_mid * prob$estabs_initial))
  
  # Ensure employment bounds
  emp_init <- pmax(prob$emplb, pmin(emp_mid * target_ratio, prob$empub))
  
  # Ensure establishment bounds with smoothing
  estabs_init <- sqrt(pmax(0.1, prob$estabs_initial - 0.5))
  estabs_init <- pmin(estabs_init, prob$estabs_tot * 3)
  estabs_init <- pmax(0.01, estabs_init)  # Strictly positive lower bound
  
  list(emp = emp_init, estabs = estabs_init)
})

# Verify initialization
stopifnot(all(x0$emp >= prob$emplb),
          all(x0$emp <= prob$empub),
          all(x0$estabs >= 0.01),
          all(x0$estabs <= prob$estabs_tot * 3))

# Objective function
objective_function <- function(x, prob) {
  es_emp <- x[1:9]
  es_estabs <- ifelse(x[10:18] < 0.5, 0, 0.5 + x[10:18]^2)
  es_emptot <- es_emp * es_estabs
  
  # Calculate penalty components
  employment_penalty <- ((sum(es_emptot) - prob$emptot)^2) / (prob$emptot^2 + 100)
  estabs_penalty <- ((sum(es_estabs) - prob$estabs_tot)^2) / (prob$estabs_tot^2 + 100)
  initial_match_penalty <- sum((es_estabs - prob$estabs_initial)^2)
  
  bounds_penalty <- sum(sapply(1:9, function(i) {
    if (es_estabs[i] > 0) {
      lower_penalty <- ifelse(es_emp[i] < prob$emplb[i], (prob$emplb[i] - es_emp[i])^2, 0)
      upper_penalty <- ifelse(es_emp[i] > prob$empub[i], (es_emp[i] - prob$empub[i])^2, 0)
      return(lower_penalty + upper_penalty)
    }
    return(0)
  }))
  
  small_estabs_penalty <- sum(ifelse(es_estabs > 0 & es_estabs < 1, (1 - es_estabs)^4, 0))
  emp_without_estabs_penalty <- sum(ifelse(es_estabs == 0 & es_emp > 0, es_emp^2, 0))
  
  # Combine with weights
  sum(c(
    prob$weights[1] * employment_penalty,
    prob$weights[2] * estabs_penalty,
    prob$weights[3] * initial_match_penalty,
    prob$weights[4] * bounds_penalty,
    prob$weights[5] * small_estabs_penalty,
    prob$weights[6] * emp_without_estabs_penalty
  ))
}

# Constraint function
constraint_function <- function(x, prob) {
  es_emp <- x[1:9]
  es_estabs <- ifelse(x[10:18] < 0.5, 0, 0.5 + x[10:18]^2)
  c(sum(es_emp * es_estabs) - prob$emptot,
    sum(es_estabs) - prob$estabs_tot)
}

# Run optimization
res <- nloptr(
  x0 = c(x0$emp, x0$estabs),
  eval_f = objective_function,
  eval_g_eq = constraint_function,
  lb = c(prob$emplb, rep(0.01, 9)),  # Explicit lower bounds
  ub = c(prob$empub, rep(prob$estabs_tot * 3, 9)),
  opts = list(
    algorithm = "NLOPT_LN_COBYLA",
    maxeval = 8000,
    xtol_rel = 1e-7,
    ftol_abs = 1,
    print_level = 0
  ),
  prob = prob
)

# Post-processing with constraint satisfaction
final <- local({
  emp <- pmax(prob$emplb, pmin(res$solution[1:9], prob$empub))
  estabs <- pmax(0, ifelse(res$solution[10:18] < 0.5, 0, 0.5 + res$solution[10:18]^2))
  
  # Scale to exact constraints
  scale_emp <- prob$emptot / sum(emp * estabs)
  scale_estabs <- prob$estabs_tot / sum(estabs)
  
  emp_final <- pmax(prob$emplb, pmin(emp * scale_emp, prob$empub))
  estabs_final <- estabs * scale_estabs
  
  # Respect initial zeros
  estabs_final[prob$estabs_initial == 0 & estabs_final < 0.5] <- 0
  
  # Final scaling to account for zeroing
  scale_emp_final <- prob$emptot / sum(emp_final * estabs_final)
  emp_final <- pmax(prob$emplb, pmin(emp_final * scale_emp_final, prob$empub))
  
  list(emp = emp_final, estabs = estabs_final)
})

# Create results table
results <- data.frame(
  Group = 1:9,
  Estabs_Initial = prob$estabs_initial,
  Estabs_Final = round(final$estabs, 2),
  Emp_per_Estab = round(final$emp, 2),
  Total_Employment = round(final$estabs * final$emp, 2)
)

# Verification
cat("Optimization results:\n")
print(results)
cat("\nVerification:\n")
cat("Total establishments:", sum(results$Estabs_Final), "(Target:", prob$estabs_tot, ")\n")
cat("Total employment:", sum(results$Total_Employment), "(Target:", prob$emptot, ")\n")









# start of faster version ----
# Optimized version with faster convergence and better numerical stability
res <- nloptr(
  x0 = {
    emp_init <- pmin(pmax(pmean(prob$emplb, prob$empub), prob$emplb), prob$empub)
    estabs_init <- sqrt(pmax(0.1, prob$estabs_initial - 0.5)) # More aggressive initialization
    c(emp_init, estabs_init)
  },
  eval_f = function(x) {
    es_emp <- x[1:9]
    es_estabs <- ifelse(x[10:18] < 0.5, 0, 0.5 + x[10:18]^2)
    
    # Rebalanced weights (higher weight for initial estimates)
    penalties <- c(
      (prob$emptot - sum(es_emp * es_estabs))^2 / 1e6,
      (prob$estabs_tot - sum(es_estabs))^2 / 1e4,
      sum((es_estabs - prob$estabs_initial)^2), # Keep this unnormalized
      sum(sapply(1:9, function(i) {
        if (es_estabs[i] > 0) {
          if (es_emp[i] < prob$emplb[i]) return((prob$emplb[i] - es_emp[i])^2)
          if (es_emp[i] > prob$empub[i]) return((es_emp[i] - prob$empub[i])^2)
        }
        return(0)
      })),
      sum(ifelse(es_estabs > 0 & es_estabs < 1, (1 - es_estabs)^4, 0)),
      sum(ifelse(es_estabs == 0 & es_emp > 0, es_emp^2, 0))
    )
    
    sum(penalties * c(100, 100, 10, 5, 10, 5)) # Adjusted weights
  },
  eval_g_eq = function(x) {
    es_emp <- x[1:9]
    es_estabs <- ifelse(x[10:18] < 0.5, 0, 0.5 + x[10:18]^2)
    c(sum(es_emp * es_estabs) - prob$emptot,
      sum(es_estabs) - prob$estabs_tot)
  },
  lb = c(prob$emplb, rep(0.01, 9)),
  ub = c(prob$empub, rep(prob$estabs_tot * 3, 9)), # Looser upper bound
  opts = list(
    algorithm = "NLOPT_LN_COBYLA",
    maxeval = 10000, # Balanced iteration count
    xtol_rel = 1e-7,
    ftol_rel = 1e-7,
    print_level = 1
  )
)



# Post-processing with exact constraint satisfaction
final_emp <- res$solution[1:9]
final_estabs <- ifelse(res$solution[10:18] < 0.5, 0, 0.5 + res$solution[10:18]^2)

# Adjust to exactly match totals while preserving ratios
scale_estabs <- prob$estabs_tot/sum(final_estabs)
final_estabs <- final_estabs * scale_estabs

# Then adjust employment
scale_emp <- prob$emptot/sum(final_emp * final_estabs)
final_emp <- pmin(pmax(final_emp * scale_emp, prob$emplb), prob$empub)

# Results
results <- data.frame(
  Group = 1:9,
  Estabs_Initial = prob$estabs_initial,
  Estabs_Final = round(final_estabs, 2),
  Emp_per_Estab = round(final_emp, 2),
  Total_Employment = round(final_estabs * final_emp, 2)
)

# Verification
cat("Total establishments:", sum(results$Estabs_Final), "(Target:", prob$estabs_tot, ")\n")
cat("Total employment:", sum(results$Total_Employment), "(Target:", prob$emptot, ")\n")
print(results)

# start -------------------------------------------------------------------


library(nloptr)

# Problem data
prob <- list(
  emplb = c(0, 5, 10, 20, 50, 100, 250, 500, 1000),
  empub = c(4, 9, 19, 49, 99, 249, 499, 999, 2000),
  estabs_initial = estabs_initial,
  emptot = emptot,
  estabs_tot = estabs_tot,
  weights = c(
    employment_match = 500,  # Increased from 100
    estabs_match = 500,      # Increased from 100
    initial_estabs = 0.5,    # Reduced from 1
    emp_bounds = 5,
    small_estabs = 10,
    emp_without_estabs = 5
  )
)

# Modified objective function with better balancing
# Corrected objective function with proper syntax
obj <- function(x, prob) {
  es_emp <- x[1:9]
  es_estabs <- ifelse(x[10:18] < 0.5, 0, 0.5 + x[10:18]^2) # Smoother transition
  
  es_emptot <- es_emp * es_estabs
  
  penalties <- c(
    (prob$emptot - sum(es_emptot))^2,
    (prob$estabs_tot - sum(es_estabs))^2,
    sum((es_estabs - prob$estabs_initial)^2),
    sum(sapply(1:9, function(i) {
      if (es_estabs[i] > 0) {
        if (es_emp[i] < prob$emplb[i]) return((prob$emplb[i] - es_emp[i])^2)
        if (es_emp[i] > prob$empub[i]) return((es_emp[i] - prob$empub[i])^2)
      }
      return(0)
    })),  # This parenthesis closes the sapply
    sum(ifelse(es_estabs > 0 & es_estabs < 1, (1 - es_estabs)^4, 0)), # Stronger penalty
    sum(ifelse(es_estabs == 0 & es_emp > 0, es_emp^2, 0))
  )  # This parenthesis closes the c() vector
  
  sum(penalties * prob$weights)
}  # This brace closes the function

# COBYLA Corrected and fully working implementation
res <- nloptr(
  x0 = {
    # Calculate initial employment values (midpoints) and enforce bounds
    emp_init <- pmin(pmax(pmean(prob$emplb, prob$empub), prob$emplb), prob$empub)
    
    # Calculate initial establishment values with bounds enforcement
    estabs_init <- pmin(sqrt(pmax(0, prob$estabs_initial - 0.5)), prob$estabs_tot * 2)
    
    # Combine into parameter vector
    c(emp_init, estabs_init)
  },
  eval_f = function(x) obj(x, prob),
  eval_g_eq = function(x) {
    es_emp <- x[1:9]
    es_estabs <- ifelse(x[10:18] < 0.5, 0, 0.5 + x[10:18]^2)
    c(sum(es_emp * es_estabs) - prob$emptot,
      sum(es_estabs) - prob$estabs_tot)
  },
  lb = c(prob$emplb, rep(0, 9)),
  ub = c(prob$empub, rep(prob$estabs_tot * 2, 9)),
  opts = list(
    algorithm = "NLOPT_LN_COBYLA",
    maxeval = 20000,
    xtol_rel = 1e-8,
    print_level = 1
  )
)


# Post-processing with exact constraint satisfaction
final_emp <- res$solution[1:9]
final_estabs <- ifelse(res$solution[10:18] < 0.5, 0, 0.5 + res$solution[10:18]^2)

# Adjust to exactly match totals while preserving ratios
scale_estabs <- prob$estabs_tot/sum(final_estabs)
final_estabs <- final_estabs * scale_estabs

# Then adjust employment
scale_emp <- prob$emptot/sum(final_emp * final_estabs)
final_emp <- pmin(pmax(final_emp * scale_emp, prob$emplb), prob$empub)

# Results
results <- data.frame(
  Group = 1:9,
  Estabs_Initial = prob$estabs_initial,
  Estabs_Final = round(final_estabs, 2),
  Emp_per_Estab = round(final_emp, 2),
  Total_Employment = round(final_estabs * final_emp, 2)
)

# Verification
cat("Total establishments:", sum(results$Estabs_Final), "(Target:", prob$estabs_tot, ")\n")
cat("Total employment:", sum(results$Total_Employment), "(Target:", prob$emptot, ")\n")
print(results)
