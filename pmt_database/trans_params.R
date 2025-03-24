

# deepseek ----

# Complete transformed parameter approach for establishment size distribution estimation

emplb <- c(0, 5, 10, 20, 50, 100, 250, 500, 1000)
empub <- c(4, 9, 19, 49, 99, 249, 499, 999, 2000)

# Problem data
emplb <- c(0, 5, 10, 20, 50, 100, 250, 500, 1000) # lower bounds
empub <- c(4, 9, 19, 49, 99, 249, 499, 999, 2000)  # upper bounds
estabs_initial <- c(59.8, 9.45, 4.72, rep(0, 6))    # initial establishment estimates
estabs_tot <- 74                                    # total establishments
emptot <- 269                                       # total employment



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
emptot <- 2304                                       # total employment



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






# Complete transformed parameter approach for establishment size distribution estimation
library(stats)

# Helper function for midpoint calculation
pmean <- function(lb, ub) {
  (lb + ub) / 2
}


# Weights for penalty components
weights <- c(
  employment_match = 100,  # match total employment
  estabs_match = 100,      # match total establishments
  initial_estabs = 1,      # match initial establishment estimates
  emp_bounds = 5,          # keep employment within bounds
  small_estabs = 10,       # penalize very small establishment counts
  emp_without_estabs = 5   # penalize employment without establishments
)

# Transformed objective function
obj <- function(par, emptot, estabs_tot, estabs_initial, emplb, empub, weights) {
  # Split parameters into employment and establishment components
  es_emp <- par[1:9]
  es_estabs_raw <- par[10:18]
  
  # Transform establishment counts to be either 0 or >=1
  es_estabs <- ifelse(es_estabs_raw < 0.5, 0, 0.5 + es_estabs_raw^2)
  es_emptot <- es_emp * es_estabs
  
  # Calculate penalty components
  penalties <- c(
    # Match total employment exactly
    employment_match = (emptot - sum(es_emptot))^2,
    
    # Match total establishments exactly
    estabs_match = (estabs_tot - sum(es_estabs))^2,
    
    # Soft matching to initial establishment estimates
    initial_estabs = sum((es_estabs - estabs_initial)^2),
    
    # Employment within bounds penalty
    emp_bounds = sum(sapply(1:9, function(i) {
      if (es_estabs[i] > 0) {
        if (es_emp[i] < emplb[i]) return((emplb[i] - es_emp[i])^2)
        if (es_emp[i] > empub[i]) return((es_emp[i] - empub[i])^2)
      }
      return(0)
    })),
    
    # Penalize very small establishment counts (between 0 and 1)
    small_estabs = sum(ifelse(es_estabs > 0 & es_estabs < 1, (1 - es_estabs)^2, 0)),
    
    # Penalize employment without establishments
    emp_without_estabs = sum(ifelse(es_estabs == 0 & es_emp > 0, es_emp^2, 0))
  )
  
  # Calculate weighted total penalty
  sum(penalties * weights)
}

# Initial parameter values
emp_initial <- pmean(emplb, empub)
par0 <- c(emp_initial, sqrt(pmax(0, estabs_initial - 0.5))) # Transform initial estabs

# Run optimization
res <- optim(
  par = par0,
  fn = obj,
  method = "L-BFGS-B",
  lower = c(emplb, rep(0, 9)),      # Employment lower bounds, establishment raw >= 0
  upper = c(empub, rep(Inf, 9)),     # Employment upper bounds, no upper on establishment raw
  control = list(maxit = 1000),
  emptot = emptot,
  estabs_tot = estabs_tot,
  estabs_initial = estabs_initial,
  emplb = emplb,
  empub = empub,
  weights = weights
)

# Process results
final_emp <- res$par[1:9]
final_estabs_raw <- res$par[10:18]
final_estabs <- ifelse(final_estabs_raw < 0.5, 0, 0.5 + final_estabs_raw^2)

# Adjust employment to exactly match total (while respecting bounds)
scale_factor <- emptot / sum(final_emp * final_estabs)
final_emp_adj <- pmin(pmax(final_emp * scale_factor, emplb), empub)

# Create results data frame
results <- data.frame(
  Group = 1:9,
  Emp_LB = emplb,
  Emp_UB = empub,
  Estabs_Initial = estabs_initial,
  Estabs_Final = round(final_estabs, 2),
  Emp_per_Estab_Initial = emp_initial,
  Emp_per_Estab_Final = round(final_emp_adj, 2),
  Total_Employment = round(final_estabs * final_emp_adj, 2)
)

# Verify totals
cat("Total establishments:", sum(results$Estabs_Final), 
    "(Target:", estabs_tot, ")\n")
cat("Total employment:", sum(results$Total_Employment), 
    "(Target:", emptot, ")\n")

# Print results
print(results)



library(stats)

# Helper function for midpoint calculation
pmean <- function(lb, ub) {
  (lb + ub) / 2
}

# Problem data
emplb <- c(0, 5, 10, 20, 50, 100, 250, 500, 1000) # lower bounds
empub <- c(4, 9, 19, 49, 99, 249, 499, 999, 2000)  # upper bounds
estabs_initial <- c(59.8, 9.45, 4.72, rep(0, 6))    # initial establishment estimates
emptot <- 269                                       # total employment
estabs_tot <- 74                                    # total establishments

# Weights for penalty components
weights <- c(
  employment_match = 100,  # match total employment
  estabs_match = 100,      # match total establishments
  initial_estabs = 1,      # match initial establishment estimates
  emp_bounds = 5,          # keep employment within bounds
  small_estabs = 10,       # penalize very small establishment counts
  emp_without_estabs = 5   # penalize employment without establishments
)

# Transformed objective function
obj <- function(par, emptot, estabs_tot, estabs_initial, emplb, empub, weights) {
  # Split parameters into employment and establishment components
  es_emp <- par[1:9]
  es_estabs_raw <- par[10:18]
  
  # Transform establishment counts to be either 0 or >=1
  es_estabs <- ifelse(es_estabs_raw < 0.5, 0, 0.5 + es_estabs_raw^2)
  es_emptot <- es_emp * es_estabs
  
  # Calculate penalty components
  penalties <- c(
    # Match total employment exactly
    employment_match = (emptot - sum(es_emptot))^2,
    
    # Match total establishments exactly
    estabs_match = (estabs_tot - sum(es_estabs))^2,
    
    # Soft matching to initial establishment estimates
    initial_estabs = sum((es_estabs - estabs_initial)^2),
    
    # Employment within bounds penalty
    emp_bounds = sum(sapply(1:9, function(i) {
      if (es_estabs[i] > 0) {
        if (es_emp[i] < emplb[i]) return((emplb[i] - es_emp[i])^2)
        if (es_emp[i] > empub[i]) return((es_emp[i] - empub[i])^2)
      }
      return(0)
    })),
    
    # Penalize very small establishment counts (between 0 and 1)
    small_estabs = sum(ifelse(es_estabs > 0 & es_estabs < 1, (1 - es_estabs)^2, 0)),
    
    # Penalize employment without establishments
    emp_without_estabs = sum(ifelse(es_estabs == 0 & es_emp > 0, es_emp^2, 0))
  )
  
  # Calculate weighted total penalty
  sum(penalties * weights)
}

# Initial parameter values
emp_initial <- pmean(emplb, empub)
par0 <- c(emp_initial, sqrt(pmax(0, estabs_initial - 0.5))) # Transform initial estabs

# Run optimization
res <- optim(
  par = par0,
  fn = obj,
  method = "L-BFGS-B",
  lower = c(emplb, rep(0, 9)),      # Employment lower bounds, establishment raw >= 0
  upper = c(empub, rep(Inf, 9)),     # Employment upper bounds, no upper on establishment raw
  control = list(maxit = 1000),
  emptot = emptot,
  estabs_tot = estabs_tot,
  estabs_initial = estabs_initial,
  emplb = emplb,
  empub = empub,
  weights = weights
)

# Process results
final_emp <- res$par[1:9]
final_estabs_raw <- res$par[10:18]
final_estabs <- ifelse(final_estabs_raw < 0.5, 0, 0.5 + final_estabs_raw^2)

# Adjust employment to exactly match total (while respecting bounds)
scale_factor <- emptot / sum(final_emp * final_estabs)
final_emp_adj <- pmin(pmax(final_emp * scale_factor, emplb), empub)

# Create results data frame
results <- data.frame(
  Group = 1:9,
  Emp_LB = emplb,
  Emp_UB = empub,
  Estabs_Initial = estabs_initial,
  Estabs_Final = round(final_estabs, 2),
  Emp_per_Estab_Initial = emp_initial,
  Emp_per_Estab_Final = round(final_emp_adj, 2),
  Total_Employment = round(final_estabs * final_emp_adj, 2)
)

# Verify totals
cat("Total establishments:", sum(results$Estabs_Final), 
    "(Target:", estabs_tot, ")\n")
cat("Total employment:", sum(results$Total_Employment), 
    "(Target:", emptot, ")\n")

# Print results
print(results)


# transform 2 -------------------------------------------------------------

library(nloptr)

# Problem data
prob <- list(
  emplb = c(0, 5, 10, 20, 50, 100, 250, 500, 1000),
  empub = c(4, 9, 19, 49, 99, 249, 499, 999, 2000),
  estabs_initial = c(47.096774, 22.370968, 3.532258, rep(0, 6)),
  emptot = 339,
  estabs_tot = 73,
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
    }),
    sum(ifelse(es_estabs > 0 & es_estabs < 1, (1 - es_estabs)^4, 0)), # Stronger penalty
    sum(ifelse(es_estabs == 0 & es_emp > 0, es_emp^2, 0))
    )
    
    sum(penalties * prob$weights)
    }

# COBYLA implementation with adjusted weights
res <- nloptr(
  x0 = c(pmean(prob$emplb, prob$empub), sqrt(pmax(0, prob$estabs_initial - 0.5))),
  eval_f = obj,
  eval_g_eq = function(x) {
    es_emp <- x[1:9]
    es_estabs <- ifelse(x[10:18] < 0.5, 0, 0.5 + x[10:18]^2)
    c(sum(es_emp * es_estabs) - prob$emptot,
      sum(es_estabs) - prob$estabs_tot)
  },
  lb = c(prob$emplb, rep(0, 9)),
  ub = c(prob$empub, rep(Inf, 9)),
  opts = list(
    algorithm = "NLOPT_LN_COBYLA",
    maxeval = 20000,  # Increased iterations
    xtol_rel = 1e-8,
    print_level = 1
  ),
  prob = prob
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
