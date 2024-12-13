# 12/13/2024

# This program estimates average wages by establishment size, for a county-naics
# combination.

# The optimization functions are currently deadwood - rough templates that can
# be fixed and fleshed out if we choose a more-complicated approach.

# For the moment, the approach taken is far simpler:

# 1. Get the total annual payroll for the county-naics combination.
# 2. Get the total annual employment for the county-naics combination.
# 3. Calculate the average wage for the county-naics combination.
# 4. Assume a relationship between the average wage overall and the average
#    wage for the largest establishment size group (e.g., the largest-size
#    group has average wages 5% higher than the overall average).
# 5. Calculate the average wage for the county-naics combination, excluding the
#    largest establishment size group.


library(alabama)

obj <- function(avgwage, totavgwage, ...) {
  sum((avgwage - totavgwage)^2)
}

gr <- function(params, n, estknown, threshold, ...) {
  estabs <- params[1:n]
  
  # Initialize gradient vector
  grad <- numeric(2*n)
  
  # Gradient with respect to estabs
  grad[1:n] <- 2 * (ifelse(estabs < threshold, 0, estabs) - estknown)
  
  # Gradient with respect to avgemp
  grad[(n+1):(2*n)] <- 0
  
  return(grad)
}

heq <- function(avgwage, emprange, totap, topwage, ...) {
  # Equality constraints
  # each must equal 0
  return(c(sum(avgwage * emprange) - totap,
         avgwage[length(avgwage)] - topwage))
}

heq.jac <- function(params, n, ...) {
  estabs <- params[1:n]
  avgemp <- params[(n+1):(2*n)]
  
  # Initialize Jacobian matrix (2 constraints x 2n variables)
  jacobian <- matrix(0, nrow = 2, ncol = 2*n)
  
  # Derivatives for first constraint: sum(estabs) - est
  jacobian[1, 1:n] <- 1        # d/d_estabs
  jacobian[1, (n+1):(2*n)] <- 0  # d/d_avgemp
  
  # Derivatives for second constraint: sum(estabs * avgemp) - emp
  jacobian[2, 1:n] <- avgemp         # d/d_estabs
  jacobian[2, (n+1):(2*n)] <- estabs # d/d_avgemp
  
  return(jacobian)
}

hin <- function(avgwage, n, lb_avgwage, ...) {
  return(avgwage[1:(n - 1)] - lb_avgwage)
}

hin.jac <- function(params, n, ...) {
  estabs <- params[1:n]
  avgemp <- params[(n+1):(2*n)]
  
  # Initialize Jacobian matrix (4n constraints x 2n variables)
  jacobian <- matrix(0, nrow = 4*n, ncol = 2*n)
  
  # Derivatives for estabs - lb_estabs
  jacobian[1:n, 1:n] <- diag(n)           # d/d_estabs
  jacobian[1:n, (n+1):(2*n)] <- 0         # d/d_avgemp
  
  # Derivatives for ub_estabs - estabs
  jacobian[(n+1):(2*n), 1:n] <- -diag(n)  # d/d_estabs
  jacobian[(n+1):(2*n), (n+1):(2*n)] <- 0 # d/d_avgemp
  
  # Derivatives for avgemp - lb_avgemp
  jacobian[(2*n+1):(3*n), 1:n] <- 0       # d/d_estabs
  jacobian[(2*n+1):(3*n), (n+1):(2*n)] <- diag(n)  # d/d_avgemp
  
  # Derivatives for ub_avgemp - avgemp
  jacobian[(3*n+1):(4*n), 1:n] <- 0       # d/d_estabs
  jacobian[(3*n+1):(4*n), (n+1):(2*n)] <- -diag(n) # d/d_avgemp
  
  return(jacobian)
}

# main calling function ----
get_avgwage <- function(totap, totemp, emprange, trace=FALSE, itmax=50, eps=1e-7){
  
  n <- length(avgemp)
  
  totavgwage <- totap / totemp
  topwage <- totavgwage * 1.05
  lb_avgwage <- .5 * totavgwage # actual value or zero if not known
  
  totapxtop <- totap - (emprange[n] * topwage)
  avgapxtop <- totapxtop / (sum(emprange) - emprange[n])
  
  start_avgwage <- c(rep(avgapxtop, n -1), topwage)
  
  result <- auglag(par = start_avgwage,
                   fn = obj,
                   # gr = gr,
                   heq = heq,
                   # heq.jac = heq.jac,
                   hin = hin,
                   # hin.jac = hin.jac,
                   # options
                   control.outer = list(trace = trace,
                                        itmax = itmax,
                                        eps=eps),
                   # possible additional arguments to the functions above
                   n = n, 
                   emprange = emprange,
                   totap = totap,
                   totavgwage = totavgwage,
                   topwage = topwage,
                   lb_avgwage = lb_avgwage)
  result
}

# non-optimization approach ----

get_avgwage_no_opt <- function(ap, emp, avgemp, empgroup, top_vsavg=1.05){
  # Calculate average wage for each establishment size group
  # no optimization
  
  n <- length(avgemp)
  ap_tot <- ap[1] # annual payroll
  emp_tot <- emp[1]
  avgwage_tot <- ap_tot / emp_tot
  avgwage_topgroup <- avgwage_tot * top_vsavg # ASSUMED
  emp_topgroup <- empgroup[n]
  emp_xtopgroup <- emp_tot - emp_topgroup
  ap_xtopgroup <- ap_tot - (emp_topgroup * avgwage_topgroup)
  avgwage_xtopgroup <- ap_xtopgroup / emp_xtopgroup
  
  avgwage <- c(rep(avgwage_xtopgroup, n - 1), avgwage_topgroup)
  # sum(avgwage * empgroup)  - ap_tot
  # (sum(avgwage * empgroup)  - ap_tot) / ap_tot
  avgwage
}

