

obj <- function(params, n, estknown) {
  # n <- length(estknown)
  estabs <- params[1:n]
  avgemp <- params[(n+1):(2*n)]
  sum((estabs - estknown)^2)
}

gr <- function(params, n, estknown) {
  # n <- length(estknown)
  estabs <- params[1:n]
  avgemp <- params[(n+1):(2*n)]
  
  # Initialize gradient vector
  grad <- numeric(2*n)
  
  # Gradient with respect to estabs
  grad[1:n] <- 2 * (estabs - estknown)
  
  # Gradient with respect to avgemp
  grad[(n+1):(2*n)] <- 0  # derivative is zero as avgemp doesn't appear in objective
  
  return(grad)
}

heq <- function(params, n, estknown) {
  # Equality constraints
  estabs <- params[1:n]
  avgemp <- params[(n+1):(2*n)]
  
  # each must equal 0
  return(c(
    sum(estabs) - est,        # equality: sum(estabs) = est
    sum(estabs * avgemp) - emp  # equality: sum(estabs * avgemp) = emp
  ))
}

heq.jac <- function(params, n, estknown) {
  # n <- length(params)/2
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

hin <- function(params, n, estknown) {
  estabs <- params[1:n]
  avgemp <- params[(n+1):(2*n)]
  return(c(
    avgemp - lb,
    ub - avgemp
  ))
}

hin.jac <- function(params, n, estknown) {
  # n <- length(params)/2
  estabs <- params[1:n]
  avgemp <- params[(n+1):(2*n)]
  
  # Initialize Jacobian matrix (2n constraints x 2n variables)
  jacobian <- matrix(0, nrow = 2*n, ncol = 2*n)
  
  # Derivatives for first n constraints: avgemp - lb
  jacobian[1:n, 1:n] <- 0                # d/d_estabs
  jacobian[1:n, (n+1):(2*n)] <- diag(n)  # d/d_avgemp
  
  # Derivatives for second n constraints: ub - avgemp
  jacobian[(n+1):(2*n), 1:n] <- 0                # d/d_estabs
  jacobian[(n+1):(2*n), (n+1):(2*n)] <- -diag(n) # d/d_avgemp
  
  return(jacobian)
}

# main calling function ----
call_auglag <- function(est, emp, estknown){
  
  n <- length(estknown)
  
  lb <- c(0, 5, 10, 20, 50, 100, 250, 500, 1000)
  ub <- c(4, 9, 19, 49, 99, 249, 499, 599, 1e9)
  
  lower_bounds <- c(estknown, lb)
  upper_bounds <- c(estknown[1:2], rep(Inf, 7), ub)
  
  start_params <- c(estknown, # initial estabs
                    (lb + ub)/2) # initial avgemp
  
  result <- auglag(par = start_params,
                   fn = obj,
                   gr = gr,
                   heq = heq,
                   heq.jac = heq.jac,
                   hin = hin,
                   hin.jac = hin.jac,
                   n = n, 
                   estknown = estknown)
  result
}


# inputs
emp <- 879
est <- 461
estknown <- c(444, 11, 0, 0, 0, 0, 0, 0, 0)

# call auglag ----
res <- call_auglag(est, emp, estknown)

# examine results
estabs_solution <- res$par[1:n]
avgemp_solution <- res$par[(n+1):(2*n)]

cbind(estknown, estabs_solution) |> kable()
cbind(lb, avgemp_solution, ub) |> kable()
est; sum(estabs_solution)
emp; sum(estabs_solution * avgemp_solution)
# calc max avg emp for top group





lb <- c(0, 5, 10, 20, 50, 100, 250, 500, 1000)
ub <- c(4, 9, 19, 49, 99, 249, 499, 599, 100000)
avgb <- (lb + ub) / 2
n <- length(estknown)

# Define bounds
lower_bounds <- c(estknown, lb)
upper_bounds <- c(estknown[1:2], rep(Inf, 7), ub)
cbind(lower_bounds, start_params, upper_bounds)

start_params <- c(estknown, # initial estabs
                  (lb + ub)/2) # initial avgemp




result <- auglag(par = start_params,
                 fn = obj,
                 gr = gr,
                 heq = heq,
                 heq.jac = heq.jac,
                 hin = hin,
                 hin.jac = hin.jac)

# Return vector of constraint values
# Each element should be >= 0 for inequality constraints
# or = 0 for equality constraints