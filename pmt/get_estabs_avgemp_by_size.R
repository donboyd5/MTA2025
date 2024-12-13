library(alabama)

obj <- function(params, n, estknown, threshold, ...) {
  estabs <- params[1:n]
  avgemp <- params[(n+1):(2*n)]
  
  # Apply threshold
  estabs_thresh <- ifelse(estabs < threshold, 0, estabs)
  
  # Original objective with thresholded values
  sum((estabs_thresh - estknown)^2)
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

heq <- function(params, n, est, emp, ...) {
  # Equality constraints
  estabs <- params[1:n]
  avgemp <- params[(n+1):(2*n)]
  
  # each must equal 0
  return(c(
    sum(estabs) - est,        # equality: sum(estabs) = est
    sum(estabs * avgemp) - emp  # equality: sum(estabs * avgemp) = emp
  ))
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

hin <- function(params, n, estknown, lb_estabs, ub_estabs, lb_avgemp, ub_avgemp, ...) {
  estabs <- params[1:n]
  avgemp <- params[(n+1):(2*n)]
  
  return(c(
    estabs - lb_estabs,
    ub_estabs - estabs,
    avgemp - lb_avgemp,
    ub_avgemp - avgemp
  ))
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
get_estabs_avgemp <- function(est, emp, estknown, lb_avgemp, ub_avgemp, threshold=1, trace=FALSE, itmax=50, eps=1e-7){
  
  n <- length(estknown)
  
  # calculate lower and upper bounds for estabs based on estknown
  lb_estabs <- estknown # actual value or zero if not known
  ub_estabs <- estknown
  ub_estabs[estknown == 0] <- Inf
  
  start_params <- c(estknown, # initial estabs
                    lb_avgemp) # initial avgemp -- (lb_avgemp + ub_avgemp) / 2
  
  result <- alabama::auglag(par = start_params,
                   fn = obj,
                   gr = gr,
                   heq = heq,
                   heq.jac = heq.jac,
                   hin = hin,
                   hin.jac = hin.jac,
                   # options
                   control.outer = list(trace = trace,
                                        itmax = itmax,
                                        eps=eps),
                   # possible additional arguments to the functions above
                   n = n, 
                   est = est,
                   emp = emp,
                   estknown = estknown,
                   lb_estabs = lb_estabs,
                   ub_estabs = ub_estabs,
                   lb_avgemp = lb_avgemp,
                   ub_avgemp = ub_avgemp,
                   threshold = threshold)
  result
}

