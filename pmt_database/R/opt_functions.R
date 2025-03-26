
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

constraint_function <- function(x, prob) {
  es_emp <- x[1:9]
  es_estabs <- ifelse(x[10:18] < 0.5, 0, 0.5 + x[10:18]^2)
  c(sum(es_emp * es_estabs) - prob$emptot,
    sum(es_estabs) - prob$estabs_tot)
}

setup_problem <- function(estabs_initial, emptot, estabs_tot){
  prob <- list(
    emplb = c(0, 5, 10, 20, 50, 100, 250, 500, 1000),
    empub = c(4, 9, 19, 49, 99, 249, 499, 999, 10000),
    estabs_initial = estabs_initial,
    emptot = emptot,
    estabs_tot = estabs_tot,
    # penalties: emptot, estabstot, estabs_initial, bounds, small estabs, emp-without-estabs
    weights = c(50, 100, 20, 5, 10, 5)
  )
  return(prob)
}

initialize <- function(prob){
  x0 <- local({
    # local environment -- variables defined here are not visible from the outside
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
  return(x0)
}

prob_solve <- function(prob, x0){
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
  return(res)
}

post_process <- function(prob, res){
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
  return(final)
}

allocate <- function(estabs_initial, emptot, estabs_tot){
  
  if(emptot == 0 | estabs_tot == 0) return(tibble(gestabs=estabs_initial, gemp_est=0))
  
  prob <- setup_problem(estabs_initial, emptot, estabs_tot)
  x0 <- initialize(prob)
  res <- prob_solve(prob, x0)
  final <- post_process(prob, res)
  return(tibble(gestabs=final$estabs, gemp_est=final$emp, res=list(res)))
}



