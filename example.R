# Hand-built starting values
emplb <- c(0, 5, 10, 20, 50, 100, 250, 500, 1000) # employment-per-establishment lower bound for each of 9 groups
empub <- c(4, 9, 19, 49, 99, 249, 499, 999, 2000) # upper bounds

estabs_initial <- c(59.8, 9.45, 4.72, rep(0, 6)) # initial number of establishments
estabs <- estabs_initial

emp_initial <- pmean(emplb, empub) # initial employment-per-establishment
es_emp <- emp_initial

par0 <- c(es_emp, estabs) # 18 parameters: employment-per-establishment x 9, number of establishments x 9

emptot <- 269 # total employment
estabs_tot <- 74 # total number of establishments

weights <- c(
  100,  # part1: match total employment
  100,  # part2: match total establishments
  1,    # part3: match initial guess
  5,    # part4: avoid fractional estabs
  10,   # part5: group 1 avg emp ∈ [1, 4]
  5,    # part6: all group avg emp ∈ bounds
  10,   # part7: mismatched estabs and emp
  50,   # part8: sharp penalty for big emp with tiny estabs
  20    # part9: group 1 freeloading
)

obj <- function(par, emptot, estabs_tot, estabs_initial, emplb, empub, weights) {
  n <- length(par)
  iemp <- 1:(n / 2)
  iestabs <- (n / 2 + 1):n
  
  es_emp <- par[iemp]
  es_estabs <- par[iestabs]
  es_emptot <- es_emp * es_estabs
  
  part1 <- (emptot - sum(es_emptot))^2
  part2 <- (estabs_tot - sum(es_estabs))^2
  part3 <- sum((es_estabs - estabs_initial)^2)
  part4 <- sum((pmax(0, 1 - es_estabs))^2)
  part5 <- es_estabs[1] * ((pmax(0, 1 - es_emp[1]))^2 + (pmax(0, es_emp[1] - 4))^2)
  
  threshold <- 0.5
  part6 <- sum(ifelse(
    es_estabs > threshold,
    (pmax(0, emplb - es_emp)^2 + pmax(0, es_emp - empub)^2),
    0
  ))
  
  part7 <- sum(
    (es_estabs > threshold & es_emp < emplb) * (emplb - es_emp)^2 +
      (es_emp > threshold & es_estabs < 1) * (1 - es_estabs)^2 * es_emp
  )
  
  part8 <- sum(ifelse(
    es_emptot > 2 & es_estabs < 1,
    (pmax(0, 1 - es_estabs))^4 * es_emptot,
    0
  ))
  
  part9 <- (pmax(0, es_estabs[1] - 1) * pmax(0, 1 - es_emptot[1]))^3
  
  total_penalty <- weights[1] * part1 +
    weights[2] * part2 +
    weights[3] * part3 +
    weights[4] * part4 +
    weights[5] * part5 +
    weights[6] * part6 +
    weights[7] * part7 +
    weights[8] * part8 +
    weights[9] * part9
  
  return(total_penalty / 100)  # scaled to avoid line search errors
}

res <- optim(
  par = par0,
  fn = obj,
  method = "L-BFGS-B",
  lower = c(emplb, rep(0, 9)),
  upper = c(empub, rep(Inf, 9)),
  control = list(maxit = 1000),
  emptot = emptot,
  estabs_tot = estabs_tot,
  estabs_initial = estabs_initial,
  emplb = emplb,
  empub = empub,
  weights = weights
)

# Results summary
iemp <- 1:9
itestabs <- 10:18
es_emp <- res$par[iemp]
es_estabs <- res$par[itestabs]
estab_bounds <- tibble(emplb, empub)

estab_bounds |> 
  mutate(estabs_initial=estabs_initial,
         es_estabs=es_estabs, 
         emp_initial=es_emp_initial,
         es_emp=es_emp, 
         totemp=es_emp * es_estabs) |> 
  adorn_totals() |> 
  knitr::kable()


tibble(
  group = 1:9,
  emplb, empub,
  estabs_initial,
  estabs = es_estabs,
  es_emp = es_emp,
  emp_total = es_emp * es_estabs
) %>% 
  bind_rows(tibble(
    group = "TOTAL",
    emplb = NA,
    empub = NA,
    estabs_initial = sum(estabs_initial),
    estabs = sum(es_estabs),
    es_emp = NA,
    emp_total = sum(es_emp * es_estabs)
  ))






















# Hand-built starting values based on user's excellent prototype
emplb <- c(0, 5, 10, 20, 50, 100, 250, 500, 1000)
empub <- c(4, 9, 19, 49, 99, 249, 499, 999, 2000)

estabs_initial <- c(59.8, 9.45, 4.72, rep(0, 6))
estabs <- estabs_initial
es_emp <- c(2.92, 5, 10, 20, 50, 100, 250, 500, 1000)
es_emp <- (emplb + empub) / 2
es_emp_initial <- es_emp
par0 <- c(es_emp, estabs)

emptot <- 269
estabs_tot <- 74

weights <- c(
  1000,  # part1: match total employment
  1000,  # part2: match total establishments
  10,    # part3: match initial guess
  50,    # part4: avoid fractional estabs
  100,   # part5: group 1 avg emp ∈ [1, 4]
  50,    # part6: all group avg emp ∈ [emplb, empub]
  100,   # part7: mismatched estabs and emp
  100,   # part8: group has large emp but tiny estabs
  200    # part9: group 1 has estabs but no emp
)

obj <- function(par, emptot, estabs_tot, estabs_initial, emplb, empub, weights) {
  n <- length(par)
  iemp <- 1:(n / 2)
  iestabs <- (n / 2 + 1):n
  
  es_emp <- par[iemp]
  es_estabs <- par[iestabs]
  es_emptot <- es_emp * es_estabs
  
  part1 <- (emptot - sum(es_emptot))^2
  part2 <- (estabs_tot - sum(es_estabs))^2
  part3 <- sum((es_estabs - estabs_initial)^2)
  part4 <- sum((pmax(0, 1 - es_estabs))^2)
  part5 <- es_estabs[1] * ((pmax(0, 1 - es_emp[1]))^2 + (pmax(0, es_emp[1] - 4))^2)
  threshold <- 0.5
  part6 <- sum(ifelse(
    es_estabs > threshold,
    (pmax(0, emplb - es_emp)^2 + pmax(0, es_emp - empub)^2),
    0
  ))
  part7 <- sum(
    (es_estabs > threshold & es_emp < emplb) * (emplb - es_emp)^2 +
      (es_emp > threshold & es_estabs < 1) * (1 - es_estabs)^2 * es_emp
  )
  part8 <- sum(ifelse(
    es_emptot > 2 & es_estabs < 1,
    (2 - es_estabs)^2 * es_emptot,
    0
  ))
  part9 <- (pmax(0, es_estabs[1] - 1) * pmax(0, 1 - es_emptot[1]))^2
  
  total_penalty <- weights[1] * part1 +
    weights[2] * part2 +
    weights[3] * part3 +
    weights[4] * part4 +
    weights[5] * part5 +
    weights[6] * part6 +
    weights[7] * part7 +
    weights[8] * part8 +
    weights[9] * part9
  return(total_penalty)
}

res <- optim(
  par = par0,
  fn = obj,
  method = "L-BFGS-B",
  lower = c(emplb, rep(0, 9)),
  upper = c(empub, rep(Inf, 9)),
  control = list(maxit = 1000),
  emptot = emptot,
  estabs_tot = estabs_tot,
  estabs_initial = estabs_initial,
  emplb = emplb,
  empub = empub,
  weights = weights
)

# Results summary
iemp <- 1:9
iestabs <- 10:18
es_emp <- res$par[iemp]
es_estabs <- res$par[iestabs]
estab_bounds <- tibble(emplb, empub)

estab_bounds |> 
  mutate(estabs_initial=estabs_initial,
         es_estabs=es_estabs, 
         emp_initial=es_emp_initial,
         es_emp=es_emp, 
         totemp=es_emp * es_estabs) |> 
  adorn_totals() |> 
  knitr::kable()

tibble(
  group = 1:9,
  emplb, empub,
  estabs_initial,
  estabs = es_estabs,
  es_emp = es_emp,
  emp_total = es_emp * es_estabs
) %>% 
  bind_rows(tibble(
    group = "TOTAL",
    emplb = NA,
    empub = NA,
    estabs_initial = sum(estabs_initial),
    estabs = sum(es_estabs),
    es_emp = NA,
    emp_total = sum(es_emp * es_estabs)
  ))



# perplexity --------------------------------------------------------------

library(nloptr)



# Dutchess 523 ----
estabs_initial <- c(59.8, 9.45, 4.72, rep(0, 6))    # initial establishment estimates
estabs_tot <- 74         
emptot <- 269                                       # total employment


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



# Dynamic initialization of parameters
emplb <- c(0, 5, 10, 20, 50, 100, 250, 500, 1000) # lower bounds
empub <- c(4, 9, 19, 49, 99, 249, 499, 999, 2000)  # upper bounds
emp_initial <- pmean(emplb, empub)
estabs_initial <- estabs_initial / sum(estabs_initial) * estabs_tot
emp_initial <- emp_initial / sum(emp_initial * estabs_initial) * emptot

par0 <- c(emp_initial, estabs_initial)

# Objective function with improved scaling and simplifications
obj <- function(par, emptot, estabs_tot, estabs_initial, emplb, empub, weights) {
  n <- length(par)
  iemp <- 1:(n / 2)
  iestabs <- (n / 2 + 1):n
  
  es_emp <- par[iemp]
  es_estabs <- par[iestabs]
  es_emptot <- es_emp * es_estabs
  
  # Penalty components
  part1 <- (emptot - sum(es_emptot))^2 / emptot^2
  part2 <- (estabs_tot - sum(es_estabs))^2 / estabs_tot^2
  part3 <- sum((es_estabs - estabs_initial)^2)
  part4 <- sum((pmax(0, 1 - es_estabs))^2)
  
  part5 <- es_estabs[1] * ((pmax(0, 1 - es_emp[1]))^2 + (pmax(0, es_emp[1] - 4))^2)
  
  part6 <- sum((pmax(0, emplb - es_emp)^2 + pmax(0, es_emp - empub)^2))
  
  threshold <- .5
  part7 <- sum(
    (es_estabs > threshold & es_emp < emplb) * (emplb - es_emp)^2 +
      (es_emp > threshold & es_estabs < 1) * (1 - es_estabs)^2 * es_emp
  )
  
  part8 <- sum(ifelse(
    es_emptot > 2 & es_estabs < 1,
    (pmax(0, 1 - es_estabs))^4 * es_emptot,
    0
  ))
  
  part9 <- (pmax(0, es_estabs[1] - 1) * pmax(0, 1 - es_emptot[1]))^3
  
  # Total penalty scaled appropriately
  total_penalty <- weights[1] * part1 +
    weights[2] * part2 +
    weights[3] * part3 +
    weights[4] * part4 +
    weights[5] * part5 +
    weights[6] * part6 +
    weights[7] * part7 +
    weights[8] * part8 +
    weights[9] * part9
  
  return(total_penalty)
}

# Optimization using nloptr for better constraint handling
weights <- rep(1, 9)
res <- nloptr::nloptr(
  x0 = par0,
  eval_f = obj,
  lb = c(emplb, rep(0, length(estabs_initial))),
  ub = c(empub, rep(estabs_tot, length(estabs_initial))),
  opts = list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1e-6),
  emptot = emptot,
  estabs_tot = estabs_tot,
  estabs_initial = estabs_initial,
  emplb = emplb,
  empub = empub,
  weights = weights
)

# Results and diagnostics
print(res$solution)
print(res$convergence)


