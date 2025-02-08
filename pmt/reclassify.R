
library(dplyr)


# deep research 1 ----

# Sample original data (extend this with your 20 groups)
original_data <- data.frame(
  firm_size = c("<5 employees", "5-9 employees"),
  lower = c(1, 5),  # Adjusted lower bounds to exclude 0 employees
  upper = c(4, 9),
  num_firms = c(100, 80),
  num_employees = c(200, 480),
  total_payroll = c(10e6, 24e6)
)
original_data

# Function to generate employee counts per firm
generate_employees <- function(N, E, lower, upper) {
  avg <- E / N
  if (avg == floor(avg)) {
    return(rep(avg, N))
  } else {
    e1 <- floor(avg)
    e2 <- ceiling(avg)
    x <- round((E - N * e1) / (e2 - e1))
    employees <- c(rep(e1, N - x), rep(e2, x))
    current_sum <- sum(employees)
    if (current_sum != E) {
      diff <- E - current_sum
      adjustment <- if (diff > 0) 1 else -1
      idx <- 1:abs(diff)
      employees[idx] <- pmax(lower, pmin(upper, employees[idx] + adjustment))
    }
    return(employees)
  }
}

# Generate synthetic firms
synthetic_firms <- data.frame()
for (i in 1:nrow(original_data)) {
  group <- original_data[i, ]
  N <- group$num_firms
  E <- group$num_employees
  lower <- group$lower
  upper <- group$upper
  payroll <- group$total_payroll
  
  employees <- generate_employees(N, E, lower, upper)
  pay_per_employee <- payroll / E
  payroll_per_firm <- employees * pay_per_employee
  
  synthetic_firms <- rbind(synthetic_firms, data.frame(
    employees = employees,
    payroll = payroll_per_firm,
    orig_group = group$firm_size
  ))
}
synthetic_firms

# Define new payroll groups (customize breaks as needed)
payroll_breaks <- c(0, 250000, 500000, Inf)
payroll_labels <- c("<250000", "250000-500000", "500000+")

# Classify into new groups and aggregate
new_payroll_groups <- synthetic_firms %>%
  mutate(payroll_group = cut(payroll, breaks = payroll_breaks, labels = payroll_labels, include.lowest = TRUE)) %>%
  group_by(payroll_group) %>%
  summarise(
    num_firms = n(),
    total_employees = sum(employees),
    total_payroll = sum(payroll),
    .groups = 'drop'
  ) %>%
  mutate(
    avg_payroll = total_payroll / num_firms,
    avg_pay_per_employee = total_payroll / total_employees
  )
new_payroll_groups

# View results
print(new_payroll_groups)

# deep research 2 ----
library(dplyr)

# Sample original data (extend with all 20 groups)
original_data <- data.frame(
  firm_size = c("<5 employees", "5-9 employees"),
  lower = c(1, 5),  # Adjusted to realistic employee counts
  upper = c(4, 9),
  num_firms = c(100, 80),
  num_employees = c(200, 480),
  total_payroll = c(10e6, 24e6)
)

# Function to generate varied employee counts
generate_employees <- function(N, E, lower, upper) {
  employees <- rep(lower, N)
  remaining <- E - (N * lower)
  
  while(remaining > 0) {
    candidates <- which(employees < upper)
    selected <- sample(candidates, 1)
    employees[selected] <- employees[selected] + 1
    remaining <- remaining - 1
  }
  return(employees)
}

# Function to generate varied payrolls
generate_payroll <- function(employees, total_payroll) {
  base_ppe <- total_payroll / sum(employees)
  variation <- rlnorm(length(employees), meanlog = 0, sdlog = 0.2)  # 20% variation
  raw_payroll <- employees * base_ppe * variation
  scaling_factor <- total_payroll / sum(raw_payroll)
  return(raw_payroll * scaling_factor)
}

# Generate synthetic dataset
synthetic_firms <- data.frame()
for(i in 1:nrow(original_data)) {
  group <- original_data[i, ]
  
  # Generate employee counts with variation
  employees <- generate_employees(
    N = group$num_firms,
    E = group$num_employees,
    lower = group$lower,
    upper = group$upper
  )
  
  # Generate payrolls with controlled variation
  payroll <- generate_payroll(
    employees = employees,
    total_payroll = group$total_payroll
  )
  
  synthetic_firms <- rbind(synthetic_firms, data.frame(
    employees = employees,
    payroll = payroll,
    orig_group = group$firm_size
  ))
}

# Define new payroll groups (customize as needed)
payroll_breaks <- c(0, 250000, 500000, Inf)
payroll_labels <- c("<250k", "250k-500k", "500k+")

# Aggregate into new groups
new_groups <- synthetic_firms %>%
  mutate(payroll_group = cut(payroll, payroll_breaks, labels = payroll_labels, include.lowest = TRUE)) %>%
  group_by(payroll_group) %>%
  summarise(
    num_firms = n(),
    total_employees = sum(employees),
    total_payroll = sum(payroll),
    .groups = 'drop'
  ) %>%
  mutate(
    avg_payroll = total_payroll / num_firms,
    avg_ppe = total_payroll / total_employees
  )

# Verification checks
original_total <- colSums(original_data[, c("num_firms", "num_employees", "total_payroll")])
new_total <- colSums(new_groups[, c("num_firms", "total_employees", "total_payroll")])

cat("Original totals:\n")
print(original_total)
cat("\nNew totals:\n")
print(new_total)


# my deep research approach ----

mta <- readRDS(fs::path(PDINTERMEDIATE, "susb2021_nysmta_shares_adj.rds"))
glimpse(mta)

data <- mta |> 
  filter(cofips == "005", naics == "23") |>
  select(cofips, county, naics, description, firmsize, fslb, fsub, firms=firms_adj, emp, payroll)

data  

generate_employees <- function(N, E, lower, upper) {
  employees <- rep(lower, N)
  remaining <- E - (N * lower)
  
  while(remaining > 0) {
    candidates <- which(employees < upper)
    selected <- sample(candidates, 1)
    employees[selected] <- employees[selected] + 1
    remaining <- remaining - 1
  }
  return(employees)
}

generate_payroll <- function(employees, total_payroll) {
  base_ppe <- total_payroll / sum(employees)
  variation <- rlnorm(length(employees), meanlog = 0, sdlog = 0.2)  # 20% variation
  raw_payroll <- employees * base_ppe * variation
  scaling_factor <- total_payroll / sum(raw_payroll)
  return(raw_payroll * scaling_factor)
}

# Generate synthetic dataset
synthetic_firms <- tibble()
for(i in 1:nrow(data)) {
  group <- data[i, ]
  
  # Generate employee counts with variation
  employees <- generate_employees(
    N = group$firms,
    E = group$emp,
    lower = group$fslb,
    upper = group$fsub
  )
  
  # Generate payrolls with controlled variation
  payroll <- generate_payroll(
    employees = employees,
    total_payroll = group$payroll
  )
  
  synthetic_firms <- bind_rows(
    synthetic_firms, 
    tibble(
      employees = employees,
      payroll = payroll,
      orig_group = group$firmsize)
    )
}

data |> 
  summarise(across(c(firms, emp, payroll), sum))

synthetic_firms |> 
  summarise(across(c(employees, payroll), sum))


count(synthetic_firms, orig_group, employees)
count(synthetic_firms, orig_group)

synthetic_firms |> 
  ggplot(aes(employees, payroll)) +
  geom_point(aes(color = orig_group))

# Define new payroll groups (customize as needed)
# 
payroll_breaks <- c(0, 1.5e6, 1.75e6, Inf)
payroll_labels <- c("<1.5m", "$1.5m-1.75m", "$1.75m")
cbind(payroll_labels, payroll_breaks)

payroll_breaks <- c(0, 0.5e6, 1e6, 1.5e6, 1.75e6, 2e6, Inf)
payroll_labels <- c("< 0.5m", "$0.5 - 1m", "$1m - $1.5m", "$1.5m - 1.75m", "$1.75m - 2m", "2m")
cbind(payroll_labels, payroll_breaks)

# Aggregate into new groups
new_groups <- synthetic_firms |> 
  mutate(payroll_group = cut(payroll, payroll_breaks, labels = payroll_labels, include.lowest = TRUE)) |> 
  summarise(
    num_firms = n(),
    total_employees = sum(employees),
    total_payroll = sum(payroll),
    minpay_firm = min(payroll),
    maxpay_firm = max(payroll),
    .by=payroll_group
    ) |>
  adorn_totals() |> 
  mutate(
    avgpay_firm = total_payroll / num_firms,
    avgpay_emp = total_payroll / total_employees
    )
new_groups

data |> 
  summarise(across(c(firms, emp, payroll), sum))


# old below here ----

# Sample original data (extend with all 20 groups)
original_data <- data.frame(
  firm_size = c("<5 employees", "5-9 employees"),
  lower = c(1, 5),  # Adjusted to realistic employee counts
  upper = c(4, 9),
  num_firms = c(100, 80),
  num_employees = c(200, 480),
  total_payroll = c(10e6, 24e6)
)

# Function to generate varied employee counts
generate_employees <- function(N, E, lower, upper) {
  employees <- rep(lower, N)
  remaining <- E - (N * lower)
  
  while(remaining > 0) {
    candidates <- which(employees < upper)
    selected <- sample(candidates, 1)
    employees[selected] <- employees[selected] + 1
    remaining <- remaining - 1
  }
  return(employees)
}

# Function to generate varied payrolls
generate_payroll <- function(employees, total_payroll) {
  base_ppe <- total_payroll / sum(employees)
  variation <- rlnorm(length(employees), meanlog = 0, sdlog = 0.2)  # 20% variation
  raw_payroll <- employees * base_ppe * variation
  scaling_factor <- total_payroll / sum(raw_payroll)
  return(raw_payroll * scaling_factor)
}

# Generate synthetic dataset
synthetic_firms <- data.frame()
for(i in 1:nrow(original_data)) {
  group <- original_data[i, ]
  
  # Generate employee counts with variation
  employees <- generate_employees(
    N = group$num_firms,
    E = group$num_employees,
    lower = group$lower,
    upper = group$upper
  )
  
  # Generate payrolls with controlled variation
  payroll <- generate_payroll(
    employees = employees,
    total_payroll = group$total_payroll
  )
  
  synthetic_firms <- rbind(synthetic_firms, data.frame(
    employees = employees,
    payroll = payroll,
    orig_group = group$firm_size
  ))
}

# Define new payroll groups (customize as needed)
payroll_breaks <- c(0, 250000, 500000, Inf)
payroll_labels <- c("<250k", "250k-500k", "500k+")

# Aggregate into new groups
new_groups <- synthetic_firms %>%
  mutate(payroll_group = cut(payroll, payroll_breaks, labels = payroll_labels, include.lowest = TRUE)) %>%
  group_by(payroll_group) %>%
  summarise(
    num_firms = n(),
    total_employees = sum(employees),
    total_payroll = sum(payroll),
    .groups = 'drop'
  ) %>%
  mutate(
    avg_payroll = total_payroll / num_firms,
    avg_ppe = total_payroll / total_employees
  )

# Verification checks
original_total <- colSums(original_data[, c("num_firms", "num_employees", "total_payroll")])
new_total <- colSums(new_groups[, c("num_firms", "total_employees", "total_payroll")])
