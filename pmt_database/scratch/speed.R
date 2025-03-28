
# final above -------------------------------------------------------------------

# setup
n_workers <- 4
(n_workers <- parallel::detectCores() - 2) # leave 2 free
future::plan(future::multisession, workers=n_workers)

test <- qclong |> 
  mutate(pay_emp = payroll / emp)|> 
  filter(area=="Bronx", naics %in% c("236", "237", "238"))

test <- qclong |> 
  mutate(pay_emp = payroll / emp)

test2 <- test |> 
  nest(.by=c(area:payroll)) 
test2

a <- proc.time()
test3 <- test2 |> 
  mutate(
    # optres = purrr::pmap(
    optres = furrr::future_pmap(
      list(data, estabs, emp),
      \(data, estabs, emp)
      allocate(estabs_initial=data$estabs_initial,
               emptot = emp,
               estabs_tot = estabs),
      .progress = TRUE))
b <- proc.time()
b - a


test4 <- test3 |> 
  filter(row_number() <= 3) |> 
  hoist(data, "estabs_initial") |> 
  unnest(cols=c(estabs_initial, optres))
test4


# final 2 ----

library(dplyr)
library(tidyr)
library(furrr)

# Assuming your data frame is called 'your_data' and has a grouping variable 'group_id'

# Define your compute-intensive function
your_function <- function(data) {
  # Your complex calculations here
  Sys.sleep(0.1) # Simulate a compute-intensive task
  return(data.frame(result = sum(data$some_value)))
}

your_function <- function(data) {
  # Your complex calculations here
  Sys.sleep(0.1) # Simulate a compute-intensive task
  return(data.frame(result = sum(data$some_value)))
}

your_data <- tibble(row=1:1000, group_id=rep(1:20, 50), some_value=sample(1:20, 1000, replace = TRUE))

# Set up the parallel backend
plan(multicore, workers = 32) # Or use 'cluster' for more control

# Process your data in parallel by group
results <- your_data %>%
  group_by(group_id) %>%
  future_map_dfr(your_function)

# Don't forget to reset the plan when you're done!
plan(sequential)


# deepseek ----------------------------------------------------------------

library(dplyr)
library(purrr)
library(furrr)
library(future)

# Set random seed for reproducibility
set.seed(123)

## Step 1: Create example data - 1,000 groups with 18 observations each
create_sample_data <- function() {
  tibble(
    group_id = rep(1:1000, each = 18),
    x = rnorm(1000 * 18),
    y = rnorm(1000 * 18)
  )
}

df <- create_sample_data()

## Step 2: Define a compute-intensive function
# This is a placeholder for your actual function
# It sleeps briefly to simulate computation time
compute_intensive_function <- function(group_data) {
  # Simulate computation time (0.1 seconds per group)
  Sys.sleep(0.1)
  
  # Example computation: fit linear model and return coefficients
  model <- lm(y ~ x, data = group_data)
  
  # Return results as a tibble
  tibble(
    intercept = coef(model)[1],
    slope = coef(model)[2],
    r_squared = summary(model)$r.squared
  )
}

## Step 3: Set up parallel processing
# Use available cores (32 in your case)
available_cores <- availableCores() # or manually set to 32
plan(multisession, workers = available_cores)

## Step 4: Process groups in parallel
process_groups_parallel <- function(data) {
  data %>%
    # Split into groups (nesting creates list-column of data frames)
    group_by(group_id) %>%
    nest() %>%
    
    # Process in parallel with progress bar
    mutate(
      result = future_map(data, compute_intensive_function,
                          .options = furrr_options(seed = TRUE, globals = TRUE),
                          .progress = TRUE)
    ) %>%
    
    # Unnest results
    unnest(result)
}

## Step 5: Run and time the processing
message("Starting parallel processing...")
system.time({
  results <- process_groups_parallel(df)
})

## Step 6: Examine results
head(results)

## Step 7: Clean up parallel workers (important!)
plan(sequential)


# another -----------------------------------------------------------------

library(dplyr)
library(furrr)

# Define your compute-intensive function
your_function <- function(value) {
  # Your complex calculations here
  Sys.sleep(0.1) # Simulate a compute-intensive task
  return(data.frame(result = sum(value)))
}

your_data <- tibble(row = 1:1000, group_id = rep(1:20, 50), some_value = sample(1:20, 1000, replace = TRUE))

# Set up the parallel backend
plan(multicore, workers = 4) # Using 4 for this example, adjust to your cores

# Process your data in parallel by group
results <- your_data %>%
  group_by(group_id) %>%
  future_map_dfr(some_value, your_function)

# Don't forget to reset the plan when you're done!
plan(sequential)

print(results)



# v2 ----------------------------------------------------------------------

library(dplyr)
library(purrr)
library(furrr)
library(future)

# Set random seed for reproducibility
set.seed(123)

## Step 1: Create example data - 1,000 groups with 18 observations each
create_sample_data <- function() {
  tibble(
    group_id = rep(1:1000, each = 18),
    x = rnorm(1000 * 18),
    y = rnorm(1000 * 18)
  )
}

df <- create_sample_data()
df <- df |> filter(row_number() <= 360)

## Step 2: Define a compute-intensive function
# This is a placeholder for your actual function
# It sleeps briefly to simulate computation time
compute_intensive_function <- function(group_data) {
  # Simulate computation time (0.1 seconds per group)
  print("computing...")
  Sys.sleep(0.5)
  
  # Example computation: fit linear model and return coefficients
  model <- lm(y ~ x, data = group_data)
  
  # Return results as a tibble
  tibble(
    intercept = coef(model)[1],
    slope = coef(model)[2],
    r_squared = summary(model)$r.squared
  )
}

## Step 3: Set up parallel processing
# Use available cores (32 in your case)
available_cores <- availableCores() # or manually set to 32
plan(multisession, workers = available_cores)

## Step 4: Process groups in parallel
process_groups_parallel <- function(data) {
  data %>%
    # Split into groups (nesting creates list-column of data frames)
    group_by(group_id) %>%
    nest() %>%
    
    # Process in parallel with progress bar
    mutate(
      result = future_map(data, compute_intensive_function,
                          .options = furrr_options(seed = TRUE, globals = TRUE),
                          .progress = TRUE)
    ) %>%
    
    # Unnest results
    unnest(result)
}

## Step 5: Run and time the processing
message("Starting parallel processing...")
system.time({
  results <- process_groups_parallel(df)
})

## Step 6: Examine results
head(results)

## Step 7: Clean up parallel workers (important!)
plan(sequential)


# v3 ----------------------------------------------------------------------

# Install packages if needed
# install.packages(c("dplyr", "multidplyr", "doParallel"))

library(dplyr)
set.seed(42)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Create Sample Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n_groups <- 1000
obs_per_group <- 9

sim_data <- tibble(
  group_id = rep(1:n_groups, each = obs_per_group),
  x = rnorm(n_groups * obs_per_group),
  y = runif(n_groups * obs_per_group)
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Compute-Intensive Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
complex_calculation <- function(df) {
  # Simulate computation time (0.1 seconds per group)
  Sys.sleep(0.001)
  print("computing...")
  
  # Example complex operations
  model <- lm(y ~ poly(x, 3), data = df)
  tibble(
    group = unique(df$group_id),
    coef1 = coefficients(model)[2],
    coef2 = coefficients(model)[3],
    rsq = summary(model)$r.squared
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Sequential Processing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("Starting sequential processing...\n")
system.time({
  sequential_results <- sim_data %>% 
    group_by(group_id) %>% 
    group_modify(~ complex_calculation(.x))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Parallel Processing (multidplyr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(multidplyr)
cat("\nStarting multidplyr processing...\n")

# Use 30 cores (leaves 2 free)
cluster <- new_cluster(20)
cluster_library(cluster, "dplyr")

system.time({
  parallel_results <- sim_data %>%
    group_by(group_id) %>% 
    partition(cluster) %>%
    group_by(group_id) %>%
    group_modify(~ complex_calculation(.x)) %>%
    collect()
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Parallel Processing (doParallel)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(doParallel)
cat("\nStarting doParallel processing...\n")

cl <- makeCluster(30)
registerDoParallel(cl)

system.time({
  dopar_results <- foreach(
    grp = unique(sim_data$group_id),
    .combine = bind_rows,
    .packages = c("dplyr")
  ) %dopar% {
    sim_data %>%
      filter(group_id == grp) %>%
      complex_calculation()
  }
})

stopCluster(cl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Verify Results & Timings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\nResults validation:")
identical(sequential_results, parallel_results) && 
  identical(sequential_results, dopar_results)

# Expected output on 32-core machine:
# Sequential: ~100 seconds (0.1s/group * 1000 groups)
# Parallel: ~3.5-4.5 seconds (depending on overhead)


# chat v1 -----------------------------------------------------------------

# Load required packages
library(dplyr)
library(furrr)
library(purrr)
library(tibble)

# Set up parallel plan (32 workers, adjust if you have fewer cores)
plan(multisession, workers = 32)

# ---- Step 1: Simulate data ----
set.seed(123)

# Create 1,000 groups each with 18 rows
n_groups <- 1000
n_per_group <- 18

df <- tibble(
  group_id = rep(1:n_groups, each = n_per_group),
  x = rnorm(n_groups * n_per_group),
  y = rnorm(n_groups * n_per_group)
)

# ---- Step 2: Define a compute-intensive function ----
my_fun <- function(group_df) {
  # Simulate compute-intensive task
  Sys.sleep(0.05)  # simulate time-consuming work (e.g., model fitting)
  
  model <- lm(y ~ x, data = group_df)
  tibble(
    group_id = unique(group_df$group_id),
    intercept = coef(model)[1],
    slope = coef(model)[2],
    r_squared = summary(model)$r.squared
  )
}

# ---- Step 3: Split into groups ----
grouped_list <- df %>%
  group_by(group_id) %>%
  group_split()

# ---- Step 4: Run in parallel ----
results <- future_map(grouped_list, my_fun, .progress = TRUE)

# ---- Step 5: Combine results ----
results_df <- bind_rows(results)

# Print first few rows
print(head(results_df))



# chat v2 works -----------------------------------------------------------------

# Load required packages
library(dplyr)
library(furrr)
library(purrr)
library(tibble)

# Set up parallel plan (32 workers, adjust if you have fewer cores)
plan(sequential) # 62 secs
plan(multisession, workers = 2) # 31 secs
plan(multisession, workers = 18) # 9 secs
plan(multisession, workers = 20) # 9.4 secs
plan(multisession, workers = 22) #  9.9 secs
plan(multisession, workers = 24) # 10.5 secs

# ---- Step 1: Simulate data ----
set.seed(123)

# Create 1,000 groups each with 18 rows
n_groups <- 1000
n_per_group <- 18

df <- tibble(
  group_id = rep(1:n_groups, each = n_per_group),
  x = rnorm(n_groups * n_per_group),
  y = rnorm(n_groups * n_per_group)
)

# ---- Step 2: Define a compute-intensive function ----
my_fun <- function(group_df) {
  # Simulate compute-intensive task
  Sys.sleep(0.05)  # simulate time-consuming work (e.g., model fitting)
  
  model <- lm(y ~ x, data = group_df)
  tibble(
    group_id = unique(group_df$group_id),
    intercept = coef(model)[1],
    slope = coef(model)[2],
    r_squared = summary(model)$r.squared
  )
}

# ---- Step 3: Split into groups ----
grouped_list <- df %>%
  group_by(group_id) %>%
  group_split()

# ---- Step 4: Run in parallel ----
system.time(results <- future_map(grouped_list, my_fun, .progress = TRUE))
# 20 5.1 secs
# 2  31.6
# 22


# ---- Step 5: Combine results ----
results_df <- bind_rows(results)

# Print first few rows
print(head(results_df))



# r multidplyr ----
library(multidplyr)

parallel::detectCores() # leave 2 free

cluster <- new_cluster(4)
cluster_library(cluster, "dplyr")

set.seed(123)

# Create 1,000 groups each with 18 rows
n_groups <- 1000
n_per_group <- 18

df <- tibble(
  group_id = rep(1:n_groups, each = n_per_group),
  x = rnorm(n_groups * n_per_group),
  y = rnorm(n_groups * n_per_group)
)

# ---- Step 2: Define a compute-intensive function
my_fun <- function(group_df) {
  # Simulate compute-intensive task
  Sys.sleep(0.05)  # simulate time-consuming work (e.g., model fitting)
  
  model <- lm(y ~ x, data = group_df)
  tibble(
    group_id = unique(group_df$group_id),
    intercept = coef(model)[1],
    slope = coef(model)[2],
    r_squared = summary(model)$r.squared
  )
}

df
df2 <- df |> 
  group_by(group_id) |> 
  partition(cluster)
df2

df3 <- df2 |>  
  summarise(z = mean(x, na.rm = TRUE), n = n())
df3

df4 <- df3 |> 
  collect() |> 
  arrange(group_id)
df4

dfx <- df |>
  group_by(group_id) |> 
  summarise(z = mean(x, na.rm = TRUE), n = n())

