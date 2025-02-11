

# gemini ----
# 
# Parameters
n_numbers <- 1000  # Number of random numbers to generate
lower_bound <- 5   # Lower bound of the range
upper_bound <- 9   # Upper bound of the range
target_sum <- 7000 # Desired sum of the generated numbers

# 1. Probability Density Function (PDF)
# We'll use an exponential decay function for the PDF.  This gives us
# a monotonically decreasing probability, and we can control the rate.

# 'decay_rate' controls how quickly the probability decreases.
# Larger values mean faster decay.  Experiment with this.
decay_rate <- 0.5  # Example decay rate. Adjust as needed.

# Function to calculate the probability at a given value 'x'
pdf <- function(x) {
  if (x < lower_bound || x > upper_bound) {
    return(0) # Probability is zero outside the bounds
  }
  exp(-decay_rate * (x - lower_bound)) # Exponential decay
}


# 2. Generating the Random Numbers

# Rejection sampling:
generated_numbers <- c()
while (length(generated_numbers) < n_numbers) {
  # Generate a random x and y within a bounding box
  x <- runif(1, lower_bound, upper_bound)
  y <- runif(1, 0, pdf(lower_bound)) # Max y is at lower_bound
  
  # Accept the x value with probability proportional to pdf(x)
  if (y <= pdf(x)) {
    generated_numbers <- c(generated_numbers, x)
  }
}



# 3. Scaling to Match the Target Sum

current_sum <- sum(generated_numbers)
scaling_factor <- target_sum / current_sum
scaled_numbers <- generated_numbers * scaling_factor

# 4. Ensuring Numbers are Within Bounds (Ad Hoc Adjustment)
# It's *possible* that scaling could push some numbers slightly outside
# the bounds due to floating-point issues.  This is a simple fix:

scaled_numbers <- pmax(scaled_numbers, lower_bound) # Ensure >= lower
scaled_numbers <- pmin(scaled_numbers, upper_bound) # Ensure <= upper

# 5. Verification
print(paste("Sum of generated numbers:", sum(scaled_numbers)))
print(paste("Min:", min(scaled_numbers)))
print(paste("Max:", max(scaled_numbers)))

# 6. Plotting (Optional)
hist(scaled_numbers, breaks = 50, main = "Distribution of Generated Numbers",
     xlab = "Value", col = "lightblue")

# Example of how to adjust decay rate for a steeper curve.
decay_rate_steep <- 1.0  # Higher decay rate
pdf_steep <- function(x) {
  if (x < lower_bound || x > upper_bound) {
    return(0)
  }
  exp(-decay_rate_steep * (x - lower_bound))
}

# Example of how to adjust decay rate for a shallower curve.
decay_rate_shallow <- 0.2  # Lower decay rate
pdf_shallow <- function(x) {
  if (x < lower_bound || x > upper_bound) {
    return(0)
  }
  exp(-decay_rate_shallow * (x - lower_bound))
}

# Example of plotting the PDF
x_values <- seq(lower_bound, upper_bound, length.out = 100)
plot(x_values, pdf(x_values), type = "l", xlab = "x", ylab = "PDF",
     main = "Probability Density Function")
lines(x_values, pdf_steep(x_values), col = "red") # Steeper curve
lines(x_values, pdf_shallow(x_values), col = "blue") # Shallower curve
legend("topright", legend = c("Original", "Steeper", "Shallower"),
       col = c("black", "red", "blue"))



# chatgpt -----------------------------------------------------------------

set.seed(123)  # For reproducibility

# Parameters
n <- 1000       # Number of values
lower <- 5      # Lower bound
upper <- 9      # Upper bound
target_sum <- 7000  # Target sum
alpha <- 0.5    # Controls the steepness of the decline (adjust to control rate)

# Step 1: Generate beta-distributed values scaled to [5, 9]
raw_values <- qbeta(runif(n), alpha, 2) * (upper - lower) + lower

# Step 2: Scale values to match the target sum
scaled_values <- raw_values * (target_sum / sum(raw_values))

# Step 3: Ensure all values are within bounds and rescale if needed
scaled_values <- pmin(pmax(scaled_values, lower), upper)
scaled_values <- scaled_values * (target_sum / sum(scaled_values))
sum(scaled_values)

# Step 4: Visualize the distribution
library(ggplot2)
ggplot(data.frame(x = scaled_values), aes(x)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Generated Values", x = "Value", y = "Frequency")


# Claude exponential ----
# First approach using exponential distribution
generate_declining_sum <- function(n, min_val, max_val, target_sum, decay_rate = 1) {
  # Generate initial numbers using exponential distribution
  x <- rexp(n, rate = decay_rate)
  
  # Transform to desired range
  range_size <- max_val - min_val
  x <- min_val + range_size * (1 - exp(-x)/max(exp(-x)))
  
  # Scale to achieve target sum
  scale_factor <- target_sum/sum(x)
  x <- x * scale_factor
  
  # Adjust any values outside bounds
  x[x < min_val] <- min_val
  x[x > max_val] <- max_val
  
  # Final scaling to hit target sum exactly
  remaining_adjustment <- target_sum/sum(x)
  x <- x * remaining_adjustment
  
  # Example usage
  set.seed(123)  # For reproducibility
  result <- generate_declining_sum(
    n = 1000,          # Number of values to generate
    min_val = 5,       # Minimum value
    max_val = 9,       # Maximum value
    target_sum = 7000, # Target sum
    decay_rate = 0.5   # Controls how quickly probability decreases
  )
  
  # Verification code
  print(summary(result))
  print(sum(result))
  hist(result, breaks = 50, main = "Distribution of Generated Values")
  
  return(x)
}

set.seed(123) # For reproducibility
result <- generate_declining_sum(
  n = 1000,          # Number of values to generate
  min_val = 5,       # Minimum value
  max_val = 9,       # Maximum value
  target_sum = 7000, # Target sum
  decay_rate = 0.5   # Controls how quickly probability decreases
)


# chatgpt2 ----------------------------------------------------------------

set.seed(123)  # For reproducibility

# Parameters
n <- 1000       # Number of values
lower <- 5      # Lower bound
upper <- 9      # Upper bound
target_sum <- 7000  # Target sum
alpha <- 0.3    # Adjust for a steeper decline
beta <- 1.5     # Adjust to reduce density near upper bound
exponent <- 3   # Controls how fast the density falls off

# Step 1: Generate beta-distributed values scaled to [5, 9]
raw_values <- qbeta(runif(n), alpha, beta) * (upper - lower) + lower

# Step 2: Apply a transformation to reduce density near the upper bound
transformed_values <- lower + (raw_values - lower) ^ (1 / exponent) * (upper - lower)

# Step 3: Scale values to match the target sum
scaled_values <- transformed_values * (target_sum / sum(transformed_values))

# Step 4: Ensure all values are within bounds
scaled_values <- pmin(pmax(scaled_values, lower), upper)
scaled_values <- scaled_values * (target_sum / sum(scaled_values))

# Step 5: Visualize the distribution
library(ggplot2)
ggplot(data.frame(x = scaled_values), aes(x)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Generated Values", x = "Value", y = "Frequency")


# deepseek ----------------------------------------------------------------

generate_adjusted_samples <- function(n, lower, upper, target_sum, k, max_iter = 10) {
  # Generate initial samples from power law distribution
  U <- runif(n)
  x <- upper - (upper - lower) * U^(1 / (k + 1))
  
  # Scale to target sum
  current_sum <- sum(x)
  x_scaled <- x * (target_sum / current_sum)
  
  # Clip values to bounds
  x_clipped <- pmin(pmax(x_scaled, lower), upper)
  current_sum_clipped <- sum(x_clipped)
  difference <- target_sum - current_sum_clipped
  
  iter <- 0
  while (abs(difference) > 1e-10 && iter < max_iter) {
    iter <- iter + 1
    
    is_clipped <- x_clipped == lower | x_clipped == upper
    x_unclipped <- x_clipped[!is_clipped]
    
    if (length(x_unclipped) == 0) {
      stop("All values are clipped; cannot adjust further.")
    }
    
    # Adjust unclipped values proportionally
    scaling_adj <- (sum(x_unclipped) + difference) / sum(x_unclipped)
    x_adj <- x_unclipped * scaling_adj
    
    # Re-clip adjusted values
    x_adj <- pmin(pmax(x_adj, lower), upper)
    
    # Update the clipped vector
    x_clipped[!is_clipped] <- x_adj
    
    # Recalculate difference
    current_sum_clipped <- sum(x_clipped)
    difference <- target_sum - current_sum_clipped
  }
  
  return(x_clipped)
}

# Example usage
set.seed(123)
n <- 1000
lower <- 5
upper <- 9
target_sum <- 7000
k <- 0.3  # Adjust k (between 0 and 1) to control decline rate

samples <- generate_adjusted_samples(n, lower, upper, target_sum, k)

# Verify results
sum(samples)  # Should be 7000
sum(samples < lower)  # Should be 0
sum(samples > upper)  # Should be 0

# Plot histogram
hist(samples, breaks = 30, main = "Histogram of Generated Samples", xlab = "Value")



# decay -------------------------------------------------------------------

generate_decaying_numbers <- function(n, min_val, max_val, target_sum, decay_rate = 1) {
  # Generate from exponential distribution
  x <- rexp(n, rate = decay_rate)
  
  # Transform to desired range
  x <- min_val + (max_val - min_val) * (1 - exp(-x)/max(exp(-x)))
  
  # Scale to target sum
  scale_factor <- target_sum/sum(x)
  x <- x * scale_factor
  
  # Ensure bounds are respected
  x[x < min_val] <- min_val
  x[x > max_val] <- max_val
  
  # Final scaling to hit target sum
  x <- x * (target_sum/sum(x))
  
  return(x)
}

# Generate 1000 numbers between 5 and 9 summing to 7000
result <- generate_decaying_numbers(
  n = 1000,
  min_val = 5,
  max_val = 9,
  target_sum = 7000,
  decay_rate = 0.5
)

# Verify results
hist(result, breaks = 50, main = "Distribution of Generated Values")

# deep 2 ----
generate_smooth_samples <- function(n, lower, upper, target_sum, alpha, beta, max_iter = 20, damping = 0.5) {
  # Generate beta-distributed samples (bounded in [0,1])
  x_beta <- rbeta(n, alpha, beta)
  
  # Map to desired range [lower, upper]
  x <- lower + (upper - lower) * x_beta
  
  # Initial scaling to target sum
  current_sum <- sum(x)
  x_scaled <- x * (target_sum / current_sum)
  
  # Clip values to bounds
  x_clipped <- pmin(pmax(x_scaled, lower), upper)
  current_sum_clipped <- sum(x_clipped)
  difference <- target_sum - current_sum_clipped
  
  iter <- 0
  while (abs(difference) > 1e-8 && iter < max_iter) {
    iter <- iter + 1
    
    # Calculate adjustment weights based on headroom from bounds
    headroom <- ifelse(difference > 0,
                       upper - x_clipped,   # Can increase
                       x_clipped - lower)   # Can decrease
    
    # Avoid zero weights (to prevent division by zero)
    headroom <- pmax(headroom, 1e-8)
    
    # Normalize weights to sum to 1
    weights <- headroom / sum(headroom)
    
    # Apply damped adjustment to avoid overshooting
    adjustment <- damping * difference * weights
    
    # Adjust values
    x_clipped <- x_clipped + adjustment
    
    # Re-clip to bounds
    x_clipped <- pmin(pmax(x_clipped, lower), upper)
    
    # Update difference
    current_sum_clipped <- sum(x_clipped)
    difference <- target_sum - current_sum_clipped
  }
  
  return(x_clipped)
}

# Example usage
set.seed(123)
n <- 1000
lower <- 5
upper <- 9
target_sum <- 7000

# Beta parameters (alpha=3, beta=5 creates a smooth right-skewed decay)
alpha <- 3
beta <- 5

samples <- generate_smooth_samples(n, lower, upper, target_sum, alpha, beta)

# Verify results
sum(samples)    # Should be ~7000
range(samples)  # Should be within [5, 9]

# Plot histogram (smoother decline)
hist(samples, breaks = 30, main = "Smoothly Decaying Samples", xlab = "Value", col = "skyblue")


# deep 4 ------------------------------------------------------------------

library(ggplot2)

# Function to generate exponentially decaying samples between [lower, upper]
generate_exponential_samples <- function(n, lower, upper, decay_rate) {
  # Inverse transform sampling for truncated exponential
  U <- runif(n)  # Uniform random numbers
  span <- upper - lower
  
  # Inverse CDF formula for truncated exponential
  samples <- lower - (1 / decay_rate) * log(1 - U * (1 - exp(-decay_rate * span)))
  
  return(samples)
}

# Example usage
set.seed(123)
n <- 1000
lower <- 5
upper <- 9
decay_rate <- 0.5  # Controls steepness of decay (larger = steeper decay)

# Generate samples
samples <- generate_exponential_samples(n, lower, upper, decay_rate)

sum(samples)    # Should be ~7000
range(samples)  # Should be within [5, 9]

target <- 7000
(error <- sum(samples) - target)
# add 1 to the first 639

samples2 <- c(samples[1:639] + 1, samples[640:1000])
sum(samples2)
range(samples2)

# Plot the distribution
ggplot(data.frame(value = samples2), aes(x = value)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  labs(title = "Exponentially Decaying Distribution", x = "Value", y = "Count")








set.seed(123)  # For reproducibility

# Define the possible firm sizes and assign higher probabilities to smaller sizes
firm_sizes <- 10:19
prob_weights <- rev(seq(1, 2, length.out = length(firm_sizes)))  # More weight to smaller sizes
prob_weights <- prob_weights / sum(prob_weights)  # Normalize to sum to 1

# Sample firm sizes ensuring the sum of employees equals 2,700 and exactly 180 firms
synthetic_firms <- numeric(180)
while (sum(synthetic_firms) != 2700) {
  synthetic_firms <- sample(firm_sizes, size = 180, replace = TRUE, prob = prob_weights)
}

for(i in 1:10){
  synthetic_firms <- sample(firm_sizes, size = 180, replace = TRUE, prob = prob_weights)
}



# Summary
table(synthetic_firms)
length(synthetic_firms)
sum(synthetic_firms)
hist(synthetic_firms)


# Check results
head(synthetic_firms)


# deep a ----
# # Set seed for reproducibility
set.seed(123)

# Define firm size range and weights favoring smaller sizes
sizes <- 10:19
weights <- 20 - sizes  # Weights decrease from 10 to 1

# Normalize weights to probabilities
prob <- weights / sum(weights)

# Sample 180 firms based on probabilities
firm_sizes <- sample(sizes, size = 180, replace = TRUE, prob = prob)
table(firm_sizes)

# Calculate the initial total employees
total_employees <- sum(firm_sizes)

# Adjust the total to exactly 2700 without violating size constraints
while (total_employees != 2700) {
  if (total_employees < 2700) {
    # Increase a firm's size that's below 19
    candidates <- which(firm_sizes < 19)
    idx <- sample(candidates, 1)
    firm_sizes[idx] <- firm_sizes[idx] + 1
    total_employees <- total_employees + 1
  } else {
    # Decrease a firm's size that's above 10
    candidates <- which(firm_sizes > 10)
    idx <- sample(candidates, 1)
    firm_sizes[idx] <- firm_sizes[idx] - 1
    total_employees <- total_employees - 1
  }
}

# Display the distribution of firm sizes
firm_distribution <- table(factor(firm_sizes, levels = sizes))
print("Firm Size Distribution:")
print(firm_distribution)
hist(firm_sizes)

# Verify totals
cat("\nTotal number of firms:", sum(firm_distribution), "\n")
cat("Total number of employees:", sum(firm_sizes), "\n")



# djb 1 -------------------------------------------------------------------



group

# deep a ----
set.seed(123)
firmsizes <- 10:19
nfirms_target <- 180
nemployees_target <- 2700
nrecs <- 40

set.seed(123)
firmsizes <- 5:9
nfirms_target <- 179
nemployees_target <- 1262
nrecs <- 20

(probs <- seq(1, .4, length.out = length(firmsizes)))
(weights <- probs / sum(probs))
sum(weights)
weights
recwt <- nfirms_target / nrecs
recwt

firm_sizes <- sample(firmsizes, size = nrecs, replace = TRUE, prob = weights)
table(firm_sizes)
totemp <- sum(firm_sizes) * recwt
totemp


while (totemp != nemployees_target) {
  # resample as needed to get proper employment
  if (totemp < nemployees_target) {
    # Increase a firm's size that's below max firm size
    # number of recs for which we want to add 1
    need <- floor((nemployees_target - totemp) / recwt)
    candidates <- which(firm_sizes < max(firmsizes))
    draw <- min(need, length(candidates))
    if(draw == 0) break
    idx <- sample(candidates, draw, replace = FALSE)
    firm_sizes[idx] <- firm_sizes[idx] + 1
    totemp <- sum(firm_sizes) * recwt
    totemp
  } else {
    # Decrease a firm's size that's above 10
    candidates <- which(firm_sizes > min(firmsizes))
    idx <- sample(candidates, 1)
    firm_sizes[idx] <- firm_sizes[idx] - 1
    total_employees <- sum(firm_sizes) * recwt
  }
}


df <- tibble(firm = 1:nrecs, firmsize = firm_sizes, weight=recwt) |> 
  mutate(wtdemp = firmsize * weight) |> 
  arrange(firmsize)
df |> 
  summarise(n=n(),
            nfirms=sum(weight), emp=sum(firmsize * weight),
            .by=firmsize) |> 
  adorn_totals()



# djb rebalance -----------------------------------------------------------

# calc average employment per firm in each new cell
# distribute firms to cells with equal number per cell
# calc total firms (should be fine)
# calc total employment, likely higher than target (too many large firms)
# calc need vs target (e.g., 70 fewer needed)
# calc net impact of moving 1 firm from highest to lowest, (3.5 here)
# calc net impact of moving half firms from highest to lowest (63 here)
# move up to that amount
# if more to go (7 here) calc net impact of moving 1 firm from 2nd highest to 2nd lowest (2 here)
# calc net impact of moving half firms from 2nd highest to 2nd lowest ( here)


abovemid_index <- function(n) {
  # calculate the movement of employees from high groups to low groups
  ceiling(n/2) + 1L
}

test
20: 500
test2 <- test |> 
  mutate(fscode = ifelse(as.integer(fscode) >= 20, "99", fscode),
         firmsize = ifelse(fscode=="99", "99: 500+ employees", firmsize)) |> 
  summarise(across(
    c(estab, avgemp, totwage, annavgsal), \(x) first(x)),
    fslb=first(fslb),
    fsub=last(fsub),
    across(c(firms, estabs, empadj, payrolladj),
           \(x) sum(x)),
    .by=c(area, year, owner, naics, title, fscode, firmsize))
test2 |> 
  janitor::adorn_totals() |> 
  mutate(ppf=payrolladj / firms)

rec <- 20
group <- test[rec, ]
group



f <- function(x){
  tibble(nfirms = sample(5:9, size = x, replace = TRUE))
}

test3 <- test2 |> 
  mutate(nrecs = case_when(fscode %in% c("03", "04") ~ 20,
                           .default = 1))

# data = pmap(
#   list(nrecs, fslb, fsub), 
#   \(n, lb, ub) tibble(nfirms = sample(lb:ub, size = n, replace = TRUE))

f2 <- function(n, lb, ub, firms, emp, payroll_grp){
  # created weighted records where each rec represents a set of firms
  
  # the following are scalars
  ppf <- payroll_grp / firms
  ppe <- payroll_grp / emp
  epf <- emp / firms
  
  # adjust bounds as needed
  
  # lower bounds
  if(lb == 0) lb <- 1 # don't need to mention this
  
  if(epf < 1.1 * lb){ # case where average employment is close to or below the stated lower limit
    print("Adjusting lower bound...")
    # print(lb)
    lb <- min(round(.8 * epf), lb) # note hard-coded 80% of average employment
    # print(lb)
  }
  
  # infinite bounds
  if(is.infinite(ub)) ub <- 2 * lb # note hard-coded 2x the lower bound
  
  nemp_pf <- sample(lb:ub, size = n, replace = TRUE)
  nfirms <- firms / n # firms per record
  # scale employment so that the total is correct
  scale <- emp / sum(nemp_pf * nfirms)
  nemp_pf <- nemp_pf * scale # scaled employment per firm
  payroll_pf <- nemp_pf * ppe
  tibble(nfirms, lb, ub, nemp_pf, payroll_pf) # one record per firm
}

test3 <- test2 |> 
  mutate(nrecs = case_when(as.integer(fscode) %in% 2:18 ~ 20,
                           fscode == "99" ~ 20,
                           .default = 1)) |> 
  mutate(
    data = pmap(
      list(nrecs, fslb, fsub, firms, empadj, payrolladj),
      \(n, lb, ub, firms, empadj, payrolladj) {
        if (n > 1 && lb <= ub && !is.na(lb) && !is.na(ub)) {
          # Valid case: generate the dataframe
          f2(n, lb, ub, firms, empadj, payrolladj)
        } else {
          # Invalid case: return a dummy/empty dataframe
          tibble(nemp = integer(0))  # Empty, or use tibble(nfirms = NA) for a placeholder
        }
      }
    )
  )
test3

test3 |> 
  filter(nrecs > 1) |> 
  unnest(cols = data) |> 
  summarise(n=n(),
            nfirms_tot=sum(nfirms),
            nemp_tot=sum(nemp_pf * nfirms),
            payroll_tot=sum(payroll_pf * nfirms),
            .by=c(fscode, firmsize)
            ) |> 
  mutate(ppf = payroll_tot / nfirms_tot)

test2

test3 |> 
  filter(fscode=="02") |> 
  unnest(cols = data) 

taxdata <- test3 |>  
  unnest(cols = data) |> 
  mutate(rategroup = case_when(
    payroll_pf <= 1.25e6 ~ "excluded",
    payroll_pf <= 1.5e6 ~ "lowrate",
    payroll_pf <= 1.75e6 ~ "midrate",
    payroll_pf > 1.75e6 ~ "highrate",
    .default = "unknown"
  )) |> 
  mutate(ratenow = case_when(
    rategroup == "excluded" ~ 0,
    rategroup == "lowrate" ~ 0.0011,
    rategroup == "midrate" ~ 0.0023,
    rategroup == "highrate" ~ 0.006,
    rategroup == "unknown" ~ 0,
    .default = 0
  ))

taxdata |> 
  mutate(tax=ratenow * payroll_pf) |> 
  summarise(tax = sum(tax * nfirms),
            .by=c(ratenow, rategroup)) |> 
  arrange(ratenow) |> 
  janitor::adorn_totals() |> 
  mutate(pct = tax / tax[ratenow=="Total"])
  
#   0 - $375k 0.11% -- $1.5m
# $375 - 437.5k 0.23% -- 1.75m
# > $437.5k 0.60% -- $1.75m

# excluded now:  312,500  x 4 = 1.25m
  

fs_avg <- group$empadj / group$firms
fs_avg

# allow lower bounds that are below the group lower bound because regional operations of firms can be smaller than the national size
lower_bound_minimum_pct <- .75
fslb <- floor(min(group$fslb, lower_bound_minimum_pct * group$empadj / group$firms))
# If the top range is infinite make it a finite multiple of the lower bound
upper_bound_maximum_multiple <- 2
fsub <- group$fsub
if(is.infinite(fsub)) fsub <- floor(upper_bound_maximum_multiple * fslb) # ensure integers
fslb; fsub
group$fslb; group$fsub

nfirms_target <- group$firms
nemp_target <- group$empadj
payroll_target <- group$payrolladj
avgpay_target <- payroll_target / nemp_target

# define lower and upper bounds per firm
emplb_unadj <- fslb:fsub
emplb_unadj <- pmax(emplb_unadj, 1)
empub_unadj <- lead(emplb_unadj)
empub_unadj[length(emplb_unadj)] <- emplb_unadj[length(emplb_unadj)]
cbind(emplb_unadj, empub_unadj)

# define number of records and collapse as needed
nrecs <- length(emplb)
maxrecs <- 30
jump <- 200

first <- emplb_unadj[1]
mids <- seq(jump, length(emplb_unadj), jump)
last <- emplb_unadj[length(emplb_unadj)]
lbkeep <- c(first, mids, last)
lbkeep
nrecs <- length(lbkeep)
recs <- tibble(emplb=emplb_unadj, empub=empub_unadj) |> 
  filter(emplb %in% lbkeep) |> 
  mutate(empub=lead(emplb) - 1,
         empub=ifelse(is.na(empub), emplb, empub))
ht(recs)

# start calculations
emplb <- recs$emplb
empub <- recs$empub
epf <- pmean(emplb, empub) # employees per firm


nfirms_initial <- rep(nfirms_target / nrecs, nrecs)
nemp_initial <- nfirms_initial * epf
cbind(emplb, empub, epf, nfirms_initial, nemp_initial)

# collapse bounds to stay within limits

# sum(nfirms_initial) - nfirms_target # should be zero
emp_gap <- sum(nemp_initial) - nemp_target
emp_gap # typically positive, too many employees

# how many firms do we have to move for a given move in employment and max % of firms
ami <- abovemid_index(nrecs)

emp_gap_left <- emp_gap
max_pct <- 1
nfirms <- nfirms_initial
# i <- nrecs
# i <- i - 1
for(i in nrecs:ami){
  ilow <- nrecs - i + 1
  if(emp_gap_left >= 0){
    donor <- i
    recipient <- ilow
  } else {
    donor <- ilow
    recipient <- i
  }
  epf_diff <- abs(epf[donor] - epf[recipient]) # change in total employment for each firm moved
  capped_firm_move <- max_pct * nfirms_initial[donor] #  min(max_pct * nemp_initial[donor] / epf_diff, nfirms_initial[donor])
  gap_firm_need <- abs(emp_gap_left / epf_diff)
  firm_move <- min(capped_firm_move, gap_firm_need)
  nfirms[donor] <- nfirms[donor] - firm_move
  nfirms[recipient] <- nfirms[recipient] + firm_move
  nemp_sum <- sum(nfirms * epf)
  emp_gap_left <-  nemp_sum - nemp_target
  if(round(emp_gap_left, 1) == 0) break
}
nfirms

df <- tibble(firm = 1:nrecs, emplb, empub, epf, nfirms_initial, nemp_initial, nfirms) |> 
  mutate(payroll_initial = nemp_initial * avgpay_target,
         nemp = epf * nfirms,
         payroll = nemp * avgpay_target) |> 
  relocate(payroll_initial, .after=nemp_initial) |> 
  janitor::adorn_totals() |> 
  mutate(change_nfirms = nfirms - nfirms_initial,
         change_nemp = nemp - nemp_initial,
         ppf = payroll / nfirms)
df
tail(df)
group
group$empadj


# issues:
# lowest group lb is 0
# highest group ub is inf
# mid groups nfirms are < 1, move up
# high groups low needs to be lower than avg
