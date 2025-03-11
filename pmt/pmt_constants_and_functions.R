

# pmt rate schedules ------------------------------------------------------

base_rates <- read_csv(
  "payroll_ub, nyc, suburbs
1.25e6, 0, 0
1.5e6, 0.0011, 0.0011
1.75e6, 0.0023, 0.0023
Inf, 0.0060, 0.0034
", show_col_types = FALSE)
# base_rates
 


# pmt model functions -----------------------------------------------------


get_info <- function(payroll, nyc, rates, prefix="base_"){
  # function to get tax rate and payroll bounds for a given
  # record, given a rate schedule
  # 
  # Example:
  #   get_info(1e6, FALSE, base_rates)  # returns the following tibble:
      # base_payroll_lb base_payroll_ub base_rate
      # <dbl>           <dbl>     <dbl>
      #   1               0         1250000         0
  
  # get rate indexes
  index <- sapply(payroll, \(x) which(rates$payroll_ub > x)[1])
  colname <- ifelse(nyc, "nyc", "suburbs")
  ub <- rates$payroll_ub[index]
  lb <- numeric(length(ub)) # fill with zeros
  ilbnz <- index[index > 1] - 1
  lb[index > 1] <- rates$payroll_ub[ilbnz]
  df <- rates[index, ] |> 
    mutate(colname=colname,
           rate=ifelse(colname=="nyc", nyc, suburbs),
           payroll_lb = lb,
           payroll_ub = ub) |> 
    select(payroll_lb, payroll_ub, rate) |> 
    rename_with(.fn = \(x) paste0(prefix, x))
  df
}


# payroll cutpoints and labels
# 
cutlabs <- function(payroll_cuts){
  # return a tibble with labels for ranges
  payroll_cuts <- payroll_cuts |> unique() |> sort()
  cutlen <- length(payroll_cuts)
  paybounds <- tibble(lb = payroll_cuts[-cutlen],
                      ub = payroll_cuts[-1]) |> 
    arrange(lb, ub) |> 
    mutate(paygroup=1:n()) |> 
    mutate(lb_label = scales::label_comma()(lb),
           ub_label = scales::label_comma()(ub)) |> 
    mutate(range_left = paste0(">= ", lb_label, " - < ", ub_label),
           range_right = paste0("> ", lb_label, " - <= ", ub_label)) |> 
    select(paygroup, lb, ub, lb_label, ub_label, range_left, range_right)
  
  paybounds
}

getlabel <- function(paygroup, labels, rightleft="left"){
  i <- match(paygroup, labels$paygroup)
  colname <- paste0("range_", rightleft)
  labels[i, colname] |> pull()
}

# getlabel(7:10, paygroup_labels)

# (payroll_cuts <- c(seq(0, 2e6, 200e3), seq(2e6, 5e6, 1e6), Inf, 1.25e6, 1.75e6))
# 
# paygroup_labels <- cutlabs(payroll_cuts)
# 
# (test_pay <- c(-1e3, seq(0, 5.2e6, 200e3), Inf))
# test_cuts <- cut(test_pay, payroll_cuts, right=FALSE, labels=FALSE)
# tibble(pay=test_pay, paygroup=test_cuts) |> 
#   left_join(paygroup_labels, by = join_by(paygroup)) |> 
#   select(pay, range_left, paygroup)



