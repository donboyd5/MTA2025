
# get_mtasub <- function(fips){
#   ifelse(fips %in% constants$nycfips, "nyc", "suburbs")
# }

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
