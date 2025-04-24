

calc_tax <- function(data, taxplan){
  df <- data |>
    mutate(
      shortname = taxplan$shortname,
      exempt = eval(taxplan$exempt),
      excluded = eval(taxplan$excluded),
      low_pay = eval(taxplan$low_pay)) |> 
    left_join(taxplan$rates, 
              by = join_by(nyc, gpay_firm > lower, gpay_firm <= upper)) |> 
    select(-lower, -upper) |> 
    mutate(rate = ifelse(exempt | excluded | low_pay,
                         0,
                         rate),
           tax = rate * gpayroll)
  
  return(df)
}


make_taxplan <- function(shortname = NULL, 
                         lower_nyc = NULL,
                         rates_nyc = NULL, 
                         lower_burbs = NULL, 
                         rates_burbs = NULL, 
                         exempt = NULL,
                         excluded = NULL,
                         low_pay = NULL,
                         description = NULL){
  taxplan <- tax2024
  if(shortname == "tax2024") return(tax2024)
  
  taxplan$shortname <- shortname
  taxplan$description <- ifelse(is.null(description), shortname, description)
  
  if(!is.null(exempt)) taxplan$exempt <- expression(exempt)
  if(!is.null(excluded)) taxplan$excluded <- expression(excluded)
  if(!is.null(low_pay)) taxplan$low_pay <- expression(low_pay)
  
  if(!is.null(rates_nyc)){
    nyc <- tibble(nyc=rep(TRUE, length(rates_nyc)),
                  lower = lower_nyc,
                  rate = rates_nyc)|> 
      mutate(upper = lead(lower, default = Inf))
    taxplan$rates <- taxplan$rates |> 
      filter(!nyc) |> 
      bind_rows(nyc)
  }
  
  if(!is.null(rates_burbs)){
    burbs <- tibble(nyc=rep(FALSE, length(rates_burbs)),
                    lower = lower_burbs,
                    rate = rates_burbs)|> 
      mutate(upper = lead(lower, default = Inf))
    taxplan$rates <- taxplan$rates |> 
      filter(nyc) |> 
      bind_rows(burbs)
  }  
  
  return(taxplan)
}

pmtdb_forecast <- function(prob, pmtdb){
  pmtfc <- pmtdb |> 
    mutate(year = prob$year,
           gemp = gemp * (1 + prob$emp_growth),
           gpayroll = gpayroll * (1 + prob$avg_wagerate_growth) * (1 + prob$emp_growth)) |> 
    # get per- variables
    mutate(get_pervars(gfirms, gestabs, gemp, gpayroll))
  
  return(pmtfc)
}

# tax rules below here ----------------------------------------------------



tax2024 <- list(
  shortname = "tax2024",
  exempt = expression(
    owner == "Federal Government" |
      (owner == "Local Government") & naics=="6111"),
  excluded = expression(naics %in% c("491", "814")),
  low_pay = expression(gpay_firm < 1.25e6),
  rates = read_csv(
    "nyc, lower, rate
  TRUE, 0, 0.0011
  TRUE, 1.5e6, 0.0023
  TRUE, 1.75e6, 0.006
  FALSE, 0, 0.0011
  FALSE, 1.5e6, 0.0023
  FALSE, 1.75e6, 0.0034
  ") |> 
    mutate(upper = lead(lower, default = Inf), .by=nyc),
  description = "PMT tax rules for 2024"
)

