
library(RcppRoll)

glimpse(df1)

check <- count(df1, naics, fnaics, date)

uname <- "New York State"

uname <- "New York City"

stsum <- df1 |> 
  filter(uniname==uname) |> 
  summarise(n=n(),
            txblsales=sum(txblsales, na.rm = TRUE),
            .by=c(stdate, uniname)) |> 
  mutate(ind="all") |> 
  arrange(stdate)

inds <- df1 |> 
  filter(uniname==uname, naics %in% c(4541, 9999)) |> 
  summarise(txblsales=sum(txblsales, na.rm = TRUE),
            .by=c(stdate, uniname, naics)) |> 
  mutate(ind=paste0("i", naics)) |> 
  select(-naics) |> 
  arrange(ind, stdate)

combo <- bind_rows(stsum, inds) |> 
  select(-n) |> 
  arrange(stdate) |> 
  mutate(txblsales = txblsales / 1e6) |> 
  pivot_wider(names_from=ind, values_from=txblsales) |>
  mutate(ishare = i4541 / all,
         isharema4 = RcppRoll::roll_mean(ishare, n=4,  align="right", fill=NA),
         i4541 = ifelse(stdate > "2021-12-01" & stdate <= "2022-12-01", 
                        all * isharema4[stdate == "2021-12-01"],
                        i4541),
         i4541=ifelse(stdate > "2022-12-01", lag(i4541, 4) * i9999 / lag(i9999, 4), i4541)) |>
  mutate(i4541=ifelse(stdate > "2022-12-01", lag(i4541, 4) * i9999 / lag(i9999, 4), i4541)) |> 
  mutate(across(c(all, i4541, i9999), list(pch = \(x) x / lag(x, 4) - 1)))

# combo |> 
#   select(stdate, all_pch, i4541_pch) |> 
#   pivot_longer(cols=cols=-stdate) |>
#   ggplot(aes(stdate, value, color=name)) +
#   geom_line()

lsize <- 1.25
combo |> 
  select(stdate, all, i4541) |> 
  pivot_longer(cols=-stdate) |>
  ggplot(aes(stdate, value)) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 1.1) +
  geom_smooth(method="loess", se=FALSE) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous() +
  facet_wrap(~name, scales="free_y", nrow=2) +
  ggtitle(uname) +
  geom_vline(xintercept = as.Date("2019-01-15"), linetype="dashed", colour="blue", linewidth = lsize) +
  geom_vline(xintercept = as.Date("2019-06-01"), linetype="dashed", colour="green", linewidth = lsize) +
  geom_vline(xintercept = as.Date("2020-03-019"), linetype="dashed", colour="red", linewidth = lsize) +
  theme_minimal()




tmp <- df1 |>
  filter(naics==4541) 
  # filter(naics==4541, uniname=="New York City") 

summary(df1)
summary(tmp)
tmp <- count(df1, naics, fnaics)


df1 |>
  filter(naics==4541, uniname=="New York State") |> 
  arrange(stdate) |> 
  select(stdate, uniname, naics, txblsales) |> 
  mutate(txblsales=txblsales / 1e6,
         tsma4=roll_sum(txblsales, 4, align="right", fill=NA),
         pchya=tsma4 / lag(tsma4, 4) - 1)
