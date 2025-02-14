
qcopt1
dput(qcopt1$elb)
dput(qcopt1$esize_estabs)

sum(qcopt1$elb * qcopt1$esize_estabsadj)
sum(qcopt1$esize_avgemp * qcopt1$esize_estabsadj)
sum(qcopt1$esize_avgemp * ifelse(qcopt1$esize_estabsadj < 1, 0, qcopt1$esize_estabsadj))

estabs <- 9
emp <- 85
epe <- 85 / 9 # average emp per estab

elb <- c(0, 5, 10, 20, 50, 100, 250, 500, 1000)
ests <- c(5.14285714285714, 3.85714285714286, 0, 0, 0, 0, 0, 0, 0)

empdiff <- emp - sum(elb * ests)
empdiff
first_nonzero_index <- which(ests != 0)[1]
ests2 <- ests
# matrix with value for taking 1 away from the row element and adding it to the column element
# e.g., if we take 1 establishment away from 0-5 and add it to 0-10, our value goes up by 5
# take 1 away from group 2 and add to group 3 goes up 5 and so on
lbmoves <- -outer(elb, elb, "-")

i <- 1
need <- empdiff
i <- i + 1

moveestabs <- function(emp, elb, ests){
  empdiff <- emp - sum(elb * ests)
  ests2 <- ests
  need <- empdiff
  
  for(i in 1:(length(elb) - 1)){
    print(need)
    if(need > 0){
      # move up to some fraction of ests out
      maxestmove <- ests2[i] / 2 # allow moves up to this amount
      maxestneed <- need / lbmoves[i, i+1] # can't be larger than the need
      imove <- pmin(maxestneed, maxestmove)
      print(imove)
      # ests
      ests2[i] <- ests2[i] - imove
      ests2[i + 1] <- ests2[i + 1] + imove
      need <- emp - sum(elb * ests2)
    } else break
  }
  ests2
}

ests2 <- move_estabs(emp, elb, ests)

sum(ests2 * elb)
  
ests
ests2
