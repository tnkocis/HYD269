#2.1
A <- matrix(c(-4,1,1,0,1,-4,0,1,1,0,-4,1,0,1,1,-4), nrow=4, ncol=4)
b <- matrix(c(-15.86,-14.75,-16.77,-16.32), nrow=4, ncol=1)
x<-solve(A,b)

#2.2a
#known
h12 <- 7.68
h21 <- 8.18
h31 <- 8.36
h42 <- 8.41
h13 <- 7.19
h24 <- 7.56
h43 <- 8.33
h34 <- 7.99

#unknown, first guess
h22 <- 8
h32 <- 8.5
h23 <- 7
h33 <- 8

n <- 10
iterations <- data.frame(h22=c(h22,rep(NA,n)),
                         h23=c(h23,rep(NA,n)),
                         h32=c(h32,rep(NA,n)),
                         h33=c(h33,rep(NA,n)),
                         iter=c(0,rep(NA,n)))
for(i in 1:n){
  m <- i
  h22old <- h22
  h32old <- h32
  h23old <- h23
  h33old <- h33
  
  h22 <- (h12 + h21 + h32 +h23)/4
  h32 <- (h22 + h31 + h42 +h33)/4
  h23 <- (h13 + h22 + h33 +h24)/4
  h33 <- (h23 + h32 + h43 +h34)/4
  
  iterations$h22[[i]] <- h22
  iterations$h32[[i]] <- h32
  iterations$h23[[i]] <- h23
  iterations$h33[[i]] <- h33
  iterations$iter[[i]] <- i
  new <- c(h22,h32,h23,h33)
  new <- trunc(new*100)/100
  old <- c(h22old,h32old,h23old,h33old)
  old <- trunc(old*100)/100
  diff <- new-old
  if(all(abs(diff)==0)){
    break
  }
}
iter1 <- iterations
results <- data.frame(location=c("h 2,2", "h 2,3","h 3,2", "h 3,3"), 
                      h=c(h22,h23,h32,h33))
results

#2.2b
x

#2.2c
analytical <- data.frame(location=c("h 2,2", "h 2,3","h 3,2", "h 3,3"),
                         x=c(200,200,300,300),
                         y=c(200,100,200,100),
                         h=NA)
for(i in 1:length(analytical$location)){
  analytical$h[[i]] <- 1.06*log(sqrt(analytical$x[[i]]^2+analytical$y[[i]]^2)/2000)+10
}
analytical

#2.2d
error <- data.frame(location=c("h 2,2", "h 2,3","h 3,2", "h 3,3"),
                    analytical=analytical$h,
                    simultaneous=c(x),
                    iterative=results$h,
                    diff_simultaneous=c(x)-analytical$h,
                    diff_iterative=results$h-analytical$h,
                    error_simultaneous=abs(c(x)-analytical$h)/analytical$h,
                    error_iterative=abs(results$h-analytical$h)/analytical$h)

#2.2e
#known
h12 <- 7.68
h21 <- 8.18
h31 <- 8.36
h42 <- 8.41
h13 <- 7.19
h24 <- 7.56
h43 <- 8.33
h34 <- 7.99

#unknown, first guess
h22 <- 10
h32 <- 10
h23 <- 10
h33 <- 10

n <- 10
iterations <- data.frame(h22=c(h22,rep(NA,n)),
                         h23=c(h23,rep(NA,n)),
                         h32=c(h32,rep(NA,n)),
                         h33=c(h33,rep(NA,n)),
                         iter=c(0,rep(NA,n)))
for(i in 1:n){
  m <- i
  h22old <- h22
  h32old <- h32
  h23old <- h23
  h33old <- h33
  
  h22 <- (h12 + h21 + h32 +h23)/4
  h32 <- (h22 + h31 + h42 +h33)/4
  h23 <- (h13 + h22 + h33 +h24)/4
  h33 <- (h23 + h32 + h43 +h34)/4
  
  iterations$h22[[i]] <- h22
  iterations$h32[[i]] <- h32
  iterations$h23[[i]] <- h23
  iterations$h33[[i]] <- h33
  iterations$iter[[i]] <- i
  new <- c(h22,h32,h23,h33)
  new <- trunc(new*100)/100
  old <- c(h22old,h32old,h23old,h33old)
  old <- trunc(old*100)/100
  diff <- new-old
  if(all(abs(diff)==0)){
    break
  }
}
iter2 <- iterations
results2 <- data.frame(location=c("h 2,2", "h 2,3","h 3,2", "h 3,3"), 
                      h=c(h22,h23,h32,h33))
results2