#calculate alpha
dt <- 0.001 #s
#dx <- 10
dx <- 0.1
lambda <- 0.1
alpha <- lambda*(dx^2)/dt

##define parameters
#dx <- 10
dx <- 0.1
lambda <- 0.1
dt <- lambda*(dx^2)/alpha
L <- 1
nx <- (L/dx)+1

#define initial and boundary conditions
#H0 is the initial head
#H0 <- 16
H0 <- 1
Hf <- 0
totalt <- .1 #s
nend <- totalt/dt
f1 <- dt*alpha
# hold <- rep(NA,nx)
# hnew <- rep(NA,nx)

hold = matrix(rep(NA, (nx + 1)*(nend + 1)), ncol = nx + 1)
colnames(hold) = c(paste("node", 1:nx), "time")

hold[1,!(colnames(hold) == "time")] <- H0
hold[,1] <- Hf
hold[,nx] <- Hf
hold[,"time"] <- seq(0,nend)*dt

for(i in 2:(nend+1)){
  for(j in 2:(nx-1)){
    d2h <-(hold[i-1,j+1]-2*hold[i-1,j]+hold[i-1,j-1])/(dx*dx)
    hold[i,j] <- hold[i-1,j] + f1*d2h
  }
}


drainage <- function(alpha,L,dx,lambda,H0,Hf,totalt){
  
  dt <- lambda*(dx^2)/alpha
  nx <- (L/dx)+1
  nend <- ceiling(totalt/dt)
  f1 <- dt*alpha
  # hold <- rep(NA,nx)
  # hnew <- rep(NA,nx)
  
  hold = matrix(rep(NA, (nx + 1)*(nend + 1)), ncol = nx + 1)
  colnames(hold) = c(paste0("x=", seq(0,L,dx)), "time")
  
  hold[1,!(colnames(hold) == "time")] <- H0
  hold[,1] <- Hf
  hold[,nx] <- Hf
  hold[,"time"] <- seq(0,nend)*dt
  
  for(i in 2:(nend+1)){
    for(j in 2:(nx-1)){
      d2h <-(hold[i-1,j+1]-2*hold[i-1,j]+hold[i-1,j-1])/(dx*dx)
      hold[i,j] <- hold[i-1,j] + f1*d2h
    }
  }
  return(hold)
}

#calculate alpha
dt <- 0.001 #s
#dx <- 10
dx <- 0.1
lambda <- 0.1
alpha <- lambda*(dx^2)/dt

part_a_0.1 <- drainage(alpha=alpha,L=1,dx=0.1,lambda=0.1,H0=1,Hf=0,totalt=.1)
#tprint <- seq(0,100,5)/1000
#part_a_0.1[which(part_a_0.1[,"time"]%in%tprint),c("x=0.4","time")]
part_a_0.3 <- drainage(alpha=alpha,L=1,dx=0.1,lambda=0.3,H0=1,Hf=0,totalt=.1)
part_a_0.5 <- drainage(alpha=alpha,L=1,dx=0.1,lambda=0.5,H0=1,Hf=0,totalt=.1)
part_a_0.55 <- drainage(alpha=alpha,L=1,dx=0.1,lambda=0.55,H0=1,Hf=0,totalt=.1)
part_a_0.7 <- drainage(alpha=alpha,L=1,dx=0.1,lambda=0.7,H0=1,Hf=0,totalt=.1)

df_a_0.1 <- as.data.frame(part_a_0.1[,c("x=0.4","time")])
df_a_0.1$lambda <- 0.1
df_a_0.3 <- as.data.frame(part_a_0.3[,c("x=0.4","time")])
df_a_0.3$lambda <- 0.3
df_a_0.5 <- as.data.frame(part_a_0.5[,c("x=0.4","time")])
df_a_0.5$lambda <- 0.5
df_a_0.55 <- as.data.frame(part_a_0.55[,c("x=0.4","time")])
df_a_0.55$lambda <- 0.55
df_a_0.7 <- as.data.frame(part_a_0.7[,c("x=0.4","time")])
df_a_0.7$lambda <- 0.7
df_a_plot <- rbind.data.frame(df_a_0.1,df_a_0.3,df_a_0.5,df_a_0.55,
                              df_a_0.7)
names(df_a_plot)[[1]] <- "h"
exact <- data.frame(h=c(1.0000,1.0000,0.9953,0.9785,0.9518,0.9192,0.8832,
                        0.8461,0.8088,0.7721,0.7363,0.7018,0.6686,0.6368,
                        0.6063,0.5773,0.5496,0.5232,0.4981,0.4741,0.4513),
                    time=seq(0,100,5)/1000,
                    lambda="exact solution")
df_a_plot <- rbind.data.frame(df_a_plot,exact)
df_a_plot$lambda <- factor(df_a_plot$lambda)

library(ggplot2)
b_plot <- ggplot(df_a_plot)+geom_line(aes(x=time,y=h,color=lambda))


part_c_0.05 <- drainage(alpha=alpha,L=1,dx=0.05,lambda=0.3,H0=1,Hf=0,totalt=.1)
part_c_0.2 <- drainage(alpha=alpha,L=1,dx=0.2,lambda=0.3,H0=1,Hf=0,totalt=.1)

df_c_0.05 <- as.data.frame(part_c_0.05[,c("x=0.4","time")])
df_c_0.05$dx <- 0.05
df_c_0.2 <- as.data.frame(part_c_0.2[,c("x=0.4","time")])
df_c_0.2$dx <- 0.2
df_c_0.1 <- as.data.frame(part_a_0.1[,c("x=0.4","time")])
df_c_0.1$dx <- 0.1
df_c_plot <- rbind.data.frame(df_c_0.05,df_c_0.2,df_c_0.1)
names(df_c_plot)[[1]] <- "h"
exact <- data.frame(h=c(1.0000,1.0000,0.9953,0.9785,0.9518,0.9192,0.8832,
                        0.8461,0.8088,0.7721,0.7363,0.7018,0.6686,0.6368,
                        0.6063,0.5773,0.5496,0.5232,0.4981,0.4741,0.4513),
                    time=seq(0,100,5)/1000,
                    dx="exact solution")
df_c_plot <- rbind.data.frame(df_c_plot,exact)
df_c_plot$dx <- factor(df_c_plot$dx)
d_plot <- ggplot(df_c_plot)+geom_line(aes(x=time,y=h,color=dx))

part_e_0.3 <- as.data.frame(as.matrix(part_a_0.3[which(sapply(part_a_0.3[,"time"],all.equal,21/1000)==TRUE),c("x=0.1","x=0.2","x=0.3","x=0.4","x=0.5")]))
part_e_0.7 <- as.data.frame(as.matrix(part_a_0.7[which(sapply(part_a_0.7[,"time"],all.equal,21/1000)==TRUE),c("x=0.1","x=0.2","x=0.3","x=0.4","x=0.5")]))
part_e_0.3$x <- c(0.1,0.2,0.3,0.4,0.5)
part_e_0.7$x <- c(0.1,0.2,0.3,0.4,0.5)
part_e_0.3$lambda <- 0.3
part_e_0.7$lambda <- 0.7
part_e <- rbind.data.frame(part_e_0.3,part_e_0.7)
names(part_e)[[1]] <- "h"
part_e$lambda <- factor(part_e$lambda)
e_plot <- ggplot(part_e) + geom_line(aes(x=x,y=h,color=lambda))

# for(i in 1:nx){
#   hold[[i]] <- H0
#   hnew[[i]] <- H0
# } 
#  hold[[nx]] <- 11
#  hnew[[nx]] <- 11
# 
#  
#  for(j in 1:nend){
#    for(k in 2:nlx){
#      f1 <- dt*t/S
#      d2h <-(hold[[k+1]]-2*hold[[k]]+hold[[k-1]])/(dx*dx)
#      hnew[[k]] <- hold[[k]]+(f1*d2h)
#    }
#    for(l in 1:nlx){
#      hold[[l]] <- hnew[[l]]
#    }
#  }