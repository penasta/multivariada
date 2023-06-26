e11 <- matrix(c(100,0,
              0,1),2,2)
e12 <- matrix(c(0,0,
                0.95,0),2,2)
e21 <- matrix(c(0,0.95,
                0,0),2,2)
e22 <- matrix(c(1,0,
                0,100),2,2)

# 4.1 R&C

e1 <- matrix(c(14,8,3,
               8,5,2,
               3,2,1),3,3)
det(e1)

e2 <- matrix(c(6,6,1,
               6,8,2,
               1,2,1),3,3)
det(e2)

cor(e1, method="pearson")
cor(e2, method="pearson")

# 4.10 a)

c <- t(c(2,-1,3))
mu <- t(t(c(3,1,4)))
c%*%mu

sigma <- matrix(c(6,1,-2,
                  1,13,4,
                  -2,4,4),3,3)

c %*% sigma %*% t(c)

# 4.10 b)

A <- matrix(c(1,1,
              1,-1,
              1,2),2,3)
A

A%*%mu # media

A%*%sigma%*%t(A) # variância



# 4.10 c)

c <- c(0,1,0)
c
c %*% mu
c %*% sigma %*% c

# 4.10 d)

c <- matrix(c(1,0,0,
              0,0,1),2,3)
c
c %*% mu
c %*% sigma %*% t(c)

# 4.10 d)

A <- matrix(c(1,0,0,
              0,0,1,
              1/2,1/2,0),3,3)
A <- t(A)
A %*% mu
A %*% sigma %*% t(A)

# 4.11 a)

dec <- svd(sigma)
dec
dec$d
sqrt(dec$d)

tr <- matrix(c(0,0,0,
             0,0,0,
             0,0,0),3,3)
tr <- diag(sqrt(dec$d))
tr
solve(dec$u %*% tr %*% t(dec$v))
# deu ruim

solve(t(chol(sigma)))
# deu bom



















m <- c(1, 2)
sigma <- matrix(c(2,1,1,2), nrow=2)
data.grid <- expand.grid(s.1 = seq(-2, 5, length.out=200), s.2 = seq(-2, 5, length.out=200))
q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = m, sigma = sigma))
ggplot(q.samp, aes(x=s.1, y=s.2, z=prob)) + 
  geom_contour() +
  coord_fixed(xlim = c(-2, 4), ylim = c(-1, 5), ratio = 1) 
