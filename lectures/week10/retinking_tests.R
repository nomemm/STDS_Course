library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 40 , ]
# define the average weight, x-bar
xbar <- mean(d2$height)
m4.3 <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b*( height - xbar ) ,
    a ~ dnorm( 60 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 30 ) ),
  data=d2 )

precis( m4.3 )

round( vcov( m4.3 ) , 3 )
plot( weight ~ height , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 , 10)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )
s_map <- mean(post$sigma)
d <- Howell1
d2 <- d[ d$age >= 1, ]
# define the average weight, x-bar
xbar <- mean(d2$height)
m4.3 <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b*( height - xbar ) ,
    a ~ dnorm( 60 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 30 ) ),
  data=d2 )
precis( m4.3 )
round( vcov( m4.3 ) , 3 )
plot( weight ~ height , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 , 20)
a_map <- mean(post$a)
b_map <- mean(post$b)
s_map <- mean(post$sigma)
curve( a_map + b_map*(x - xbar) , add=TRUE )

# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
height.seq <- seq( from=60 , to=180 , by=10 )

# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( m4.3 , data=data.frame(height=height.seq), )
str(mu)

plot(height.seq, mu[1,]+mu[2,])
