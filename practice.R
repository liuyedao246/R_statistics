rm(list = ls())

data("Puromycin")
head(Puromycin)
puroA <- subset(Puromycin, state == "treated")
plot(rate ~ conc, data = puroA)
attach(puroA)
plot(conc, rate)
detach(puroA)
u <- 1:25;plot(u, pch = u, col = u, cex = 3)
plot(rate ~ conc, data = puroA, pch = 2, col = 3, cex = 2.5)
plot(rate ~ conc, data = puroA, pch = 7, col = 3, cex = 2.5, xlim = c(0, 1.2), ylim = c(40, 210),xlab = "Concentration", ylab = "Rate", cex.lab = 2)
title(main = "Puromycin", cex.main = 2)
head(puroA)
m1 <- lm(rate ~ conc, data = puroA)
m2 <- lm(rate ~ conc + I(conc^2), data = puroA)
m3 <- lm(rate ~ conc + I(conc^2) + I(conc^3), data = puroA)
lines(fitted(m1) ~ conc, data = puroA, col = 2)
lines(fitted(m2) ~ conc, data = puroA, col = 3)
lines(fitted(m3) ~ conc, data = puroA, col = 4)

x <- rnorm(20,2,10)
m <- matrix(x, nrow = 4, ncol = 5)
colnames(m) <- c("r1", "r2", "r3", "r4", "r5")
rownames(m) <- c("c1", "c2", "c3", "c4")
plot(rate ~ conc, data = Puromycin, col = c(4,2)[state], pch = c(1,2)[state])
legend(x = 0.0, y = 200, legend = c("treated", "untreated"), col = c(4,2), pch = c(1,2), lty = c(1,3))
dev.off()


limited.central <- function(r = runif, distpar = c(0,1), m = 0.5, s = 1/sqrt(12), n = c(1, 3, 10, 30), N = 1000){
        for (i in n){
                if (length(distpar)==2){x <- matrix(r(i*N, distpar[1],distpar[2]), nc = i)} 
                else { x <- matrix(r(i*N, distpar), nc = i)}
                       x <- (apply(x, 1, sum) - i*m)/(sqrt(i)*s)
                       hist(x, col = 'light blue', probability = T, main = paste("n=", i), ylim = c(0, max(0.4, density(x)$y)))
                       lines(density(x), col = 'red', lwd = 3)
                       curve(dnorm(x), col = 'blue', lwd = 3, lty = 3, add = T)
                if( N > 100){rug(sample(x, 100))}
                       else{rug(x)}
        }
}

limited.central()

op <- par(mfrow = c(2,2))
limited.central(rbinom, distpar = c(10, 0.1), m = 1, s = 0.9)
limited.central(rpois, distpar = 1, m = 1, s = 1, n =c(3, 10, 30, 50))
limited.central()
limited.central(rexp, distpar = 1, m =1, s = 1)
mixn <- function(n, a = -1, b = 1){
        rnorm(n, sample(c(a, b), n, replace = T))
}
limited.central(r = mixn, distpar = c(-3, 3), m = 0, s = sqrt(10), n = c(1, 2, 3, 30))
        
sum(sample(1:100,5))
rnorm(1000, mean = 100, sd = 100)
hist(rnorm(1000, mean = 1, sd = 100) , breaks = 30, col = 'light blue')
lines(density(rnorm(1000, mean = 100, sd = 100)))
dev.off()
par(mfrow = c(1,1))

n <- 20
p <- 0.2
k <- seq(0, n)
plot(k, dbinom(k, n, p),type = 'h',main = 'Binominal distribution, n=20,p=0.2', xlab = 'k')

lambda <- 4.0
k <- seq(0, 20)
plot(k, dpois(k, lambda), type = 'h', main = 'Poisson distribution, lambada = 4.0', xlab = 'k')

p <- 0.5
k <- seq(0, 10)
plot(k, dgeom(k, p), type = 'h', main = 'Geometric distribution, p=0.5', xlab = 'k')

N <- 30
M <- 10
n <- 10
k <- seq(0,10)
plot(k, dhyper(k, N, M,n),type = 'h', main = 'Hypergeometric distribution,N=30, M=10, n =10', xlab = 'k')

n <- 10
p <- 0.5
k <- seq(0, 40)
plot(k, dnbinom(k,n,p), type = 'h', main = 'Negative Binomial distribution, n=10, p=0.5', xlab = 'k')

curve(dnorm(x,0,1), xlim = c(-5,5), ylim = c(0, 0.8), col = 'red', lwd = 2, lty = 3)
curve(dnorm(x, 0, 2), add = TRUE, col = "blue", lwd = 2, lty = 2)
curve(dnorm(x, 0, 1/2), add = TRUE, lwd = 2, lty = 1)
title(main = "Gaussian distribution")
legend(par('usr')[2],par('usr')[4],xjust = 1, c('sigma=1','sigma=2','sigma=1/2'),lwd = c(2,2,2),lty = c(3,2,1),col = c('red','blue',par('fg')))

curve(dt(x,1), xlim = c(-3,3), ylim = c(0,4), col = 'red', lwd = 2, lty = 1)
curve(dt(x,2), add = TRUE, col = 'green', lwd = 2, lty =2)
curve(dt(x,10), add = TRUE, col = 'orange', lwd = 2, lty = 3)
curve(dnorm(x), add = TRUE, lwd = 3, lty =4)
title(main = "Student T distribution")
legend(par('usr')[2],par('usr')[4], xjust = 1,c('df=1', 'df=2','df=10', 'Gaussian distribution'),lwd = c(2,2,2,3),lty = c(1,2,3,4),col = c('red','green','orange',par('fg')))

curve(dchisq(x,1), xlim = c(0,10), ylim = c(0,0.6), col = 'red', lwd = 2)
curve(dchisq(x,2), add = TRUE, col = 'green', lwd =2)
curve(dchisq(x,3), add = TRUE, col = 'blue', lwd =2)
curve(dchisq(x,5), add = TRUE, col = 'orange', lwd = 2)
abline(h=0, lty = 3)
abline(v=0, lty = 3)
title(main = 'Chi square distribution')
legend(par('usr')[2], par('usr')[4], xjust = 1, c('df=1', 'df=2', 'df=3', 'df=5'),lwd = 3, lty=1, col = c('red', 'green','blue','orange'))

curve(df(x, 1,1), xlim = c(0,2), ylim = c(0,0.8), lty =1)
curve(df(x,3,1), add = TRUE, lwd = 2, lty = 2)
curve(df(x,6,1), add = TRUE, lwd = 2, lty = 3)
curve(df(x,3,3), add = TRUE, col = 'red', lwd = 3, lty =4)
curve(df(x,3,6), add = TRUE, col = 'blue', lwd = 3, lty = 5)
title(main = "Fisher's F distribution")
legend(par('usr')[2],par('usr')[4], xjust = 1, c('df=(1,1)', 'df=(3,1)', 'df=(6,1)', 'df=(3,3)', 'df=(3,6)'),lwd = c(1,2,2,3,3), lty = c(1,2,3,4,5), col = c(par('fg'), par('fg'), par('fg'), 'red', 'blue'))

curve(dlnorm(x), xlim = c(-0.2,5), ylim = c(0, 1.0), lwd = 2)
curve(dlnorm(x, 0,3/2), add = TRUE, col = 'blue', lwd = 2, lty = 2)
curve(dlnorm(x, 0, 1/2), add = TRUE, col = 'orange',lwd = 2,lty = 3)
title(main = 'Log normal distribution')
legend(par('usr')[2], par('usr')[4], xjust = 1,c('sigma=1', 'sigma=3/2', 'sigma=1/2'), lwd = c(2,2,2), lty = c(1,2,3), col = c(par('fg'), 'blue', 'orange'))

curve(dcauchy(x), xlim = c(-5,5), ylim = c(0,0.5), lwd = 2)
curve(dnorm(x), add = TRUE, col = 'red', lty = 2)
legend(par('usr')[2],par('usr')[4], xjust = 1, c('Cauchy distribution', 'Gaussian distribution'), lwd = c(3,1), lty = c(1,2), col = c(par('fg'), 'red'))
        
curve(dexp(x), xlim = c(0,3), ylim = c(0,2))
curve(dweibull(x,1), lty = 3, lwd =3, add = TRUE)
curve(dweibull(x,2), col = 'red', add = TRUE)
curve(dweibull(x, 0.8), col = 'blue', add = TRUE)
title(main = "Weibull distribution")
legend(par('usr')[2], par('usr')[4], xjust = 1, c('Exponential', 'Weibull,shape=1', 'Weibull,shape=2', 'Weibull,shape=0.8'), lwd = c(1,3,1,1), lty = c(1,3,1,1), col = c(par('fg'), par('fg'), 'red', 'blue'))

curve(dgamma(x, 1,1), xlim = c(0,5), lwd=2, lty=1)
curve(dgamma(x,2,1), add = TRUE, col = 'red', lwd=2, lty =2)
curve(dgamma(x,3,1), add = TRUE, col = 'green', lwd =2, lty=3)
curve(dgamma(x,4,1), add = TRUE, col = 'blue', lwd=2, lty=4)
curve(dgamma(x,5,1), add = TRUE, col = 'orange', lwd =2, lty=5)
title(main = "Gamma distribution")
legend(par('usr')[2],par('usr')[4], xjust = 1, c('k=1(Exponential distribution)', 'k=2','k=3','k=4','k=5'),lwd = c(2,2,2,2,2), lty = c(1,2,3,4,5), col = c(par('fg'),'red','green','blue','orange'))

curve(dbeta(x,1,1), xlim = c(0,1), ylim = c(0,4))
curve(dbeta(x,3,1), add = TRUE, col = 'green')
curve(dbeta(x,3,2), add = TRUE, lwd =2, lty = 2)
curve(dbeta(x,4,2), add = TRUE, lwd=2, lty=2, col = 'blue')
curve(dbeta(x,2,3), add = TRUE, lwd=3, lty=3, col = 'red')
curve(dbeta(x,4,3), add = TRUE, lwd=3, lty=3, col = 'orange')
title(main = 'Beta distribution')
legend(par('usr')[1], par('usr')[4], xjust = 0,c('(1,1)', '(3,1)','(3,2)','(4,2)','(2,3)','(4,3)'),lwd = c(1,1,2,2,3,3), lty = c(1,1,2,2,3,3), col = c(par('fg'), 'green', par('fg'), 'blue', 'red', 'orange'))
