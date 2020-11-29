# ITEMS: Fundations of IRT Estimation 
## (Examples with R mirt and mirtCAT package)

Zhuoran Wang\
2020-10-25

## 1. Install and Load Packages
```r
install.packages("mirt")
install.packages("mirtCAT")
library(mirt)
library(mirtCAT)
```

## 2. Parameter Generation
40 3PL items and 1000 normally distributed candidates are generated.
```r
# define test length and population
n_item <- 40
n_person <- 1000

# item
set.seed(123)
a <- rlnorm(n_item, meanlog = 0.3, sdlog=0.3)
b <- rnorm(n_item)
c <- rbeta(n_item, 20, 80)
par <- data.frame(a1=a, d=-a * b, g=c)

# person
set.seed(123)
theta <- rnorm(n_person)
```

## 3. Response Simulation
```r
resp <- simdata(a=a, d=-a * b, N=n_person, itemtype = '3PL', guess = c, Theta=matrix(theta))

```

## 4. Scoring with Known Item Parameters
```r
# IRT model definition
mod <- generate.mirt_object(parameters = par, itemtype ='3PL')

# ability estimation with MLE 
temp <- fscores(mod, method="ML", response.pattern=resp)[,-(1:n_item)] # exclude response matrix
theta_est <- temp[,1] # theta estimation
se_est <- temp[,2] # SE of theta estimation
# assign -4 and 4 to extreme
theta_est[which(theta_est>4)] <- 4
theta_est[which(theta_est< -4)] <- -4

# ability estimation with MAP 
temp <- fscores(mod, method="MAP", response.pattern=resp)[,-(1:n_item)] # exclude response matrix
theta_est <- temp[,1] # theta estimation
se_est <- temp[,2] # SE of theta estimation

# ability estimation with EAP
temp <- fscores(mod, method="EAP", response.pattern=resp)[,-(1:n_item)] # exclude response matrix
theta_est <- temp[,1] # theta estimation
se_est <- temp[,2] # SE of theta estimation
```

## 5. Use Customized Population Prior in MAP and EAP
```r
# customized population prior
fun <- function(Theta, ...) {
  as.numeric(dgamma(Theta+2, shape = 2))
}

# MAP
temp <- fscores(mod, method="MAP", custom_den = fun, response.pattern=resp)[,-(1:n_item)]
theta_est <- temp[,1] # theta estimation
se_est <- temp[,2] # SE of theta estimation

# EAP
temp <- fscores(mod, method="EAP", custom_den = fun, response.pattern=resp)[,-(1:n_item)]
theta_est <- temp[,1] # theta estimation
se_est <- temp[,2] # SE of theta estimation
```

## 6. Calibration
```r
mod_est <- mirt(resp, model=1, itemtype = "3PL", SE = T)
temp <- coef(mod_est, printSE=T, IRTpars=T, as.data.frame=T)
par_est <- matrix(temp[1:(n_item * 4),1], nrow = n_item, byrow = T)[,-4] # in a,b,c format
par_se <- matrix(temp[1:(n_item * 4),2], ncol = 4, byrow = T)[,-4]
```

## 7. Calibration with Empirical Histogram for Latent Trait Distribution 
```r
mod_est <- mirt(resp, model=1, itemtype = "3PL", dentype = "EH", SE=T)
temp <- coef(mod_est, printSE=T, IRTpars=T, as.data.frame=T)
par_est <- matrix(temp[1:(n_item * 4),1], nrow = n_item, byrow = T)[,-4] # in a,b,c format
par_se <- matrix(temp[1:(n_item * 4),2], ncol = 4, byrow = T)[,-4]
```

## 8. Calibration with Start Values for Item Parameters
```r
start_values <- mod2values(mod)
# fix the . and _ discrepancy
temp <- NA
for (j in 1:n_item)
{
  temp[j] <- paste0("Item_", j)
}
start_values$item[1:(n_item*4)] <- rep(temp, each=4)
mod_est <- mirt(resp, model=1, itemtype = "3PL", pars = start_values, SE=T)
temp <- coef(mod_est, printSE=T, IRTpars=T, as.data.frame=T)
par_est <- matrix(temp[1:(n_item * 4),1], ncol = 4, byrow = T)[,-4]
par_se <- matrix(temp[1:(n_item * 4),2], ncol = 4, byrow = T)[,-4]
```

## 9. Calibration with Priors for Item Parameters
```r
prior <- list(c(seq(1, by=4, length.out = n_item), "lnorm", 0.3, 0.3),
              c(seq(2, by=4, length.out = n_item), "norm", 0, 1))
mod_est <- mirt(resp, model=1, itemtype = "3PL", SE=T,
                parprior = prior)
temp <- coef(mod_est, printSE=T, IRTpars=T, as.data.frame=T)
par_est <- matrix(temp[1:(n_item * 4),1], ncol = 4, byrow = T)[,-4]
par_se <- matrix(temp[1:(n_item * 4),2], ncol = 4, byrow = T)[,-4]
```
