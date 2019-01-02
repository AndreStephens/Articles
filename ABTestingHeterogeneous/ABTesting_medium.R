#set seed
set.seed(123)

# no. of user-sessions
n <- 1000

# choose parameters
alpha <- 0.3
beta <- 3.52
gamma <- 0.08
delta <- -0.15
epsilon <- rnorm(n=n, mean=0, sd=0.25)

# make treatment predictor
treat <- sample(rep(c(TRUE,FALSE), each=n/2))

# make age predictor for users 16+
age <- rnorm(n=n, mean=22, sd=6)
above16 <- age[age>=16]
## replace values lower than 16
sample(above16,1)
for (i in 1:length(age)) {
  if (age[i] < 16){
    age[i] <- sample(above16, 1)
  }
}

# compute means
mu <- alpha + beta*treat + gamma*age + delta*treat*age + epsilon

# compute standard deviations
sd_mu_treat <- sd(alpha + beta*1 + gamma*age + delta*1*age + epsilon)
sd_mu_cont <- sd(alpha + beta*0 + gamma*age + delta*0*age + epsilon)
sd = (sd_mu_cont + sd_mu_treat)/2
sd(mu)

# use means to generate actual values
dur <- rnorm(n=n, mean = mu, sd = sd)

# since mins must be greater than zero,
# set small no. of mu < 0 to near-zero
#mu[mu <= 0] <- 0.001
dur[dur <= 0] <- 0.001

# build dataframe
data <- data.frame(dur=dur, age=age, treat=treat)

# get difference in means
treat_mins <- subset(data, treat==TRUE)
cont_mins <- subset(data, treat==FALSE)
meandiff <- mean(treat_mins$dur) - mean(cont_mins$dur)
meandiff

# calculate z-score
zscore = function(vec1,vec2){
  mu_diff = mean(vec2) - mean(vec1)
  mean_var = (var(vec1)+var(vec2))/(n/2)
  mu_diff/sqrt(mean_var)
}

z = zscore(cont_mins$dur, treat_mins$dur)
z

# get p-values
p_val_upper <- pnorm(z, lower.tail = FALSE)
p_val_upper

p_val_lower <- pnorm(z)
p_val_lower


# run model 1
model1 <- lm(dur ~ treat, data=data)
summary(model1)

# run model 2
model2 <- lm(dur ~ treat + age, data=data)
summary(model2)

# run model 3
model3 <- lm(dur ~ treat*age, data=data)
summary(model3)
