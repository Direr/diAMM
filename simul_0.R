# install.packages("readxl")
# install.packages("usethis")
# use_git_config(user.name = "Direr", user.email = "alexis.direr@univ-orleans.fr")

library(readxl)
library(usethis)

# hackmd : A simple model of MM

data_usdc_eth <- read_excel("ETH_USDC_0.3_eth.xlsx",col_names = TRUE)
# str(data_usdc_eth)

data_p <- data.frame(data_usdc_eth$price,data_usdc_eth$qty)
colnames(data_p) <- c("p", "q")
data_p <- subset(data_p, q>0.005)
pv <- data_p$p
head(pv)


##### baseline  ######

min_p <- 1500
max_p <- 5000
fac <- 1.1                      # multiplicative step

omega <- function(price) {
  gam <- 2
  price_state <- (price-min_p)/(max_p-min_p)
  phi <- 1/(1+(price_state/(1-price_state))^gam)
  return(phi)
}

stats_df = data.frame(Wt=numeric(), Wh=numeric())

W0 <- 100000                      # MM's initial budget
p0 <- pv[1]
(xh <- x0 <- omega(p0)*W0/p0)     # initial value for x
(yh <- y0 <- W0 - p0*x0)          # for y
fill <- c()                       # every time a price movement trigers a buy or sell
floop <- TRUE

for (i in 2:length(pv))
{
  while ((pd<-(pv[i]<((1/fac)*p0))) | (pi<-(pv[i]>fac*p0)))  # check points for price decrease/increas
  {
    p0 <- (pd*(1/fac)+pi*fac)*p0
    W0 <- p0*x0+y0
    x0 <- omega(p0)*W0/p0
    y0 <- W0-p0*x0
    if (floop) {
      fill <- append(fill,i)
      floop <- FALSE
    }
  }
  stats_df <- rbind(stats_df, data.frame(Wt=pv[i]*x0+y0, Wh=pv[i]*xh+yh))
  floop <- TRUE
}

plot(stats_df$Wt, type = "l", col="red", lwd = 1)
lines(stats_df$Wh, type = "l", col="blue", lwd = 1)

for (j in 1:length(fill)) {
  abline(v=fill[j], col="grey", lwd=0.5)
}
# legend("topleft", c("mm", "hold"), bty = "n", col=c("red", "blue"), lty=1, cex=1)

ratio <- stats_df$Wt/stats_df$Wh
plot(ratio, type = "l", col="black", lwd = 1)
abline(h=1, lty=2)

