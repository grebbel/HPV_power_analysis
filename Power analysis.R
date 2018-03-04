library(pwr)

#### pwr.chisq.test(w =, N = , df = , sig.level =, power = )
#### where w is the effect size,
#### N is the total sample size, 
#### and df is the degrees of freedom. 
#### Cohen suggests that w values of 0.1, 0.3, and 0.5 represent 
#### small, medium, and large effect sizes respectively.

## Yirgallem study, size:
size <- pwr.chisq.test(w=0.4,df=(3-1)*(3-1),N=NULL, sig.level=0.05, power=.8)

size

## Yirgallem study, power:
power <- pwr.chisq.test(w=0.4,df=(3-1)*(3-1),N=94, sig.level=0.05, power=NULL)

power


# Plot sample size curves for CHI_SQUARE of various sizes

# range of effect size
w <- seq(.1,.5,.01)
nw <- length(w)

# power values
p <- seq(.4,.9,.1)
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nw*np), dim=c(nw,np))
for (i in 1:np){
  for (j in 1:nw){
     result <- pwr.chisq.test(w = w[j], df = (3-1)*(3-1), N = NULL, sig.level = .05, power = p[i])
    samsize[j,i] <- ceiling(result$N)
  }
}

# set up graph
xrange <- range(w)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="effect size (w)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(w, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend) 
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Chis-square Studies\n
      Sig=0.05")
legend("topright", title="Power", as.character(p),
       fill=colors)







# Plot sample size curves for detecting CORRELATIONS of
# various sizes.

library(pwr)

# range of correlations
r <- seq(.1,.5,.01)
nr <- length(r)

# power values
p <- seq(.4,.9,.1)
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nr*np), dim=c(nr,np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(n = NULL, r = r[j],
                         sig.level = .05, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Correlation Coefficient (r)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend) 
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Correlation Studies\n
      Sig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)

