#############################
### Moralistic Gods Chapter Script
### code prepped by Benjamin Grant Purzycki
### email: bgpurzycki@cas.au.dk
### Last updated: January 14, 2022

rm(list = ls())

library(reshape)
library(xtable)
library(rethinking)
library(shape)

setwd("C:/Users/au624473/Dropbox/Moralistic Supernatural Punishment/Workflow and Data")
setwd("C:/Users/Benjamin/Dropbox/Moralistic Supernatural Punishment/Workflow and Data")

Corner_text <- function(text, location = "topright"){
  legend(location,legend = text, bty ="n", pch = NA, cex = 1.3) 
}

Corner_text2 <- function(text, location="topleft"){
  legend(location,legend = text, bty = "n", pch = NA, cex = 1.3) 
}

mycol1 <-  rgb(255, 255, 255, max = 255, alpha = 100, names = "white")
mycol2 <- rgb(211, 211, 211, max = 255, alpha = 50, names = "darkgray") 


#############
# Original codes from EA and SCCS
# 1. absent or not reported
# 2. present but not active in human affairs
# 3. present and active in human affairs but not supportive of human morality
# 4. present, active, and specifically supportive of human morality
# 5. missing data
#############

###########################################################
### Standard Cross-Cultural Sample from DPLACE
###########################################################

sccs <- read.csv("SCCS_religion.csv")

vars <- c("var_id", "soc_id", "code")
sccs2 <- sccs[vars]

dat <- reshape(sccs2, 
               timevar = "var_id",
               idvar = "soc_id",
               direction = "wide")
names(dat) <- c("Society", "Juris_L", "Juris_B", "High_God", "World_Rel")

dat$High_God[is.na(dat$High_God)] <- 999 # recode missing values

counts <- table(dat$Juris_B, dat$High_God)
rownames(counts)[1] <- 0
rownames(counts)[2] <- 1
rownames(counts)[3] <- 2
rownames(counts)[4] <- 3
rownames(counts)[5] <- 4

counts.d <- counts[,1:4] # axe missing values
hgd <- prop.table(counts.d, 1) # row props (across high god var)
jgd <- prop.table(counts.d, 2) # column props (across juris. hier.)

xxx <- cbind(hgd[,1], hgd[,4])
colnames(xxx) <- c("Abs./Unr.", "MHG")

############################
### Test of MHG ~ Complexity
# create binary MHG var
dat$MHG <- NA
dat$MHG[dat$High_God == "1"] <- 0
dat$MHG[dat$High_God == "2"] <- 0
dat$MHG[dat$High_God == "3"] <- 0
dat$MHG[dat$High_God == "4"] <- 1
dat$MHG[dat$High_God == "999"] <- NA

# make complexity codes from 0 to 4 instead of 1 to 5
dat$complex <- NA
dat$complex <- dat$Juris_B - 1

lab <- c("MHG", "complex")
d <- dat[lab]
d <- d[complete.cases(d),] # axe missing

m1 <- quap(alist(
  MHG ~ dbinom(1 , p) ,
  logit(p) <- a + b*complex,
  c(a, b) ~ dnorm(0 , 1)
),
data = d)
modtab <- precis(m1, prob = .95)

logistic(modtab[1,1]) # 0 levels
logistic(modtab[1,1] + modtab[2,1]) # 1 level
logistic(modtab[1,1] + modtab[2,1]*2) # 2 level
logistic(modtab[1,1] + modtab[2,1]*3) # 3 level
logistic(modtab[1,1] + modtab[2,1]*4) # 4 level

post <- extract.samples(m1) # Extract posterior samples
comseq <- seq(0, 10, length.out = 10) # new complexity var
p_pred <- sapply(comseq, function(x) # predicting MHG across new complexity levels var
  with(post, {
    inv_logit(a + b*x)
  }))
comavg <- apply(p_pred, 2, mean) # new estimates
comPI <- apply(p_pred, 2, PI, prob = 0.95) # their CI's

## Moral Index
cercd <- read.csv("ALLSITES_V3.6.csv")
clabs <- c("SITE", "LGMURDIMP", "LGSTEALIMP", "LGLIEIMP")
cerc <- cercd[clabs]
cerc <- cerc[complete.cases(cerc), ]
cerc$MSUM <- cerc$LGMURDIMP+cerc$LGSTEALIMP+cerc$LGLIEIMP
cerc$SITE <- as.factor(cerc$SITE)

############################
### Plots
############################
par(mfrow = c(2, 2), 
    mai = c(0, 0, 0, 0),
    mar = c(4, 4, 3, 1)) #bottom, left, top, right

## DAG
plot(NA, xlim = c(-.5, 1.5), ylim = c(-.5, 1.5), axes = F,
     xlab = NA, ylab = NA)
Arrows(0, .05, 0, 1, lwd = 1, arr.type = "triangle") # 
legend(-0.25, .05, "S_O", bty = "n", bg = "white")
legend(-0.25, 1.6, "S_D", bty = "n")
Arrows(0.05, 0, .95, 0, lwd = 1, arr.type = "triangle") # 
legend(.8, 0.05, "G_O", bty = "n")
Arrows(1, 0.05, 1, 1, lwd = 1, arr.type = "triangle") # 
legend(.8, 1.6, "G_D", bty = "n")
Arrows(0.05, 0.05, .4, .4, lwd = 1, arr.type = "triangle") # 
Arrows(.9, 0.05, .55, .4, lwd = 1, arr.type = "triangle") # 
legend(.27, .85, "M", bty = "n")
Arrows(.42, .55, 0.1, 1, lwd = 1, arr.type = "triangle") # 
Arrows(.55, .55, .9, 1, lwd = 1, arr.type = "triangle") # 
Arrows(0.05, 1.2, 0.9, 1.2, lwd = 1, arr.type = "triangle") # 
Corner_text2(text = "(a)")
mtext(side = 1, line = 3, cex = 1, "Causal Model")

### Combined Frequency Plot
barplot(t(xxx), beside = T, col = c("white", "grey35"),
        ylim = c(0, 0.7))
mtext(side = 1, line = 3, cex = 1, "Jurisdictional Hierarchy Levels (SCCS)")
legend(2.7, .67, title = "Code", legend = c("Abs./Unreported", "Moral. High God"),
       fill = c("white", "grey35"))
box()
Corner_text(text="(b)")

### Prediction Plot
plot(comseq, comavg, type = "l",
     ylab = "Pr(Moralistic High God)", 
     xlab = NA,
     ylim = c(0, 1), add = T)
shade(comPI, comseq, col = col.alpha("grey", 0.2))
abline(h = 0.5, lty = 2, lwd = 0.5)
abline(v = 4, lty = 3, lwd = 0.5)
mtext(side = 1, line = 3, cex = 1, "Hierarchy Levels (SCCS)")
Corner_text2(text = "(c)")

### Density plot (12 sites)
plot(NA, xlim = c(-5, 17), ylim = c(0, .5), ylab = "Density", xlab = NA)
for (i in unique(cerc$SITE)){
  d <- subset(cerc, i == cerc$SITE)
  dens <- density(d$MSUM)
  polygon(dens, col = mycol2, border = "black")
}
mtext(side = 1, line = 3, cex = 1, "Moral Interest Scale")
Corner_text(text = "(d)")
abline(v = 0, lty = 2)

