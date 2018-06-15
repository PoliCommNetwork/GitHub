# R Script to replicate the results of:
# The virtuous cycle of news diffusion on Facebook:
# effects of platform affordances and journalistic routines on news sharing

library("lme4")
library("moments")

Fmt <- function(x)
{
    if(x < 10)
        sprintf("%0.2f", x)
    else
        if(x < 1000)
            sprintf("%0.1f", x)
        else
            format(x, big.mark=",")
}

# Load data

load("Facebook.RData")

# Histograms of share count
par(mfrow = c(1, 2), mar = c(4, 4, 1, 2)+0.1)
hist(d$shares_count_fb, col = "lightgray", main = "", xlab = "Share count")
hist(d$logshare, col = "lightgray", main = "", xlab = "Logarithm of share count")

# Fortnight explained
par(mar = c(0, 0, 0, 0))
plot(NULL, xaxt="n", bty="n", yaxt ="n", xlim=c(-1, 31), ylim = c(-4, 2.3), xlab = "", ylab="")
rect(1,  0,  2, 1, col = "black")
rect(15, 0, 16, 1, col = "black")
rect(29, 0, 30, 1, col = "black")
for(i in 1:29)
    rect(i, 0, i+1, 1)
rect(1,   1.2, 15,  2.2, col = "lightgray") 
rect(15,  1.2, 29,  2.2, col = "lightgray") 
rect(2,  -0.2, 16, -1.2, col = "darkgray") 
rect(16, -0.2, 30, -1.2, col = "darkgray") 
legend("bottomleft", fill = c("black", "lightgray", "darkgray"), bty = "n",
       legend = c("Day with known number of followers",
                  "Fortnight before",
                  "Fortnight after"))
rm(i)

# Histograms of number of followers
options(scipen = 9)
par(mfrow = c(1, 2), mar = c(4, 4, 1, 2)+0.1)
hist(a$nflwrsI, col = "lightgray", xlab = "Number of followers", main = "")
hist(a$logNflwrsI, col = "lightgray", xlab = "Log of number of followers", main = "")

# Figure: Correlation between share count and number of followers
par(mar=c(4,4,0,2)+0.1)
plot(a$logshare ~ a$logNflwrsI, col = "#00000044",
     xlab = "Log of number of followers",
     ylab = "Mean log of share count")
text(11, 7, paste("r =", Fmt(cor(a$logshare, a$logNflwrsI))))
abline(lm(a$logshare ~ a$logNflwrsI))

# Logarithm of number of posts
a$lognpost <- log(a$npost)

# Figure: Share count and number of posts
par(mfrow=c(2, 2), mar=c(6, 4, 0.5, 2)+0.1)
plot(a$logShareSum ~ a$lognpost, col = "#00000033",
     ylab = "Log of sum of share count",
     xlab = "Log of number of posts", sub = "(a)")
x <- min(a$lognpost) + ((max(a$lognpost) - min(a$lognpost)) / 10)
y <- max(a$logShareSum) - ((max(a$logShareSum) - min(a$logShareSum)) / 10)
text(x, y, paste("r =", Fmt(cor(a$logShareSum, a$lognpost))))
abline(lm(logShareSum ~ lognpost, data = a))
par(mar=c(6, 6, 0.5, 0)+0.1)
plot(a$logshare ~ a$lognpost, col = "#00000033",
     ylab = "Mean of log of share count",
     xlab = "Log of number of posts", sub = "(b)")
y <- max(a$logshare) - ((max(a$logshare) - min(a$logshare)) / 10)
text(x, y, paste("r =", Fmt(cor(a$logshare, a$lognpost))))
abline(lm(logshare ~ lognpost, data = a))
par(mar=c(5, 4, 1.5, 2)+0.1)
plot(a$logShareSum ~ a$nflwrPost, col = "#00000066", sub = "(c)",
     xlab = "NF/NP",
     ylab = "Log of sum of share count")
text(min(a$nflwrPost) + ((max(a$nflwrPost) - min(a$nflwrPost)) / 10),
     max(a$logShareSum) - ((max(a$logShareSum) - min(a$logShareSum)) / 10),
     paste("r =", Fmt(cor(a$logShareSum, a$nflwrPost))))
abline(lm(a$logShareSum ~ a$nflwrPost))
par(mar=c(5, 6, 1.5, 0)+0.1)
plot(a$logshare ~ a$nflwrPost, col = "#00000066", sub = "(d)",
     xlab = "NF/NP",
     ylab = "Mean log of share count")
text(min(a$nflwrPost) + ((max(a$nflwrPost) - min(a$nflwrPost)) / 10),
     max(a$logshare) - ((max(a$logshare) - min(a$logshare)) / 10),
     paste("r =", Fmt(cor(a$logshare, a$nflwrPost))))
abline(lm(a$logshare ~ a$nflwrPost))

# Causality 1
a2 <- a[, c("page.name", "fortnightA", "logshare", "logShareSum", "logNflwrsI", "nflwrPost")]
b2 <- b[, c("page.name", "fortnightB", "logshare", "logShareSum", "logNflwrsD", "nflwrPost")]
a2 <- a2[order(a2$page.name, a2$fortnightA), ]
b2 <- b2[order(b2$page.name, b2$fortnightB), ]

a2$logshare.b <- NA
a2$logShareSum.b <- NA
a2$nflwrPost.b <- NA
for(i in levels(factor(a2$page.name)))
    for(j in 2:13){
        lsB <- a2$logshare[a2$page.name == i & a2$fortnightA == (j-1)]
        if(length(lsB))
            a2$logshare.b[a2$page.name == i & a2$fortnightA == j] <- lsB
        lsB <- a2$logShareSum[a2$page.name == i & a2$fortnightA == (j-1)]
        if(length(lsB))
            a2$logShareSum.b[a2$page.name == i & a2$fortnightA == j] <- lsB
        lsB <- a2$nflwrPost[a2$page.name == i & a2$fortnightA == (j-1)]
        if(length(lsB))
            a2$nflwrPost.b[a2$page.name == i & a2$fortnightA == j] <- lsB
}

b2$logNflwrsD.b <- NA
b2$nflwrPost.b <- NA
for(i in levels(factor(b2$page.name)))
    for(j in 2:12){
        lsB <- b2$logNflwrsD[b2$page.name == i & b2$fortnightB == (j-1)]
        if(length(lsB))
            b2$logNflwrsD.b[b2$page.name == i & b2$fortnightB == j] <- lsB
        lsB <- b2$nflwrPost[b2$page.name == i & b2$fortnightB == (j-1)]
        if(length(lsB))
            b2$nflwrPost.b[b2$page.name == i & b2$fortnightB == j] <- lsB
}

ms <- lm(logshare ~ logshare.b + logNflwrsI, data = a2)
mf <- lm(logNflwrsD ~ logNflwrsD.b + logshare, data = b2)
ss <- lm(logShareSum ~ logShareSum.b + logNflwrsI, data = a2)
sf <- lm(logNflwrsD ~ logNflwrsD.b + logShareSum, data = b2)

SubNames <- function(x)
{
    x <- sub("logNflwrsD.b", "Log of number of followers (lag)", x)
    x <- sub("logShareSum.b", "Log of share count sum (lag)", x)
    x <- sub("logshare.b", "Mean log of share count (lag)", x)
    x <- sub("logshare", "Mean log of share count", x)
    x <- sub("logNflwrsD", "Log of number of followers", x)
    x <- sub("logNflwrsI", "Log of number of followers", x)
    x <- sub("logShareSum", "Log of share count sum", x)
    x
}
names(ms$coefficients) <- SubNames(names(ms$coefficients))
names(mf$coefficients) <- SubNames(names(mf$coefficients))
names(ss$coefficients) <- SubNames(names(ss$coefficients))
names(sf$coefficients) <- SubNames(names(sf$coefficients))

# Test of causal relationship direction (I)
summary(ms) # MLS
summary(mf) # NF
summary(ss) # LSS
summary(sf) # NF

rm(lsB, i, j, ms, mf, ss, sf, SubNames)

# Causality 2
ms <- lm(logshare     ~ logshare.b     + nflwrPost, data = a2)
mf <- lm(nflwrPost ~ nflwrPost.b + logshare, data = b2)
ss <- lm(logShareSum  ~ logShareSum.b  + nflwrPost, data = a2)
sf <- lm(nflwrPost ~ nflwrPost.b + logShareSum, data = b2)

SubNames <- function(x)
{
    x <- sub("logShareSum.b", "Log of share count sum (lag)", x)
    x <- sub("logshare.b", "Mean log of share count (lag)", x)
    x <- sub("logshare", "Mean log of share count", x)
    x <- sub("logShareSum", "Log of share count sum", x)
    x <- sub("nflwrPost.b", "Number of followers/post (lag)", x)
    x <- sub("nflwrPost", "Number of followers/post", x)
    x
}
names(ms$coefficients) <- SubNames(names(ms$coefficients))
names(mf$coefficients) <- SubNames(names(mf$coefficients))
names(ss$coefficients) <- SubNames(names(ss$coefficients))
names(sf$coefficients) <- SubNames(names(sf$coefficients))

# "Test of causal relationship direction (II)"
summary(ms) # MLS
summary(mf) # NF/NP
summary(ss) # LSS
summary(sf) # NF/NP

rm(a2, b2, ms, mf, ss, sf, SubNames)

# Number of characters
BuildRow <- function(v)
{
    x <- v - d$nchar
    x[x > 0] <-  -1 * x[x > 0]
    c(sprintf("%0.3f", cor(d$logshare[d$type == "video"], x[d$type == "video"])),
      sprintf("%0.3f", cor(d$logshare[d$type == "photo"], x[d$type == "photo"])),
      sprintf("%0.3f", cor(d$logshare[d$type == "link"],  x[d$type == "link"])))
}

tab <- rbind(c("N. char 100",   BuildRow(100)),
             c("N. char 200",   BuildRow(200)),
             c("N. char 300",   BuildRow(300)),
             c("N. char 400",   BuildRow(400)),
             c("N. char 500",   BuildRow(500)),
             c("N. char 600",   BuildRow(600)),
             c("N. char 700",   BuildRow(700)),
             c("N. char 800",   BuildRow(800)),
             c("N. char 900",   BuildRow(900)),
             c("N. char 1000",  BuildRow(1000)),
             c("N. char 3000",  BuildRow(3000)),
             c("N. char 5000",  BuildRow(5000)),
             c("N. char 7000",  BuildRow(7000)),
             c("N. char 10000", BuildRow(10000)))

colnames(tab) <- c("Ideal number", "Video", "Photo", "Link")

# Correlation between logarithm of share count and different measures of the ideal number of characters
tab
rm(tab, BuildRow)

d$nchar4000 <- 4000 - d$nchar
d$nchar4000[d$nchar4000 > 0] <-  -1 * d$nchar4000[d$nchar4000 > 0]

# Average value of some variables aggregated by page according target public
tmp <- rbind("Log of share count" = tapply(pa$logshare, pa$west, mean),
             "Log of number of followers" = tapply(pa$logNflwrsI, pa$west, mean),
             "Log of number of posts" = tapply(pa$lognpost, pa$west, mean),
             "Regularity of posting" = tapply(pa$npost.reg, pa$west, mean, na.rm = TRUE),
             "Proportion of videos" = tapply(pa$video, pa$west, mean))
colnames(tmp) <- c("Not West", "West")
tmp

# Average value of some variables aggregated by page according origin of page
tmp <- rbind("Log of share count" = tapply(pa$logshare, pa$native, mean),
             "Log of number of followers" = tapply(pa$logNflwrsI, pa$native, mean),
             "Log of number of posts" = tapply(pa$lognpost, pa$native, mean),
             "Regularity of posting" = tapply(pa$npost.reg, pa$native, mean, na.rm = TRUE),
             "Proportion of videos" = tapply(pa$video, pa$native, mean))
colnames(tmp) <- c("Not native", "Native")
tmp

# HLM
d$npost440 <- 440 - d$npostA
d$npost440[!is.na(d$npostA) & d$npost440 > 0] <- -1 * d$npost440[!is.na(d$npostA) & d$npost440 > 0]
d$lognpost <- log(d$npostA)
ds <- d[!is.na(d$keyA),
        c("logshare", "logNflwrsI", "nflwrPost", "lognpost", "native",
          "npost.regA", "nchar4000", "west", "type", "keyA", "page.id")]
ds$page.id <- factor(ds$page.id)
ds$logshare <- as.numeric(scale(ds$logshare))
ds$logNflwrsI <- as.numeric(scale(ds$logNflwrsI))
ds$nflwrPost <- as.numeric(scale(ds$nflwrPost))
ds$lognpost <- as.numeric(scale(ds$lognpost))
ds$npost.regA <- as.numeric(scale(ds$npost.regA))
ds$nchar4000 <- as.numeric(scale(ds$nchar4000))
ds$west <- as.numeric(scale(ds$west == "Yes"))
ds$native <- as.numeric(scale(ds$native == "Yes"))
ds$video <- as.numeric(scale(ds$type == "video"))
ds$link <- as.numeric(scale(ds$type == "link"))
ds$photo <- as.numeric(scale(ds$type == "photo"))
ds$type <- NULL

# Multilevel regressions.
m1 <- lmer(logshare ~ logNflwrsI + lognpost + npost.regA + nchar4000 + photo + video + west + native + (1|keyA) + (1|page.id), data = ds)
m2 <- lmer(logshare ~ nflwrPost + npost.regA + nchar4000 + photo + video + west + native + (1|keyA) + (1|page.id), data = ds)

SubNames <- function(x)
{
    x <- sub("logNflwrsI", "Log of number of followers", x)
    x <- sub("lognpost", "Log of number of posts", x)
    x <- sub("nflwrPost", "NF/NP", x)
    x <- sub("npost.regA", "Regularity of posting", x)
    x <- sub("nchar4000", "Number of characters 4000", x)
    x <- sub("west", "Western country", x)
    x <- sub("native", "Native media page", x)
    x <- sub("photo", "Photo", x)
    x <- sub("video", "Video", x)
    x
}

sm1 <- summary(m1)
sm2 <- summary(m2)
rownames(sm1$coefficients) <- SubNames(rownames(sm1$coefficients))
rownames(sm2$coefficients) <- SubNames(rownames(sm2$coefficients))
sm1
sm2
rm(m1, m2, sm1, sm2)

# Appendix: Pages' country/region of coverage, average share count and average number of followers
tmp1 <- data.frame("Page" = tapply(d$page.name, d$page.name, function(x) as.character(x[1])),
                   "Country/Region" = tapply(d$country, d$page.name, function(x) as.character(x[1])),
                   "Share" = tapply(d$shares_count_fb, d$page.name, mean))
tmp2 <- tapply(a$nflwrsI, a$page.name, mean)
tmp2 <- data.frame("Page" = names(tmp2), "Followers" = tmp2)
app <- merge(tmp1, tmp2)
app$Share <- format(app$Share, digits = 2, big.mark = ",")
app$Followers <- format(app$Followers, digits = 6, big.mark = ",")
app
