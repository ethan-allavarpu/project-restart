library(dplyr)

# Import Data
homestop <- read.csv("../data/homestop.csv")
names(homestop)[[1]] <- "Club"
homestop <- homestop[order(homestop$Club), ]
awaystop <- read.csv("../data/awaystop.csv")
names(awaystop)[[1]] <- "Club"
awaystop <- awaystop[order(awaystop$Club), ]
homeend <- read.csv("../data/homeend.csv")
names(homeend)[[1]] <- "Club"
homeend <- homeend[order(homeend$Club), ]
awayend <- read.csv("../data/awayend.csv")
names(awayend)[[1]] <- "Club"
awayend <- awayend[order(awayend$Club), ]


# Analysis
homerestart <- homeend[, 2:9] - homestop[, 2:9]
homerestart <- data.frame(homestop$Club, homerestart)
homerestartpoints <- sum(homerestart$P)
awayrestart <- awayend[, 2:9] - awaystop[, 2:9]
awayrestart <- data.frame(awaystop$Club, awayrestart)
awayrestartpoints <- sum(awayrestart$P)

homestoppoints <- sum(homestop$P)
awaystoppoints <- sum(awaystop$P)

stoptotal <- homestoppoints + awaystoppoints
restarttotal <- homerestartpoints + awayrestartpoints

prop.test(c(homerestartpoints, homestoppoints), c(restarttotal, stoptotal))

hstopprop <- homestoppoints / stoptotal
hrestartprop <- homerestartpoints / restarttotal

barplot(c(hstopprop, hrestartprop),
        col = rgb(0, 0.5, 0.5, 0.5),
        ylim = c(0, 1), las = 2,
        main = "Proportion of Points Earned by Home Team\nBefore and After the Restart",
        ylab = "Proportion of Points for Home Team",
        xlab = "Season Stretch",
        cex.axis = 0.75,
        cex.names = 0.5)
axis(1, at = c(0.7, 1.9), labels = c("Before Suspension", "Project Restart"))
text(c(0.7, 1.9), c(hstopprop, hrestartprop) + 0.05,
     labels = round(c(hstopprop, hrestartprop), 4))