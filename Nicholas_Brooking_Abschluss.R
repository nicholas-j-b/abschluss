#############################################################################
#1

#load data
setwd("~/r-ws/abschluss/data/")
files <- list.files()

#load control data
data <- files[grep(".RData", files)]
load(data) -> laser

#read data
titles <- c("rank", "shots", "accuracy", "powers", "score") #  "player", "team",
files <- files[grepl("Laser_[0-9]+.*\\.txt", files)]
nums <- list() #list of player numbers
names <- list() #corresponding list of player names
data <- list() #corresponding data
for (i in 1:length(files)){
  nums <- c(nums, as.numeric(sub("_", "", substring(files[i], 7, 8))))
  names <- c(names, substring(files[i], regexpr("_[^0-9]{2,}", files[i]) + 1, nchar(files[i]) - 4))
  data[[i]] <- scan(files[i], what = "", sep = "\n")
}

#extract and order relevant information
struct <- list()
splitted <- list()
for (h in 1:length(data)){
  hits <- list()
  reg <- gregexpr("<[^/]+>", data[[h]])
  reg2 <- gregexpr("</", data[[h]])
  reg3 <- regexpr("^[A-Z]", data[[h]])
  splitted[[h]] <- strsplit(data[[h]], ";")[reg3 == 1]
  data.names <- list()
  data.vals <- list()
  for (i in 1:length(reg)){
    if (reg[[i]] != -1){
      data.names[[i]] <- substring(data[[h]][[i]], reg[[i]] + 1, reg[[i]] + attr(reg[[i]], "match.length") - 2)
      data.vals[[i]] <- substring(data[[h]][[i]], reg[[i]] + attr(reg[[i]], "match.length"), reg2[[i]] - 1)
    }
  }
  for (i in 1:length(titles)){
    struct[[titles[i]]] <- c(struct[[titles[i]]], as.numeric(data.vals[data.names == titles[i]]))
  }
  struct$player <- c(struct$player, as.character(names[h]))
  struct$team <- c(struct$team, as.character(data.vals[data.names == "team"]))
}
#alphabetise and name data
names(splitted) <- struct$player
splitted <- splitted[sort(names(splitted))]
split.order <- order(sapply(splitted[[1]], function(y) y[[1]]))

#############################################################################
#2

#create data frame
struct.frame <- data.frame(struct)
rownames(struct.frame) <- struct$player

#create you hit/hit you matricies
you.hit <- hit.you <- matrix(0, nrow = length(struct$player), ncol = length(struct$player))
for (i in 1:length(struct$player)){
  for (j in 1:length(struct$player)){
    you.hit[i, j] <- as.numeric(splitted[[i]][[split.order[j]]][2])
    hit.you[i, j] <- as.numeric(splitted[[i]][[split.order[j]]][3])
  }
}
#alphabetise and name
colnames(you.hit) <- colnames(hit.you) <- rownames(you.hit) <- rownames(hit.you) <- sort(struct$player)
struct.frame <- struct.frame[order(rownames(struct.frame)), ]

#test correctness against given matrix
sum(hit.you, na.rm = TRUE) == sum(you.hit, na.rm = TRUE)
all(rowSums(you.hit, na.rm = TRUE) == rowSums(you_hit, na.rm = TRUE))
all(colSums(you.hit, na.rm = TRUE) == colSums(you_hit, na.rm = TRUE))
all(rowSums(hit.you, na.rm = TRUE) == rowSums(hit_you, na.rm = TRUE))
all(colSums(hit.you, na.rm = TRUE) == colSums(hit_you, na.rm = TRUE))

#relative hit percentages
hit.you.rel <- sweep(hit.you, MARGIN = 1, FUN = "/", STATS = rowSums(hit.you, na.rm = TRUE))

#test correctness
all(hit.you.rel == hit_you_rel, na.rm = TRUE)
all(rowSums(hit.you.rel, na.rm = TRUE) == 1)
all(hit.you.rel >= 0, na.rm = TRUE)

#add you.hit and hit.you to dataframe
struct.frame <- cbind(struct.frame, you.hit = rowSums(you.hit, na.rm = TRUE), hit.you = rowSums(hit.you, na.rm = TRUE))

#############################################################################
#3

tab <- apply(you.hit, 1, FUN = function(x) tapply(x, INDEX = struct.frame$team, FUN = sum, na.rm = TRUE))
tab2 <- apply(hit.you, 1, FUN = function(x) tapply(x, INDEX = struct.frame$team, FUN = sum, na.rm = TRUE))

a.n <- b.n <- c.n <- d.n <- numeric(0)
for (i in 1:length(struct.frame$team)){
  b.n <- c(b.n, tab[struct.frame$team[[i]], i])
  a.n <- c(a.n, sum(tab[, i]) - b.n[i])
  d.n <- c(d.n, tab2[struct.frame$team[[i]], i])
  c.n <- c(c.n, sum(tab2[, i]) - d.n[i])
}
y.n <- struct.frame$score - struct.frame$accuracy - struct.frame$powers

#linear modell
betas <- solve(cbind(a.n, b.n, c.n, d.n)[1:4, ], y.n[1:4])
all(betas[1] * a.n + betas[2] * b.n + betas[3] * c.n + betas[4] * d.n == y.n)

#regression
model <- lm(y ~ -1 + a.n + b.n + c.n + d.n)
gammas <- round(model$coefficients, digits = 10) #rid floating point error
all(gammas[1] * a.n + gammas[2] * b.n + gammas[3] * c.n + gammas[4] * d.n == y.n)

#############################################################################
#4

f <- colorRampPalette(c("coral4", "tan1"))
vars <- c("score", "shots", "accuracy", "you.hit", "hit.you") 

#a function that draws a bar graph splitted by teams
plot.balk <- function(variable = "score", by = "team", breaks = 5, palette = NULL, leg = TRUE){
  cols <- palette(breaks)
  barplot(sapply(tapply(struct.frame[[variable]], struct.frame[[by]], 
                        FUN = function(x) cut(x, breaks = breaks)), table), col = cols,
          main = c("Comparing", variable, "for each team"), ylim = c(0, max(table(struct.frame$team)) * 1.35), 
          ylab = "Players")
  if(leg == TRUE){
    legend("topright", legend = c("best group", rep("", times = breaks - 2), "worst group"), pch = 15, col = rev(cols))
  }
}

#leg = FALSE for no legend
for (i in vars){
  dev.new()
  plot.balk(i, palette = f)
}

#############################################################################
#5

simulate <- function(time = 15, info.frame, hits){
  rate <- hits / 15
  rate[is.na(rate)] <- 0
  apply(rate, 2, FUN = function(x) rpois(length(x), time * x))


}


simulate(info.frame = info, hits = you_hit)























  
  
