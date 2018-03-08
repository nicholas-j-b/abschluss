# A short program written by Nicholas Brooking for the purpose of submitting
# to the University of Vienna for the course 'Statistisches Programmieren'.

#############################################################################
#1
#true when loading simulated answers
#run with false for initial iteration, then true for second
SIM <- FALSE

#load data
setwd("~/r-ws/abschluss/data/")
files <- list.files()

#load control data
data <- files[grep(".RData", files)]
load(data) -> laser

#read data
titles <- c("rank", "shots", "accuracy", "powers", "score")
titles2 <- c("player", "team")
if(SIM == FALSE){
  files <- files[grepl("Laser_[0-9]+.*\\.txt", files)]
} else {
  files <- files[grepl("sim_[0-9]+.*\\.txt", files)]
}
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
struct.frame$accuracy2 <-  struct.frame$you.hit / struct.frame$shots

#############################################################################
#3

#function for providing contigency table
#hits - you.hit style matrix
#frame - data frame with relevant data
get.tab <- function(hits, frame){
  apply(hits, 1, FUN = function(x) tapply(x, INDEX = frame$team, FUN = sum, na.rm = TRUE))
}
tab <- get.tab(you.hit, struct.frame)
tab2 <- get.tab(hit.you, struct.frame)

#function for calculating coefficients
#frame - data frame with relevant data
#tab - contingency table for you.hit data
#tab2 - contigency table for hit.you data
abcd <- function(frame, tab, tab2){
  a <- b <- c <- d <- numeric(0)
  for (i in 1:length(frame$team)){
    b <- c(b, tab[frame$team[[i]], i])
    a <- c(a, sum(tab[, i]) - b[i])
    d <- c(d, tab2[frame$team[[i]], i])
    c <- c(c, sum(tab2[, i]) - d[i])
  }
  mat <- cbind(a, b, c, d)
  rownames(mat) <- frame$player
  return(mat)
}

#calculate coefficients and test
co.matrix <- abcd(struct.frame, tab, tab2)
y.n <- struct.frame$score - struct.frame$accuracy - struct.frame$powers

#linear modell
betas <- solve(co.matrix[1:4, ], y.n[1:4])
all(co.matrix %*% betas == y.n)

#regression
model <- lm(y.n ~ -1 + co.matrix)
gammas <- round(model$coefficients, digits = 10) #rid floating point error
all(co.matrix %*% gammas == y.n)

#############################################################################
#4

#set colour palatte
f <- colorRampPalette(c("coral4", "tan1"))
vars <- c("score", "shots", "accuracy", "you.hit", "hit.you") 

#simulated results have no bonus accuracy statistic
if(SIM == TRUE){
  vars <- vars[!(vars == "accuracy")]
}

#a function that draws a bar graph split by teams
#variable - vector - variable with which to compare
#by - split data into groups to compare
#breaks - number of breaks in histogram
#palette - colour palette
#leg - logical - is there a legend?
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

#a function that simulates a game of laser tag
#time - length of game in minutes
#info.frame - dataframe with information upon which to base the simulation
#score.co - coefficients for calculating the score
simulate <- function(time = 15, info.frame, hits, score.co){
  
  #calculate hit rate, remove NAs for poisson calculation then place back in
  rate <- hits / 15
  temp.bool <- is.na(rate)
  rate[temp.bool] <- 0
  sim.you.hit <- apply(rate, 2, FUN = function(x) rpois(length(x), time * x))
  sim.you.hit[temp.bool] <- NA
  sim.hit.you <- t(sim.you.hit)
  rownames(sim.you.hit) <- info.frame$player
  rownames(sim.hit.you) <- info.frame$player
  
  #data fram to be returned
  sim.info <- data.frame(team = as.character(info.frame$team), player = info.frame$player, stringsAsFactors = FALSE)
  rownames(sim.info) <- info.frame$player

  sim.tab <- get.tab(sim.you.hit, struct.frame)
  sim.tab2 <- get.tab(sim.hit.you, struct.frame)

  #calculating score
  sim.co.matrix <- abcd(sim.info, sim.tab, sim.tab2)
  score <- sim.co.matrix %*% score.co
  
  #prepare return value, list with two matricies and a dataframe
  sim.info <- cbind(sim.info, sim.co.matrix, score)
  sim.info$shots <- floor(rowSums(sim.you.hit, na.rm = TRUE) / info.frame$accuracy2)
  sim.info$rank <- length(info.frame$team) - rank(sim.info$score) + 1
  sim.info$powers <- 0
  sim.info$accuracy <- 0
  sim.info$youhit <- rowSums(sim.you.hit, na.rm = TRUE)
  sim.info$hityou <- rowSums(sim.hit.you, na.rm = TRUE)
  
  list(sim.hit.you = sim.hit.you, sim.you.hit = sim.you.hit, sim.info = sim.info)
}

#simulate a 24hr game
sim <- simulate(time = 60 * 24, info.frame = struct.frame, hits = you_hit, score.co = betas)

#############################################################################
#6

#to save overwriting data, only write to disk in non-sim mode
if(SIM == FALSE){
  #set titles for text file
  titles3 <- c("team", "rank", "score", "shots", "accuracy", "youhit", "hityou", "powers")
  
  #function to write .txt files
  #info - data frame with simulated (or real) data
  #hits - matrix of you.hit style data
  write.laser <- function(info, hits){
    #prepare file name
    name <- paste0("sim_", info$rank, "_", info$player, ".txt")
    
    #open sink and write file
    sink(file = name)
    cat("<head>\n")
    for(i in titles3){
      cat(paste("  <", i, ">", info[i], "</", i, ">\n", collapse = "", sep = ""))
    }
    cat("</head>\n<body>\nplayer;you_hit;hit_you\n")
    cat(paste(colnames(hits), hits[info$player, ], hits[ , info$player], sep = ";", collapse = "\n"))
    cat("\n</body>")
    
    #close sink
    sink(file = NULL)
  }
  
  #write simulated data into memory
  for(i in 1:length(struct.frame$team)){
    write.laser(sim$sim.info[i, ], sim$sim.you.hit)
  }
}

# created folder 'data2' with laser data for 3+ teams
# code runs smoothly even with unevenly sized teams