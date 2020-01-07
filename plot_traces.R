#Algis Toleikis 2015-2019 algirdas.toleikis@gmail.com

#plot traces from optical trapping of motor proteins
#options: 
# 0) change the 'filename' and 'dataDirectory' accordingly. 'codeDirectory' as well if needed
# 1) plot the whole file or just a section: minx maxx
# 2) filter data with a moving-average filter: n
# 3) **key feature**: find steps (based on a custom t-score finding algorithm - Cross and Carter Nature 2005)
#   adjust stepfinder parametres: box, thr, minstep, minF, stepdif
#   Note: The step-finder itself is in a separate file. Also - peak finder. 
# 4) x&y can be rotated
# 5) comments from the experiment can be added (if any): comments
# 6) plots are produced and these features are added: detected steps, comments, second y axis (force)

#file format
#txt file with a header of n-lines, each line starts like this "Hertz =  50000Hz"
#data with 3 columns: Time, x, y
#typically 3-10 mln lines

# The code is devided into these sections:

############### 0. Start settings #############################
############### 1. Read in the file #############################
############### 2. Extract and process the data #############################
############### 3. Find steps (if enabled) #############################
############### 4. Plot #############################
############### 5. Save steps (if enabled) #############################

library(data.table)

############### 0. Start settings #############################

filename <- "testfile.txt"

library(rstudioapi)
codeDirectory <-
  dirname(rstudioapi::getActiveDocumentContext()$path)

dataDirectory <-
  codeDirectory #change if the data is not located in the same folder as the code

n <- 20 #data filtering n-points

#plotting parametres
minx <- 50 #doesn't like if it is 0
maxx <- 60
int <- 8  #lines
rotation <- 0
comments <- 1

#stepfinder parametres
stepfind <- 1
box <- 0.008
thr <- 25
minstep <- 5 #nm
minF <- 1 #pN, later recalc as mindisp in nm
stepdif <- 4 # nm


############### 1. Read in the file #############################
dir <-
  regmatches(dataDirectory, regexpr("data.+$", dataDirectory))
dir <- paste(dir, filename, sep = " ")
setwd(dataDirectory)

#quick file read in
a <- file(filename, open = "r")
lines <- readLines(a, n = 10000)
lntoskip <-
  grep("Time", lines) #how many lines to skip (all metadata)

#frequency
lnfreq <- grep("Hertz", lines)
freq <-
  unlist (regmatches(lines[lnfreq], regexec("Hertz =  ([0-9]+)", lines[lnfreq])))
freq <- as.numeric(freq[2])

#read in file as a table to get time, x and y

myfile <-
  as.data.frame(fread(filename,  blank.lines.skip = TRUE, skip = lntoskip -
                        1))

############### 2. Extract and process the data #############################

time <- myfile[, 1]
y <- myfile[, 3]
x <- myfile[, 2]

#rotate
rot <- rotation / 180 * pi
x <- x * cos(rot) - y * sin(rot)
y <- x * sin(rot) + y * cos(rot)

##filtering
ma <- function(x, n) {
  filter(x, rep(1 / n, n), sides = 2)
} #moving average
#yfilt <- ma(y, n)
#xfilt <- ma(x, n)

##yfilt <- y


#time3 <- time[time>minx & time<maxx]
#yfilt3 <- yfilt[time>minx & time<maxx]
#xfilt3 <- xfilt[time>minx & time<maxx]

time3 <- time[time > minx & time < maxx]
yzoom <- y[time > minx & time < maxx]
xzoom <- x[time > minx & time < maxx]

yfilt3 <- ma(yzoom, n)
#yfilt3 <- medianFilter(yzoom, n)

xfilt3 <- ma(xzoom, n)


miny <- round(min(y[time > minx & time < maxx]))
maxy <- round(max(y[time > minx & time < maxx]))

#stiffness
lnk <- grep("Stiffness", lines)
kw <-
  unlist (regmatches(lines[lnk], regexec("Stiffness: (0.[0-9]+)", lines[lnk])))
kw <- as.numeric(kw[2])
lnlaser <- grep("Laser", lines)
laser <-
  unlist (regmatches(lines[lnlaser], regexec("Laser: ([0-9]+)", lines[lnlaser])))
laser <- as.numeric(laser[2])
k <- kw * laser / 1000
mindisp <- minF / k

force <- y * k

############### 3. Find steps (if enabled) #############################

########### stepfinder

if (stepfind == 1) {
  setwd(codeDirectory)
  
  source("stepfinder3.R")
  b <- stepfinder(yfilt3, time3, box, minstep, mindisp, freq)
  
  a5 <- abs(b$tscore) > thr
  bthr <- b[a5, ]
  
  source("findpeaks.R")
  bthr3 <- findpeaks(bthr, box, k, minstep, stepdif)
  
} #if stepfinder =1

############### 4. Plot #############################

par(mar=c(5,5,5,5)) 

plot(
  time3,
  yfilt3,
  col = "black",
  xlab = "Time, s",
  ylab = "Displacement, nm",
  type = "l",
  xlim = c(minx, maxx),
  main = dir,
  ylim = c(miny, maxy),
  yaxt = "n"
)
#lines(time3,xfilt3,col = "blue")
at <- seq(from = miny, to = maxy, by = int)
axis(side = 2, at = at)
abline(
  h = seq(from = miny, to = maxy, by = int),
  NULL,
  col = "gray",
  lty = 3
)


# Plot steps

if (stepfind == 1) {
  count <- 1
  
  for (i in bthr3$y) {
    xx <- bthr3$time[count]
    yy1 <- bthr3$mean1[count]
    yy2 <- bthr3$mean2[count]
    xxx <- c(xx - box, xx, xx, xx + box)
    yyy <- c(yy2, yy2, yy1, yy1)
    lines(xxx,
          yyy,
          type = "l",
          col = "green",
          lwd = 2)
    count <- count + 1
  }
}

############comments###

lnk <- grep("Comment[0-9]+ = ([1-9][0-9]+),(.+)", lines)
m <-
  unlist (regmatches(lines[lnk], regexec(
    "Comment[0-9]+ = ([1-9][0-9]+),(.+)", lines[lnk]
  )))

a <- c(FALSE, TRUE, FALSE)
b <- c(FALSE, FALSE, TRUE)
com_time <- as.numeric(m[a]) / freq
com_text <- m[b]

c <- seq(0, 20, by = 5)
c <- rep(c, 100)
c <- c[1:length(com_time)]

#####new

com_time2 <- c()
for (pp in com_time) {
  s <- which(abs(time3 - pp) == min(abs(time3 - pp)))
  
  com_time2 <- c(com_time2, s)
  
}

#####

if (length(com_time) > 0) {
  lines(
    com_time,
    yfilt3[com_time2],
    type = "p",
    pch = "|",
    cex = 1,
    col = 2
  )
  text(
    com_time,
    yfilt3[com_time2] + c,
    com_text,
    cex = 0.5,
    col = 2,
    adj = 0
  )
}


#######################
#}

## Force
if (TRUE) {
  par(new = T)
  plot(
    time,
    force,
    col = rgb(0, 0, 0, 0),
    axes = F,
    xlab = "",
    ylab = "",
    xlim = c(minx, maxx),
    ylim = c(miny * k, maxy * k)
  )
  axis(4)
  mtext("Force, pN", side = 4, line = 3)
}

## t score plot
if (stepfind == 1) {
  if (FALSE) {
    plot(b$time,
         b$tscore,
         type = "p",
         xlab = "Time, s",
         ylab = "t score")
    lines(
      bthr3$time,
      bthr3$tscore,
      type = "p",
      col = "red",
      lwd = "2"
    )
    curve(x * thr / x, add = T)
    curve(x * thr / x * (-1), add = T)
  }
}

############### 5. Save steps (if enabled) #############################

#output file
setwd(dataDirectory)
#if (stepfind == 1) {
if (FALSE) {
  dir_split <- unlist(strsplit(dataDirectory, "/"))
  dir <- paste(dataDirectory, filename, sep = "/")
  filename2 <-
    paste(dir_split[length(dir_split)], unlist(strsplit(filename, "[.]"))[1], sep =
            "_")
  filename2 <- paste(filename2, "_steps.txt", sep = "")
  write(dir, filename2, append = TRUE)
  write(
    c(
      "Settings: box, thr, minstep, mindisp",
      box,
      thr,
      minstep,
      mindisp
    ),
    filename2,
    append = TRUE
  )
  bthr3[, 2:7] <-
    round(bthr3[, 2:7], 2)
  bthr3$dwell <- round(bthr3$dwell, 5)
  write.table(bthr3, filename2, append = TRUE)
}
