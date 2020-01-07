findpeaks <-
  function (bthr, box, k, minstep, stepdif) {
    #bthr is a datframe returned by stepfinder.R
    
    mat <- as.matrix(abs(bthr$tscore))
    der <- rbind(NA, diff(mat, lag = 2) / 2, NA)
    pos2neg <- der[1:(nrow(der) - 1), , drop = FALSE] >= 0 &
      der[2:(nrow(der)), , drop = FALSE] <= 0 &
      !(der[1:(nrow(der) -
                 1), , drop = FALSE] == 0 &
          der[2:(nrow(der)), , drop = FALSE] ==
          0)
    spikes <- rbind(pos2neg, FALSE)
    spikes[is.na(spikes), ] <- FALSE
    
    ########################
    a4 <- spikes[, 1]
    
    bthr2 <- bthr[a4, ]
    #a6 <- abs(bthr2$amplitude)>minstep
    #bthr3 <- bthr2[a6,]
    bthr3 <- bthr2
    
    if (length(bthr3$tscore) > 1) {
      #remove overlapping steps (leave the one with a higher tscore)
      count <- 1
      overlaps <- c()
      for (i in 1:(length(bthr3$time) - 1)) {
        if ((bthr3[count + 1, ]$time - bthr3[count, ]$time) < box) {
          if (abs(bthr3[count, ]$tscore) < abs(bthr3[count + 1, ]$tscore)) {
            bthr3 <- bthr3[-count, ]
          } else {
            bthr3 <- bthr3[-(count + 1), ]
          }
          count <-
            count - 1 #compare the same step with the new-subsequent one
        }
        count <- count + 1
      }
    }
    
    # calc force and dwell times
    bthr3$force <- bthr3$mean2 * k
    bthr3$dwell <- NA
    dwell <- diff(bthr3$time)
    
    if (length(bthr3$tscore) > 1) {
      s <-
        abs((bthr3$mean2[2:nrow(bthr3)] - bthr3$mean1[1:(nrow(bthr3) - 1)])) < stepdif #5nm threshold; abs added 2019 07 12
      
      for (i in 1:(nrow(bthr3) - 1)) {
        if (s[i] == 1) {
          bthr3$dwell[i + 1] <- dwell[i]
        }
      }
    }#if
    
    bthr3
    
  }
