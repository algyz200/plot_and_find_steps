stepfinder <-
  function (y, time, box, minstep, mindisp, freq) {
    #box - averaging window for step calc
    
    box <- box * freq  # nb of data points
    
    time <- time[!is.na(y)]
    y <- y[!is.na(y)]
    
    ###########
    
    my_t <-
      function(g1, g2) {
        #this runs about 6-7x faster than "t.test" adn gives identical result
        
        t_stat <- function(x) {
          m <-
            .Internal(mean(x))    #.internal(mean(x)) runs ~25% quicker than mean(x), however, might fail on updated R
          n <- length(x)
          var <- sum((x - m) ^ 2) / (n - 1)
          
          list(m = m, n = n, var = var)
        }
        
        t_g1 <- t_stat(g1)
        t_g2 <- t_stat(g2)
        
        se_total <- sqrt(t_g1$var / t_g1$n + t_g2$var / t_g2$n)
        r <- (t_g1$m - t_g2$m) / se_total
        m1 <- t_g1$m
        m2 <- t_g2$m
        list(r = r, m1 = m1, m2 = m2)
      }
    
    #################
    
    tscore <- as.numeric()
    amplitude <- as.numeric()
    mean1 <- as.numeric()
    mean2 <- as.numeric()
    
    timey <-
      data.frame(
        time,
        y,
        tscore = numeric(length(y)),
        amplitude = numeric(length(y)),
        mean1 = numeric(length(y)),
        mean2 = numeric(length(y))
      )
    
    repeats <- length(y) - box
    
    count <- box #start counting from the box+1
    
    results <- timey$tscore
    steps <- timey$tscore
    mean1 <- timey$mean1
    mean2 <- timey$mean2
    
    while (count < repeats) {
      #most time-consuming part
      a1 <- y[(count + 1):(count + box)]
      a1m <- .Internal(mean(a1))
      a2 <- y[(count - box):(count - 1)]
      a2m <- .Internal(mean(a2))
      
      if (a1m && a2m > mindisp) {
        if (abs(a1m - a2m) > minstep) {
          ttest <-  my_t(a1, a2)			#slow
          results[count + 1] <- ttest$r
          steps[count + 1]  <- ttest$m1 - ttest$m2
          mean1[count + 1] <- ttest$m1
          mean2[count + 1] <- ttest$m2
        }
      }
      
      
      count <- count + 1
      
    }
    
    timey$tscore <- results
    timey$amplitude <- steps
    timey$mean1 <- mean1
    timey$mean2 <- mean2
    
    timey
    
  }
