#loading required packages
  library("ggplot2")

#defining functions
  print_ks <- function(arg1) {
    print(arg1)
    if (arg1[2] > 0.05) {
      print("Null hypothesis accepted. Signifance level = 5%")
    } else if (arg1[2] > 0.01) {
      print("Null hypothesis accpepted. Signifance level = 1%")
    } else {
      print(paste("Null hypothesis rejected. Alternative hypothesis is: ", arg1[3]))
    }
  }

  #arg2 is name of cdf in one_sample and second sample's vector in two sample mode
  plot_ks <- function(arg1, arg2, one_sample = TRUE) {
    first_cdf = ecdf(arg1)
    second_cdf = NULL
    if (one_sample) {
      second_cdf = arg2
    } else {
      second_cdf = ecdf(arg2)
    }
    frame1 <- data.frame(arg1)
    maxInd <- which.max(abs(first_cdf(arg1) - second_cdf(arg1)))
    maxDiffAt <- arg1[maxInd]
    ggplot(frame1, aes(x = arg1)) + stat_ecdf() + stat_function(fun = second_cdf, color = "red") +
      geom_vline(xintercept = maxDiffAt, color = "blue")
  }

#part one
  #one sample ks test with given data in project document
  x <- c(0.58, 0.42, 0.52, 0.33, 0.43, 0.23, 0.58, 0.76, 0.53, 0.64) 
  result <- ks.test(x, "punif", 0, 1)
  print_ks(result)
  #visualizing test result
  frame <- data.frame(x)
  plot_ks(x, punif)
  
#part two 
  #two sample ks test with out own data sets
  #normal with mean = 1, sd = 1
  set1 <- rnorm(n = 100, mean = 1, sd = 1)
  #normal with mean = 0, sd = 2
  set2 <- rnorm(n = 100, mean = 0, sd = 2)
  frame <- data.frame(set1, set2)
  result <- ks.test(set1, set2)
  print_ks(result)
  #visualizing test result
  plot_ks(set1, set2, FALSE)
  
#part three
  #two sample ks test with excel file datas
  frame1 <- read.csv(file.choose(), header = FALSE)
  frame2 <- read.csv(file.choose(), header = FALSE)
  vec1 <- frame1[, 1]
  vec2 <- frame2[, 1]
  result <- ks.test(vec1, vec2)
  print_ks(result)
  #visualizing test result
  plot_ks(vec1, vec2, FALSE)
  
#part four
  #running t test on vec 1 and vec2
  t.test(vec1, vec2)
  #checking whether vec1 and vec2 are normal distributions with same variance
  ks.test(vec1, "pnorm", mean = mean(vec1), sd = sd(vec1))
  ks.test(vec2, "pnorm", mean = mean(vec2), sd = sd(vec2)) 
  #if at least one of the sets fails to be normal (has p-value less than 1%)
  #or they have large enough difference in variance then ttest can't be trusted here
  helpFrame <- data.frame(vec1)
  second_func <- ecdf(vec2)
  mean1 = mean(vec1)
  mean2 = mean(vec2)
  ggplot(helpFrame, aes(x = vec1)) + stat_ecdf() + stat_function(fun = second_func) +
    geom_point(aes(mean1, ecdf(vec1)(mean1), color = "red")) + geom_point(aes(mean2, second_func(mean2), color = "red"))
  