#loading required packages
  library("ggplot2")
  library(RGtk2)
  
#GFX
  window <- gtkWindow("toplevel" , show = FALSE)
  window['title'] = "Project Peyvandi"
  window$setDefaultSize(500 , 200)
  
  frame = gtkFrameNew("Dashboard")
  window$add(frame)
  
  window["visible"] = TRUE
  
  box1 = gtkVBoxNew()
  box1$setBorderWidth(30)
  frame$add(box1)
  
  label = gtkLabelNewWithMnemonic("Project Sections:")
  box1$packStart(label)
  
  box3 = gtkHBoxNew();
  box3$setBorderWidth(30)
  box1$packStart(box3)
  
  loadDataButton = gtkButton("Load Data")
  box3$packStart(loadDataButton , fill = F)
  
  box2 = gtkHBoxNew();
  box2$setBorderWidth(30)
  box1$packStart(box2)
  
  section1 = gtkButton("Section 1");
  box2$packStart(section1 , fill = F)
  
  section2 = gtkButton("Section 2");
  box2$packStart(section2 , fill = F)
  
  section3 = gtkButton("Section 3");
  box2$packStart(section3 , fill = F)
  
  section4 = gtkButton("Section 4");
  box2$packStart(section4 , fill = F)
  
  
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
    print(
      ggplot(frame1, aes(x = arg1)) + stat_ecdf() + stat_function(fun = second_cdf, color = "red") +
      geom_vline(xintercept = maxDiffAt, color = "blue")
    )
  }

#loading Data
  
  frame1 <- read.csv(file = "DataSet1.csv", header = FALSE)
  frame2 <- read.csv(file = "DataSet2.csv", header = FALSE)
  vec1 <- frame1[, 1]
  vec2 <- frame2[, 1]
  
  
  loadData <- function(button)
  {
    frame1 <- read.csv(file.choose(), header = FALSE)
    frame2 <- read.csv(file.choose(), header = FALSE)
    vec1 <- frame1[, 1]
    vec2 <- frame2[, 1]
  }
  
  
#part one
  #one sample ks test with given data in project document
  section1_function <- function(button){
    x <- c(0.58, 0.42, 0.52, 0.33, 0.43, 0.23, 0.58, 0.76, 0.53, 0.64) 
    result <- ks.test(x, "punif", 0, 1)
    print_ks(result)
    #visualizing test result
    frame <- data.frame(x)
    plot_ks(x, punif)
  }
  
  
#part two 
  #two sample ks test with out own data sets
  section2_function <- function(button){
    #normal with mean = 1, sd = 1
    set1 <- rnorm(n = 100, mean = 1, sd = 1)
    #normal with mean = 0, sd = 2
    set2 <- rnorm(n = 100, mean = 0, sd = 2)
    frame <- data.frame(set1, set2)
    result <- ks.test(set1, set2)
    print_ks(result)
    #visualizing test result
    plot_ks(set1, set2, FALSE)
    
  }
  
#part three
  #two sample ks test with excel file datas
  section3_function <- function(button){
    result <- ks.test(vec1, vec2)
    print_ks(result)
    #visualizing test result
    plot_ks(vec1, vec2, FALSE)  
  }
  
  
#part four
  #running t test on vec 1 and vec2
  section4_function <- function(button){
    print(t.test(vec1, vec2))
    #checking whether vec1 and vec2 are normal distributions with same variance
    ks.test(vec1, "pnorm", mean = mean(vec1), sd = sd(vec1))
    ks.test(vec2, "pnorm", mean = mean(vec2), sd = sd(vec2)) 
    #if at least one of the sets fails to be normal (has p-value less than 1%)
    #or they have large enough difference in variance then ttest can't be trusted here
    helpFrame <- data.frame(vec1)
    second_func <- ecdf(vec2)
    mean1 = mean(vec1)
    mean2 = mean(vec2)
    print(
      ggplot(helpFrame, aes(x = vec1)) + stat_ecdf() + stat_function(fun = second_func) +
      geom_point(aes(mean1, ecdf(vec1)(mean1), color = "red")) + geom_point(aes(mean2, second_func(mean2), color = "red"))
    )
  }
  
  gSignalConnect(section1, "clicked", section1_function)
  gSignalConnect(section2, "clicked", section2_function)
  gSignalConnect(section3, "clicked", section3_function)
  gSignalConnect(section4, "clicked", section4_function)
  gSignalConnect(loadDataButton , "clicked" , loadData)
  