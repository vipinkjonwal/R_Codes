library(cubature)
#BASIC_FUNCTIONS

USERINPUT <- function(){
  #This function takes the input from command line seperated by enter.
  #x will be a list, which is to be converted into numreric for computation purpose.
  cat("Enter data seperated by enter: ")
  x <<- list()
  index <- 1
  y <- readline("")
  while(y != ''){
    x[index] <<- y
    index <- index + 1
    y <- readline("")    
  }
}

READFROMCSV <- function() {
  #This function reads the CSV file from Local Storage.
  library(data.table)
  fileData <<- fread(file.choose())
  x1 <<- fileData$x1
  x2 <<- fileData$x2
  y <<- fileData$y
}

INPUT <- function() {
  #This function lets the user choose from the option of READFROMCSV or USERINPUT. 
  cat('\f')
  cat('\t\t***** CHOOSE DATA ENTRY METHOD ******\n\n')
  cat('1. READ FROM CSV\n')
  cat('2. COMMAND LINE INPUT\n')
  inputchoice <- readline(prompt = "Enter the choice: ")
  inputchoice <- as.integer(inputchoice)
  
  if(inputchoice == 1){
    READFROMCSV()
  }
  if(inputchoice == 2){
    USERINPUT()
    x1 <<- as.numeric(x)
  }
}

MEAN <- function(x){
  sum <- 0
  n <- length(x)
  for (i in x) {
    sum <- sum + i
  }
  calculatedMean <- sum/n
  return(calculatedMean)
}

VARIANCE <- function(x){
  calculatedMean <- MEAN(x)
  n <- length(x)
  numerator <- 0
  for (i in x) {
    numerator <- (numerator + ((i - calculatedMean)^2))
  }
  calculatedVar <- (numerator / n)
  return(calculatedVar)
}

VARIANCE2 <- function(x){
  #This is sample variance with degree of freedom n-1.
  calculatedMean <- MEAN(x)
  sumOfXiSquares <- 0
  n <- length(x)
  for (i in x) {
    sumOfXiSquares <- (sumOfXiSquares + (i^2))
  }
  calculatedVar <- (sumOfXiSquares - n*(calculatedMean^2))/(n-1)
  return(calculatedVar)
}

SD <- function(x) {
  calculatedVar <- VARIANCE2(x)
  calculatedSD <- sqrt(calculatedVar)
  return (calculatedSD)
}

MEDIAN <- function(x) {
  x <- sort(x)
  n <- length(x)
  calculatedMedian <- 0
  index <- 0
  if((n %% 2) != 0) {
    index <- ((n + 1) / 2)
    calculatedMedian <- x[index] 
  }
  else {
    index <- (n / 2) 
    calculatedMedian <- ((x[index] + x[index + 1]) / 2)
  }
  return(calculatedMedian)
}

MODE <- function(x) {
  uniqueData <<- unique(x)
  count <<- array()
  
  for ( i in 1:(length(uniqueData))) {
    len <- length(grep(uniqueData[i],x))
    count[i] <<- len
  }
  index <<- which(count == max(count))
  
  if( length(index) == 1 ){
    finalMode <- uniqueData[index]
    return(finalMode)
  }
  else {
    return(-1)
  }
}

MEANABSDEV <- function(x) {
  calculatedMean <- MEAN(x)
  n <- length(x)
  calculatedAbsMeanDev <- 0
  for (i in x) {
    if (i > calculatedMean) {
      calculatedAbsMeanDev <- calculatedAbsMeanDev + (i - calculatedMean)
    }
    else {
      calculatedAbsMeanDev <- calculatedAbsMeanDev + (calculatedMean - i)
    }
  }
  return (calculatedAbsMeanDev/n)
}

MINIMUM <- function(x) {
  minValue <- x[1]
  for (i in x) {
    if (i < minValue) {
      minValue <- i
    }
  }
  return (minValue)
}

MAXIMUM <- function(x) {
  maxValue <- x[1]
  for (i in x) {
    if (i > maxValue) {
      maxValue <- i
    }
  }
  return (maxValue)
}

RANGE <- function(x) {
  return (MAXIMUM(x) - MINIMUM(x))
}

QUARTILES <- function(x) {
  x <- sort(x)
  n <- length(x)
  indexQ1 <- (n/4)+1
  indexQ3 <- (3*n/4)+1
  q1 <- x[indexQ1]
  q2 <- MEDIAN(x)
  q3 <- x[indexQ3]
  
  
  calculatedQuartiles <- cbind(q1, q2, q3)
  return (calculatedQuartiles)
}

IQR <- function(x) {
  calculatedQuartiles <- QUARTILES(x)
  q3 <- calculatedQuartiles[3]
  q1 <- calculatedQuartiles[1]
  return (q3 - q1)
}

SKEWNESS <- function(x) {
  calculatedMean <- MEAN(x)
  n <- length(x)
  numerator <- 0
  for (i in x) {
    numerator <- (numerator + ((i - calculatedMean)^3))
  }
  denominator <- (n * (SD(x)^3))
  g1 <- (numerator / denominator)
  return (g1)
}

KURTOSIS <- function(x) {
  calculatedMean <- MEAN(x)
  n <- length(x)
  numerator <- 0
  for (i in x) {
    numerator <- (numerator + ((i - calculatedMean)^4))
  }
  denominator <- (n * (VARIANCE(x)^2))
  g2 <- ((numerator / denominator) - 3)
  return (g2)
}

MOMENTS <- function(x) {
  #Returns the values of first four quartiles.
  calculatedMean <- MEAN(x)
  n <- length(x)
  m1 <- 0
  m2 <- 0
  m3 <- 0
  m4 <- 0
  
  for (i in x) {
    m1 <- m1 + (i - calculatedMean)
    m2 <- m2 + ((i - calculatedMean)^2)
    m3 <- m3 + ((i - calculatedMean)^3)
    m4 <- m4 + ((i - calculatedMean)^4)
  }
  
  M1 <- formatC(m1,digits = 6, format = "f")
  M2 <- formatC(m2,digits = 6, format = "f")
  M3 <- formatC(m3,digits = 6, format = "f")
  M4 <- formatC(m4,digits = 6, format = "f")
  
  calculatedMoments <- cbind(M1, M2, M3, M4)
  return (calculatedMoments)
}

CORRELATION <- function(x,y){
  #Returns the value of Karl's Pearson of Correlation Coefficient.
  n <- length(x)
  
  sumX <- 0
  for(i in 1:n){
    sumX <- sumX + x[i]
  }
  
  sumY <- 0
  for(i in 1:n){
    sumY <- sumY + y[i]
  }
  
  prodXY <- 0
  for(i in 1:n){
    prodXY <- prodXY + x[i]*y[i]
  }
  
  sumXSq <- 0
  for(i in 1:n){
    sumXSq <- sumXSq + x[i]*x[i]
  }
  
  sumYSq <- 0
  for(i in 1:n){
    sumYSq <- sumYSq + y[i]*y[i]
  }
  
  numerator <- ((n*prodXY) - (sumX*sumY))
  denominator <- ((((n*sumXSq) - (sumX^2))*((n*sumYSq) - (sumY^2)))^(0.5))
  r <- numerator/denominator
  return(r)
  
}

MULTREG <- function(x1,x2,y){
  #This is Multiple Linear Regression.
  #Assumptions : Works for 3 varibles only, 2 independent and one dependent.
  A <- matrix(nrow = 3,ncol = 3)
  A[1,1] <- length(y)
  A[1,2] <- sum(x1)
  A[1,3] <- sum(x2)
  A[2,1] <- sum(x1)
  A[2,2] <- sum(x1^2)
  A[2,3] <- sum(x1*x2)
  A[3,1] <- sum(x2)
  A[3,2] <- sum(x2*x1)
  A[3,3] <- sum(x2^2)
  
  B <- matrix(nrow = 3,ncol = 1)
  B[1,1] <- sum(y)
  B[2,1] <- sum(x1*y)
  B[3,1] <- sum(x2*y)
  
  X <- solve(A,B)
  return(X)
}

FACTORIAL <- function(n){
  #BAsic Factorial function.
  if(n == 0 || n == 1){
    return(1)
  }
  else{
    product <- 1
    for(i in 1:n-1){
      product <- product + product*i
    }
    return(product)
  }
}

PERMUTATION <- function(n,r){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : n can take positive value greater than or equal to 1.
  #               r can takes value less than or equal to n.
  #Returns nPr
  numerator <- FACTORIAL(n)
  denominator <- FACTORIAL(n-r)
  nPr <- numerator/denominator
  return(nPr)
}

COMBINATION <- function(n,r){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : n can take positive value greater than or equal to 1.
  #               r can takes value less than or equal to n.
  #Returns nCr
  numerator <- FACTORIAL(n)
  denominator <- FACTORIAL(r)*FACTORIAL(n-r)
  nCr <- numerator/denominator
  return(nCr)
}

BASICPROBABILITY <- function(favorable,total){
  #Assumptions : Works well for valid input i.e, favorable <= total and both positive.
  probability <- favorable/total
  return(probability)
}

BAYESTHEOREM <- function(pofAi, pofBAi, i) {
  len <- length(pofAi)
  numerator <- as.numeric(pofAi[i])*as.numeric(pofBAi[i])
  denominator <- 0
  
  for(j in 1:len){
    product <- as.numeric(pofAi[j])*as.numeric(pofBAi[j])
    denominator <- denominator + product
  }
  
  result <- (numerator/denominator)
  pofAiB <- formatC(result, digits = 6, format = "f")
  return(pofAiB)
}

UNIFORMDISTRIBUTION <- function(total){
  #Assumptions : total >= 1
  probability <- 1/total
  return(probability)
}

BERNOULLI <- function(x,p){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : x can take values either 0 or 1.
  #               p can takes value in range 0-1, both inclusive.
  probability <- (p**x)*((1-p)**(1-x))
  return(probability)
}

BINOMIALDISTRIBUTION <- function(n,x,p){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : n can take positive value greater than or equal to 1.
  #               x can takes value less than or equal to n. 
  #               p can takes value in range 0-1, both inclusive.
  probability <- COMBINATION(n,x)*(p**x)*((1-p)**(n-x))
  return(probability)
} 

GEOMETRIC <- function(k,p){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : k is Number of trials, greater than or equal to 1.
  #               p is probability of success, ranging between 0-1, both inclusive.
  
  probability <- ((1-p)**(k-1))*p
  return(probability)
}

HYPERGEOMETRIC <- function(N,K,n,k){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : N greater than or equal to n.
  #               K greater than or equal to k.
  #               N-K greater than or equal to n-k.
  
  #N : Population size.
  #K : Number of success states.
  #n : Number of draws.
  #k : Number of observed successes.
  probability <- (COMBINATION(K,k)*COMBINATION((N-K),(n-k)))/COMBINATION(N,n)
  return(probability)
}

NEGATIVEBIN <- function(k,r,p){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : k+r-1 greater than or equal to k (Number of successes).
  #               p is probability ranging from 0-1, both inclusive.
  
  #k : Number of successes.
  #r : Number of failures.
  #p : Probability.
  
  probability <- COMBINATION((k+r-1),k)*(p**k)*((1-p)**r)
  return(probability)
}

POISSON <- function(lambda,k){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : k is a whole number.

  e <- 2.718281
  probability <- ((e**(-lambda))*(lambda**k))/FACTORIAL(k)
  return(probability)
}

MULTINOMIAL <- function(x1,x2,x3,n,p1,p2,p3){
  #Assumptions  : Works well with valid inputs and for three random variables only.
  #Valid Inputs : n is a natural number, also n should be x1+x2+x3
  #               xi's should be positive.
  #               pi's should be in range 0-1, both inclsuive.
  
  
  facN <- FACTORIAL(n)
  facx1 <- FACTORIAL(x1)
  facx2 <- FACTORIAL(x2)
  facx3 <- FACTORIAL(x3)
  probability <- (facN/(facx1*facx2*facx3))*(p1**x1)*(p2**x2)*(p3**x3)
  return(probability)
}

MULTIHYPGEO <- function(x1,x2,x3,x4,M1,M2,M3,M4){
  #Assumptions  : Works well with valid inputs and for four random variables only.
  #Valid Inputs : xi's should be positive.
  #               Mi's should be positive.
  
  N <- M1+M2+M3+M4
  n <- x1+x2+x3+x4
  numerator <- COMBINATION(M1,x1)*COMBINATION(M2,x2)*COMBINATION(M3,x3)*COMBINATION(M4,x4)
  probability <- numerator/COMBINATION(N,n)
  return(probability)
}

UNICONTINOUS <- function(lowX,highX,a,b){
  if(lowX >= a && highX <= b){
    tempFun <- function(x){
      return(1/(b-a))
    }
    integralValue <- adaptIntegrate(tempFun,lowX,highX)$integral
    return(integralValue)
  }
  
  else if (lowX >= a && highX > b && lowX <= b){
    tempFun <- function(x){
      return(1/(b-a))
    }
    integralValue <- adaptIntegrate(tempFun,lowX,b)$integral
    return(integralValue)
  }
  
  else if (lowX < a && highX <= b && highX >= a){
    tempFun <- function(x){
      return(1/(b-a))
    }
    integralValue <- adaptIntegrate(tempFun,a,highX)$integral
    return(integralValue)
  }
  
  else if (lowX < a && highX > b){
    tempFun <- function(x){
      return(1/(b-a))
    }
    integralValue <- adaptIntegrate(tempFun,a,b)$integral
    return(integralValue)
  }
  
  else if ((lowX < a && highX < a)){
    return(0)
  }
  
  else if ((lowX > b && highX > b)){
    return(0)
  }
  
}

NORMALDIST <- function(lowX,highX,popMean,popVariance){
  tempFun <- function(x){
    numerator <- exp((-0.5)*(((x - popMean)/popVariance)^2))
    denominator <- ((2*pi*popVariance)^(0.5))
    return(numerator/denominator)
  }
  integrateValue <- integrate(tempFun,lowX,highX)$value
  return(integrateValue)
}

myGamma <- function(alpha){
  tempFun <- function(x){
    var <- (x^(alpha-1))*(exp(-x))
    return(var)
  }
  integrateValue <- integrate(tempFun,0,Inf)$value
  return(integrateValue)
}

GAMMADIST <- function(lowX,highX,alpha,beta){
    tempFun <- function(x){
      numerator <- (x^(alpha-1))*(exp(-x/beta))
      denominator <- (beta^alpha)*myGamma(alpha)
      return(numerator/denominator)
    }
    integralValue <- adaptIntegrate(tempFun,lowX,highX)$integral
    return(integralValue)
}

CHISQDIST <- function(n,samVariance,popVariance){
  return(((n-1)*samVariance)/popVariance)
}

TDIST <- function(n,samMean,popMean,samSD) {
  t <- ((samMean - popMean)/(samSD/(n^(0.5))))
  return(t)
}

FDIST <- function(samVar1,popVar1,samVar2,popVar2){
  return((samVar1/popVar1)/(samVar2/popVar2))
}

ZDIST <- function(samMean,popMean,popSD,n){
  z <- ((samMean - popMean)/(popSD/(n^(0.5))))
  return(z)
}

SIGNTEST <- function(x,popMean){
  n <- length(x)
  finalN <- n
  countPlus <- 0
  for (i in 1:n){
    if(x[i] > popMean){
      countPlus <- countPlus + 1
    }
    
    if(x[i] == popMean){
      finalN <- finalN - 1
    }
  }
  retValues <- c(countPlus,finalN)
  return(retValues)
}

HISTOGRAM <- function(x){
  x <<- as.numeric(x)
  hist(x, col="CYAN4", main="HISTOGRAM", xlab="x-axis", ylab="y-axis")
}

LINEGRAPH <- function(x){
  x <<- as.numeric(x)
  plot(x, type="l", col="GREEN", main="LINEGRAPH", xlab="x-axis", ylab="y-axis")
}

BARGRAPH <- function(x){
  x <<- as.numeric(x)
  barplot(x, col="RED", main="BAR GRAPH", xlab="x-axis", ylab="y-axis")
}

PIECHART <- function(x){
  x <<- as.numeric(x)
  pie(x, col=rainbow(length(x)), main="PIE CHART")
}

SCATTERPLOT <- function(x,y){
  x <<- as.numeric(x)
  y <<- as.numeric(y)
  plot(x, y, col="BLUE", main="SCATTER PLOT", xlab="x-axis", ylab="y-axis")
}

BOXPLOT <- function(x){
  x <<- as.numeric(x)
  boxplot(x, y, col="BLUE", main="BOX PLOT", xlab="x-axis", ylab="y-axis")
}

QQPLOT <- function(x,y){
  x <<- as.numeric(x)
  y <<- as.numeric(y)
  qqplot(x, y, col="BLUE", main="Q-Q PLOT")
  qqnorm(y)
  qqline(y, col="GREEN")
}

STEMLEAFPLOT <- function(x){
  x <<- as.numeric(x)
  stem(x)
}

PARETOCHART <- function(x){
  x <<- as.numeric(x)
  x <<- sort(x, decreasing = TRUE)
  library(qcc)
  pareto.chart(x, main="PARETO CHART", ylab="Frequency", ylab2="Cumulative Percentage", xlab="x-axis")
}


#SUB_MENUS
DESCRIPTIVEANALYSIS <- function(){
  
  choice <- 'y'
  
  while(choice == 'y'){
    cat('\f')
    cat('\t\t***** DESCRIPTIVE ANALYSIS ******\n\n')
    
    cat('1. MEAN\n')
    cat('2. MEDIAN\n')
    cat('3. MODE\n')
    cat('4. VARIANCE\n')
    cat('5. STANDARD DEVIATION\n')
    cat('6. MEAN ABSOLUTE DEVIATION\n')
    cat('7. RANGE\n')
    cat('8. QUARTILES\n')
    cat('9. INTER QUARTILE RANGE (IQR)\n')
    cat('10. MINIMUM\n')
    cat('11. MAXIMUM\n')
    cat('12. SKEWNESS\n')
    cat('13. KURTOSIS\n')
    cat('14. MOMENTS\n')
    
    tempSubChoice1 <- readline(prompt = "Enter the choice: ")
    subChoice1 <- as.integer(tempSubChoice1)
    
    if(subChoice1 > 0 && subChoice1 < 15){
      
      if(subChoice1 == 1){
        cat('\n\t\tMEAN\n\n')
        INPUT()
        answer <- MEAN(x1)
        cat("Mean is",answer)
      }
      
      if(subChoice1 == 2){
        cat('\n\t\tMEDIAN\n\n')
        INPUT()
        answer <- MEDIAN(x1)
        cat("Median is",answer)
      }
      
      if(subChoice1 == 3){
        cat('\n\t\tMODE\n\n')
        INPUT()
        answer <- MODE(x1)
        cat("MODE is",answer)
      }
      
      if(subChoice1 == 4){
        cat('\n\t\tVARIANCE\n\n')
        INPUT()
        answer <- VARIANCE2(x1)
        cat("Variance is",answer)
      }
      
      if(subChoice1 == 5){
        cat('\n\t\tSTANDARD DEVIATION\n\n')
        INPUT()
        answer <- SD(x1)
        cat("Standard Deviation is",answer)
      }
      
      if(subChoice1 == 6){
        cat('\n\t\tMEAN ABSOLUTE DEVIATION\n\n')
        INPUT()
        answer <- MEANABSDEV(x1)
        cat("Mean Absolute Deviation is",answer)
      }
      
      if(subChoice1 == 7){
        cat('\n\t\tRANGE\n\n')
        INPUT()
        answer <- RANGE(x1)
        cat("Range is",answer)
      }
      
      if(subChoice1 == 8){
        cat('\n\t\tQUARTILES\n\n')
        INPUT()
        answer <- QUARTILES(x1)
        cat("Quartiles are",answer)
      }
      
      if(subChoice1 == 9){
        cat('\n\t\tINTER QUARTILE RANGE\n\n')
        INPUT()
        answer <- IQR(x1)
        cat("Inter Quartile Range is",answer)
      }
      
      if(subChoice1 == 10){
        cat('\n\t\tMINIMUM\n\n')
        INPUT()
        answer <- MINIMUM(x1)
        cat("Minimum is",answer)
      }
      
      if(subChoice1 == 11){
        cat('\n\t\tMAXIMUM\n\n')
        INPUT()
        answer <- MAXIMUM(x1)
        cat("Maximum is",answer)
      }
      
      if(subChoice1 == 12){
        cat('\n\t\tSKEWNESS\n\n')
        INPUT()
        answer <- SKEWNESS(x1)
        cat("Skewness is",answer)
      }
      
      if(subChoice1 == 13){
        cat('\n\t\tKURTOSIS\n\n')
        INPUT()
        answer <- KURTOSIS(x1)
        cat("Kurtosis is",answer)
      }
      
      if(subChoice1 == 14){
        cat('\n\t\tMOMENTS\n\n')
        INPUT()
        answer <- MOMENTS(x1)
        cat("Moments are",answer)
      }
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}

PREDANALYSIS <- function(){
  
  choice <- 'y'
  
  while(choice == 'y'){
    
    cat('\f')
    cat('\t\t***** PREDICTIVE ANALYSIS ******\n\n')
    
    cat('1. CORRELATION\n')
    cat('2. MULTIPLE LINEAR REGRESSION\n')
    
    tempSubChoice2 <- readline(prompt = "Enter the choice: ")
    subChoice2 <- as.integer(tempSubChoice2)
    
    if(subChoice2 > 0 && subChoice2 < 3){
      if(subChoice2 == 1){
        cat('\n\t\tCORRELATION\n\n')
        cat("Choose a CSV File from Local Storage\n")
        READFROMCSV()
        r <- CORRELATION(x1,y)
        cat("Correlation Coefficient is ",r)
      } 
      
      if(subChoice2 == 2){
        cat('\n\t\tMULTIPLE LINEAR REGRESSION\n\n')
        cat("Choose a CSV File from Local Storage\n")
        READFROMCSV()
        X <- MULTREG(x1,x2,y)
        b0 <- X[1,1]
        b1 <- X[2,1]
        b2 <- X[3,1]
        cat("Intercept coefficient is",b0,'\n')
        cat("First slope coefficient is",b1,'\n')
        cat("Second slope coefficient is",b2,'\n')
        
      } 
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}

PROBABILITYANALYSIS <- function(){
  
  choice <- 'y'
  
  while(choice == 'y'){
    
    cat('\f')
    cat('\t\t***** PROBABILITY ANALYSIS ******\n\n')
    
    cat('1. PERMUTATION\n')
    cat('2. COMBINATION\n')
    cat('3. BASIC PROBABILITY\n')
    cat('4. CONDITIONAL PROBABILITY\n')
    cat('5. BAYES THEOREM\n')
    
    tempSubChoice3 <- readline(prompt = "Enter the choice: ")
    subChoice3 <- as.integer(tempSubChoice3)
    
    if(subChoice3 > 0 && subChoice3 < 6){
      
      if(subChoice3 == 1){
        cat('\n\t\tPERMUTATION\n\n')
        n <- readline(prompt = "Enter 'n': ")
        nInt <- as.integer(n)
        r <- readline(prompt = "Enter 'r': ")
        rInt <- as.integer(r)
        cat('nPr is ')
        nPr <- PERMUTATION(nInt,rInt)
        cat(nPr)
      }
      
      if(subChoice3 == 2){
        cat('\n\t\tCOMBINATION\n\n')
        n <- readline(prompt = "Enter 'n': ")
        nInt <- as.integer(n)
        r <- readline(prompt = "Enter 'r': ")
        rInt <- as.integer(r)
        cat('nCr is ')
        nCr <- COMBINATION(nInt,rInt)
        cat(nCr)
      }
      
      if(subChoice3 == 3){
        cat('\n\t\tBASIC PROBABILITY\n\n')
        tempFavorable <- readline(prompt = "Enter the favorable outcomes: ")
        favorable <- as.integer(tempFavorable)
        tempTotal <- readline(prompt = "Enter the total possible outcomes: ")
        total <- as.integer(tempTotal)
        cat('Probability is ')
        probability <- BASICPROBABILITY(favorable,total)
        finalAnswer <- formatC(probability,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice3 == 4){
        cat('\n\t\tCONDITIONAL PROBABILITY\n\n')
        
        tempSampleSpace <- readline(prompt = "Enter the cardinality of sample space: ")
        sampleSpace <- as.integer(tempSampleSpace)
        tempIntersection <- readline(prompt = "Enter the cardinality of A intersection B: ")
        intersection <- as.integer(tempIntersection)
        tempCardinalityA <- readline(prompt = "Enter the cardinality of A: ")
        cardinalityA <- as.integer(tempCardinalityA)
        tempCardinalityB <- readline(prompt = "Enter the cardinality of B: ")
        cardinalityB <- as.integer(tempCardinalityB)
        
        cat('Probability of A given B is ')
        probabiltyAgB <- BASICPROBABILITY(intersection,sampleSpace)/BASICPROBABILITY(cardinalityB,sampleSpace)
        finalAnswerAgB <- formatC(probabiltyAgB,digits = 6,format = "f")
        cat(finalAnswerAgB)
        cat('\n')
        
        cat('Probability of B given A is ')
        probabiltyBgA <- BASICPROBABILITY(intersection,sampleSpace)/BASICPROBABILITY(cardinalityA,sampleSpace)
        finalAnswerBgA <- formatC(probabiltyBgA,digits = 6,format = "f")
        cat(finalAnswerBgA)
        cat('\n')
      }
      
      if(subChoice3 == 5){
        cat('\n\t\tBAYES THEOREM\n\n')
        
        cat("Enter Ai: ")
        USERINPUT()
        Ai <- as.numeric(x)
        
        cat("Enter BAi: ")
        USERINPUT()
        BAi <- as.numeric(x)
        
        tempI <- readline(prompt = "Enter i: ")
        i <- as.numeric(tempI)
        
        cat("Probability is ")
        finalAnswer <- BAYESTHEOREM(Ai,BAi,i)
        final <- formatC(finalAnswer,digits = 6,format = "f")
        cat(final)
        cat('\n')
      }
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}

DISCRETEDISTRIBUTION <- function(){
  
  choice <- 'y'
  
  while(choice == 'y'){
    cat('\f')
    cat('\t\t***** DISCRETE DISTRIBUTION FUNCTION ******\n\n')
    
    cat('1. UNIFORM DISTRIBUTION\n')
    cat('2. BERNOULLI DISTRIBUTION\n')
    cat('3. BINOMIAL DISTRIBUTION\n')
    cat('4. GEOMETRIC DISTRIBUTION\n')
    cat('5. HYPER-GEOMETRIC DISTRIBUTION\n')
    cat('6. NEGATIVE BINOMIAL DISTRIBUTION\n')
    cat('7. POISSON DISTRIBUTION\n')
    cat('8. MULTINOMIAL DISTRIBUTION\n')
    cat('9. MULTIVARIATE HYPERGEOMETRIC DISTRIBUTION\n')
    
    tempSubChoice4 <- readline(prompt = "Enter the choice: ")
    subChoice4 <- as.integer(tempSubChoice4)
    
    if(subChoice4 > 0 && subChoice4 < 10){
      
      if(subChoice4 == 1){
        cat('\n\t\tUNIFORM DISTRIBUTION\n\n')
        tempTotal <- readline(prompt = "Enter the cardinality of total: ")
        total <- as.integer(tempTotal)
        cat('Probability is ')
        probability <- UNIFORMDISTRIBUTION(total)
        finalAnswer <- formatC(probability,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 2){
        cat('\n\t\tBERNOULLI DISTRIBUTION\n\n')
        tempX <- readline(prompt = "Enter value of x (0 or 1): ")
        x <- as.integer(tempX)
        tempProbability <- readline(prompt = "Enter value of probability (Between 0 and 1): ")
        probability <- as.numeric(tempProbability)
        cat('Probability is ')
        answer <- BERNOULLI(x,probability)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 3){
        cat('\n\t\tBINOMIAL DISTRIBUTION\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.integer(tempN)
        tempX <- readline(prompt = "Enter value of x: ")
        x <- as.integer(tempX)
        tempProbability <- readline(prompt = "Enter value of probability (Between 0 and 1): ")
        probability <- as.numeric(tempProbability)
        cat('Probability is ')
        answer <- BINOMIALDISTRIBUTION(n,x,probability)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 4){
        cat('\n\t\tGEOMETRIC DISTRIBUTION\n\n')
        tempK <- readline(prompt = "Enter value of k: ")
        k <- as.integer(tempK)
        tempProbability <- readline(prompt = "Enter value of probability (Between 0 and 1): ")
        probability <- as.numeric(tempProbability)
        cat('Probability is ')
        answer <- GEOMETRIC(k,probability)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 5){
        cat('\n\t\tHYPER-GEOMETRIC DISTRIBUTION\n\n')
        tempN <- readline(prompt = "Enter value of N: ")
        N <- as.integer(tempN)
        tempK <- readline(prompt = "Enter value of K: ")
        K <- as.integer(tempK)
        tempn <- readline(prompt = "Enter value of n: ")
        n <- as.integer(tempn)
        tempk <- readline(prompt = "Enter value of k: ")
        k <- as.integer(tempk)
        cat('Probability is ')
        answer <- HYPERGEOMETRIC(N,K,n,k)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 6){
        cat('\n\t\tNEGATIVE BINOMIAL DISTRIBUTION\n\n')
        tempK <- readline(prompt = "Enter value of k (Number of successes): ")
        k <- as.integer(tempK)
        tempR <- readline(prompt = "Enter value of r (Number of failures): ")
        r <- as.integer(tempR)
        tempProbability <- readline(prompt = "Enter value of probability (Between 0 and 1): ")
        probability <- as.numeric(tempProbability)
        cat('Probability is ')
        answer <- NEGATIVEBIN(k,r,probability)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 7){
        cat('\n\t\tPOISSON DISTRIBUTION\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.integer(tempN)
        tempProbability <- readline(prompt = "Enter value of probability (Between 0 and 1): ")
        probability <- as.numeric(tempProbability)
        lambda <- n*probability
        cat('Lambda is ',lambda)
        tempK <- readline(prompt = "Enter value of k: ")
        k <- as.integer(tempK)
        cat('Probability is ')
        answer <- POISSON(lambda,k)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 8){
        cat('\n\t\tMULTINOMIAL DISTRIBUTION\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.integer(tempN)
        tempX1 <- readline(prompt = "Enter value of x1: ")
        x1 <- as.integer(tempX1)
        tempX2 <- readline(prompt = "Enter value of x2: ")
        x2 <- as.integer(tempX2)
        tempX3 <- readline(prompt = "Enter value of x3: ")
        x3 <- as.integer(tempX3)
        
        tempP1 <- readline(prompt = "Enter value of probability p1 (Between 0 and 1): ")
        p1 <- as.numeric(tempP1)
        tempP2 <- readline(prompt = "Enter value of probability p2 (Between 0 and 1): ")
        p2 <- as.numeric(tempP2)
        tempP3 <- readline(prompt = "Enter value of probability p3 (Between 0 and 1): ")
        p3 <- as.numeric(tempP3)
        
        cat('Probability is ')
        answer <- MULTINOMIAL(x1,x2,x3,n,p1,p2,p3)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 9){
        cat('\n\t\tMULTIPLE HYPER-GEOMETRIC DISTRIBUTION\n\n')
        tempX1 <- readline(prompt = "Enter value of x1: ")
        x1 <- as.integer(tempX1)
        tempX2 <- readline(prompt = "Enter value of x2: ")
        x2 <- as.integer(tempX2)
        tempX3 <- readline(prompt = "Enter value of x3: ")
        x3 <- as.integer(tempX3)
        tempX4 <- readline(prompt = "Enter value of x4: ")
        x4 <- as.integer(tempX4)
        
        tempM1 <- readline(prompt = "Enter value of M1: ")
        M1 <- as.integer(tempM1)
        tempM2 <- readline(prompt = "Enter value of M2: ")
        M2 <- as.integer(tempM2)
        tempM3 <- readline(prompt = "Enter value of M3: ")
        M3 <- as.integer(tempM3)
        tempM4 <- readline(prompt = "Enter value of M4: ")
        M4 <- as.integer(tempM4)
        
        cat('Probability is ')
        answer <- MULTIHYPGEO(x1,x2,x3,x4,M1,M2,M3,M4)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }

}

CONTINOUSDISTRIBUTION <- function(){
  
  choice <- 'y'
  while(choice == 'y'){
    cat('\f')
    cat('\t\t***** CONTINOUS DISTRIBUTION FUNCTION ******\n\n')
    cat('1. UNIFORM DISTRIBUTION\n')
    cat('2. NORMAL DISTRIBUTION\n')
    cat('3. BIVARIATE NORMAL DISTRIBUTION\n')
    cat('4. GAMMA DISTRIBUTION\n')
    cat('5. EXPONENTIAL DISTRIBUTION\n')
    
    tempSubChoice5 <- readline(prompt = "Enter the choice: ")
    subChoice5 <- as.integer(tempSubChoice5)
    
    if(subChoice5 > 0 && subChoice5 < 6){
      
      if(subChoice5 == 1){
        cat('\n\t\tUNIFORM CONTINOUS DISTRIBUTION\n\n')
        tempLowX <- readline(prompt = "Enter value of lower limit: ")
        lowX <- as.integer(tempLowX)
        
        tempHighX <- readline(prompt = "Enter value of upper limit: ")
        highX <- as.integer(tempHighX)
        
        tempA <- readline(prompt = "Enter a: ")
        a <- as.integer(tempA)
        
        tempB <- readline(prompt = "Enter b: ")
        b <- as.integer(tempB)
        
        cat('Probability is ')
        answer <- UNICONTINOUS(lowX,highX,a,b)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice5 == 2){
        cat('\n\t\tNORMAL DISTRIBUTION\n\n')
        tempLowX <- readline(prompt = "Enter value of lower limit: ")
        lowX <- as.numeric(tempLowX)
        
        tempHighX <- readline(prompt = "Enter value of upper limit: ")
        highX <- as.numeric(tempHighX)
        
        tempPopMean <- readline(prompt = "Enter value of population mean: ")
        popMean <- as.numeric(tempPopMean)
        
        tempPopVariance <- readline(prompt = "Enter value of population variance: ")
        popVariance <- as.numeric(tempPopVariance)
        
        cat('Probability is ')
        answer <- NORMALDIST(lowX,highX,popMean,popVariance)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice5 == 3){
        #underProcessing
      }
      
      if(subChoice5 == 4){
        cat('\n\t\tGAMMA DISTRIBUTION\n\n')
        tempLowX <- readline(prompt = "Enter value of lower limit: ")
        lowX <- as.numeric(tempLowX)
        
        tempHighX <- readline(prompt = "Enter value of upper limit: ")
        highX <- as.numeric(tempHighX)
      
        tempAlpha <- readline(prompt = "Enter alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        tempBeta <- readline(prompt = "Enter beta: ")
        beta <- as.numeric(tempBeta)
        
        cat('Probability is ')
        answer <- GAMMADIST(lowX,highX,alpha,beta)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice5 == 5){
        cat('\n\t\tEXPONENTIAL DISTRIBUTION\n\n')
        tempLowX <- readline(prompt = "Enter value of lower limit: ")
        lowX <- as.numeric(tempLowX)
        
        tempHighX <- readline(prompt = "Enter value of upper limit: ")
        highX <- as.numeric(tempHighX)
        
        tempLambda <- readline(prompt = "Enter lambda: ")
        lambda <- as.numeric(tempLambda)
        
        cat('Probability is ')
        answer <- GAMMADIST(lowX,highX,1,1/lambda)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}

SAMPLEDISTTEST <- function(){
  
  choice <- 'y'
  
  while(choice == 'y') {
    cat('\f')
    cat('\t\t***** SAMPLE DISTRIBUTION TEST STATISTIC ******\n\n')
    cat('1. CHI-SQUARE TEST\n')
    cat('2. STUDENT t-TEST\n')
    cat('3. F-TEST\n')
    cat('4. Z-TEST\n')
    
    tempSubChoice6 <- readline(prompt = "Enter the choice: ")
    subChoice6 <- as.integer(tempSubChoice6)
    
    if(subChoice6 > 0 && subChoice6 < 5){
      if(subChoice6 == 1){
        cat('\n\t\tCHI-SQUARE TEST\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempSamVariance <- readline(prompt = "Enter value of sample variance: ")
        samVariance <- as.numeric(tempSamVariance)
        
        tempPopVariance <- readline(prompt = "Enter value of population variance: ")
        popVariance <- as.numeric(tempPopVariance)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        calcChiSq <- CHISQDIST(n,samVariance,popVariance)
        obsChiSq <- qchisq((1-alpha),(n-1))
        
        if(calcChiSq >= obsChiSq){
          cat("Calculated value is",calcChiSq, "and observed is", obsChiSq,'\n')
          cat("Because Calculated is greater than observed, therefore REJECT")
        }
        
        if(calcChiSq < obsChiSq){
          cat("Calculated value is",calcChiSq, "and observed is", obsChiSq,'\n')
          cat("Because Calculated is less than observed, therefore ACCEPT")
        }
        
      }
      
      if(subChoice6 == 2){
        cat('\n\t\tSTUDENT t-TEST\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempSamMean <- readline(prompt = "Enter value of sample mean: ")
        samMean <- as.numeric(tempSamMean)
        
        tempPopMean <- readline(prompt = "Enter value of population mean: ")
        popMean <- as.numeric(tempPopMean)
        
        tempSamSD <- readline(prompt = "Enter value of sample standard deviation: ")
        samSD <- as.numeric(tempSamSD)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        calcT <- TDIST(n,samMean,popMean,samSD)
        if(calcT < 0){
          calcT <- -calcT
        }
        
        cat("\nTAIL-TEST\n1. ONE-TAILED\n2. TWO-TAILED\n")
        tailInput <- readline(prompt = "Enter the input: ")
        
        if(tailInput == 1){
          obsT <- qt((1-alpha),(n-1))
        }
        
        if(tailInput == 2){
          obsT <- qt((1-(alpha/2)),(n-1))
        }
        
        if(calcT >= obsT){
          cat("Calculated value is",calcT, "and observed is", obsT,'\n')
          cat("Because Calculated is greater than observed, therefore REJECT")
        }
        
        if(calcT < obsT){
          cat("Calculated value is",calcT, "and observed is", obsT,'\n')
          cat("Because Calculated is less than observed, therefore ACCEPT")
        }
        
      }
      
      if(subChoice6 == 3){
        cat('\n\t\tF TEST\n\n')
        tempN1 <- readline(prompt = "Enter value of n1: ")
        n1 <- as.numeric(tempN1)
        
        tempN2 <- readline(prompt = "Enter value of n2: ")
        n2 <- as.numeric(tempN2)
        
        tempSamVariance1 <- readline(prompt = "Enter value of sample variance1: ")
        samVariance1 <- as.numeric(tempSamVariance1)
        
        tempPopVariance1 <- readline(prompt = "Enter value of population variance1: ")
        popVariance1 <- as.numeric(tempPopVariance1)
        
        tempSamVariance2 <- readline(prompt = "Enter value of sample variance2: ")
        samVariance2 <- as.numeric(tempSamVariance2)
        
        tempPopVariance2 <- readline(prompt = "Enter value of population variance2: ")
        popVariance2 <- as.numeric(tempPopVariance2)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        calcF <- FDIST(samVariance1,popVariance1,samVariance2,popVariance2)
        
        cat("\nTAIL-TEST\n1. ONE-TAILED\n2. TWO-TAILED\n")
        tailInput <- readline(prompt = "Enter the input: ")
        
        if(tailInput == 1){
          obsF <- qf((1-alpha),(n1-1),(n2-1))
        }
        
        if(tailInput == 2){
          obsF <- qf((1-(alpha/2)),(n1-1),(n2-1))
        }
        
        if(calcF >= obsF){
          cat("Calculated value is",calcF, "and observed is", obsF,'\n')
          cat("Because Calculated is greater than observed, therefore REJECT")
        }
        
        if(calcF < obsF){
          cat("Calculated value is",calcF, "and observed is", obsF,'\n')
          cat("Because Calculated is less than observed, therefore ACCEPT")
        }
        
      }
      
      if(subChoice6 == 4){
        cat('\n\t\tZ-TEST\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempSamMean <- readline(prompt = "Enter value of sample mean: ")
        samMean <- as.numeric(tempSamMean)
        
        tempPopMean <- readline(prompt = "Enter value of population mean: ")
        popMean <- as.numeric(tempPopMean)
        
        tempPopSD <- readline(prompt = "Enter value of population standard deviation: ")
        PopSD <- as.numeric(tempPopSD)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        calcZ <- ZDIST(samMean,popMean,PopSD,n)
        if(calcZ < 0){
          calcZ <- -calcZ
        }
        
        cat("\nTAIL-TEST\n1. ONE-TAILED\n2. TWO-TAILED\n")
        tailInput <- readline(prompt = "Enter the input: ")
        
        if(tailInput == 1){
          obsZ <- qnorm((1-alpha))
        }
        
        if(tailInput == 2){
          obsZ <- qnorm((1-(alpha/2)))
        }
        
        if(calcZ >= obsZ){
          cat("Calculated value is",calcZ, "and observed is", obsZ,'\n')
          cat("Because Calculated is greater than observed, therefore REJECT")
          pValue <- 1 - pnorm(calcZ)
          cat("\nP-value is",pValue)
        }
        
        if(calcZ < obsZ){
          cat("Calculated value is",calcZ, "and observed is", obsZ,'\n')
          cat("Because Calculated is less than observed, therefore ACCEPT")
          pValue <- 1 - pnorm(calcZ)
          cat("\nP-value is",pValue)
        }
        
      }
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}

INTERVALESTIMATION <- function(){
  choice <- 'y'
  
  while(choice == 'y') {
    cat('\f')
    cat('\t\t***** INTERVAL ESTIMATIONS ******\n\n')
    cat('1. ESTIMATION OF MEANS\n')
    cat('2. ESTIMATION OF DIFFERENCE IN MEANS\n')
    cat('3. ESTIMATION OF PROPORTIONS\n')
    cat('4. ESTIMATION OF DIFFERENCE IN PROPORTIONS\n')
    cat('5. ESTIMATION OF VARIANCES\n')
    cat('6. ESTIMATION OF RATIO OF TWO VARIANCES\n')
    
    tempSubChoice7 <- readline(prompt = "Enter the choice: ")
    subChoice7 <- as.integer(tempSubChoice7)
    
    if(subChoice7 > 0 && subChoice7 < 7){
      if(subChoice7 == 1){
        cat('\n\t\tESTIMATION OF MEANS\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempSamMean <- readline(prompt = "Enter value of sample mean: ")
        samMean <- as.numeric(tempSamMean)
        
        if(n >= 30){
          tempPopSD <- readline(prompt = "Enter value of population standard deviation: ")
          popSD <- as.numeric(tempPopSD)
        }
        
        if(n < 30){
          tempSamSD <- readline(prompt = "Enter value of sample standard deviation: ")
          samSD <- as.numeric(tempSamSD)
        }
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        if(n >=30 ){
          zValue <- qnorm(1-(alpha/2))
        
          lowLimit <- (samMean - (zValue*((popSD*popSD/n)^(0.5))))
          upperLimit <- (samMean + (zValue*((popSD*popSD/n)^(0.5))))
        
          cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit)
        }
        
        if(n < 30){
          tValue <- qt((1-(alpha/2)),(n-1))
          
          lowLimit <- (samMean - (tValue*((samSD*samSD/n)^(0.5))))
          upperLimit <- (samMean + (tValue*((samSD*samSD/n)^(0.5))))
          
          cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit)
        }
      }  
      
      if(subChoice7 == 2){
        cat('\n\t\tESTIMATION OF DIFFERENCE MEANS\n\n')
        tempN1 <- readline(prompt = "Enter value of n1: ")
        n1 <- as.numeric(tempN1)
        
        tempN2 <- readline(prompt = "Enter value of n2: ")
        n2 <- as.numeric(tempN2)
        
        tempSamMean1 <- readline(prompt = "Enter value of sample mean1: ")
        samMean1 <- as.numeric(tempSamMean1)
        
        tempSamMean2 <- readline(prompt = "Enter value of sample mean2: ")
        samMean2 <- as.numeric(tempSamMean2)
        
        if(n1 >= 30){
          tempPopVar1 <- readline(prompt = "Enter value of population variance1: ")
          popVar1 <- as.numeric(tempPopVar1)
        }
        
        if(n2 >= 30){
          tempPopVar2 <- readline(prompt = "Enter value of population variance2: ")
          popVar2 <- as.numeric(tempPopVar2)
        }
        
        if(n1 < 30 && n2 < 30){
          tempSamVar1 <- readline(prompt = "Enter value of sample variance1: ")
          samVar1 <- as.numeric(tempSamVar1)
          
          tempSamVar2 <- readline(prompt = "Enter value of sample variance2: ")
          samVar2 <- as.numeric(tempSamVar2)
          
          poolSamVar <- (((n1-1)*samVar1)+((n2-1)*samVar2))/(n1+n2-2)
          
        }
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        if(n1 >= 30 && n2 >= 30){
          zValue <- qnorm(1-(alpha/2))
          
          meanDifference <- samMean1 - samMean2
          varianceFactor <- (popVar1/n1)+(popVar2/n2)
          lowLimit <- (meanDifference - (zValue*((varianceFactor)^(0.5))))
          upperLimit <- (meanDifference + (zValue*((varianceFactor)^(0.5))))
          
          cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit)
        }
        
        if(n1 < 30 && n2 < 30){
          tValue <- qt((1-(alpha/2)),(n1+n2-2))
          
          meanDifference <- samMean1 - samMean2
          varianceFactor <- (poolSamVar/n1)+(poolSamVar/n2)
          lowLimit <- (meanDifference - (tValue*(varianceFactor^(0.5))))
          upperLimit <- (meanDifference + (tValue*(varianceFactor^(0.5))))
          
          cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit)
        }
      }  
      
      if(subChoice7 == 3){
        cat('\n\t\tESTIMATION OF PROPORTIONS\n\n')
        tempX <- readline(prompt = "Enter value of x: ")
        x <- as.numeric(tempX)
        
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        zValue <- qnorm(1-(alpha/2))
        p <- x/n
        
        lowLimit <- (p - zValue*(((p*(1-p))/n)^(0.5)))
        upperLimit <- (p + zValue*(((p*(1-p))/n)^(0.5)))
        cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit) 
        
      }  
      
      if(subChoice7 == 4){
        cat('\n\t\tESTIMATION IN DIFFERENCE OF PROPORTIONS\n\n')
        tempX1 <- readline(prompt = "Enter value of x1: ")
        x1 <- as.numeric(tempX1)
        
        tempN1 <- readline(prompt = "Enter value of n1: ")
        n1 <- as.numeric(tempN1)
        
        tempX2 <- readline(prompt = "Enter value of x2: ")
        x2 <- as.numeric(tempX2)
        
        tempN2 <- readline(prompt = "Enter value of n2: ")
        n2 <- as.numeric(tempN2)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        zValue <- qnorm(1-(alpha/2))
        p1 <- x1/n1
        p2 <- x2/n2
        
        pDifference <- p1-p2
        varianceFactor <- ((p1*(1-p1))/n1)+((p2*(1-p2))/n2)
        
        lowLimit <- (pDifference - (zValue*(((varianceFactor)^(0.5)))))
        upperLimit <- (pDifference + (zValue*(((varianceFactor)^(0.5)))))
        cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit) 
      }  
      
      if(subChoice7 == 5){
        cat('\n\t\tESTIMATION OF VARIANCES\n\n')
        
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempSamVar <- readline(prompt = "Enter value of sample variance: ")
        samVar <- as.numeric(tempSamVar)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        lowDen <- qchisq((1-(alpha/2)),(n-1))
        upperDen <- qchisq((alpha/2),(n-1))
        
        lowLimit <- ((n-1)*samVar)/lowDen
        upperLimit <- ((n-1)*samVar)/upperDen
        cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit) 
      }  
      
      if(subChoice7 == 6){
        cat('\n\t\tESTIMATION OF RATIO OF VARIANCES\n\n')
        
        tempN1 <- readline(prompt = "Enter value of n1: ")
        n1 <- as.numeric(tempN1)
        
        tempSamVar1 <- readline(prompt = "Enter value of sample variance1: ")
        samVar1 <- as.numeric(tempSamVar1)
        
        tempN2 <- readline(prompt = "Enter value of n2: ")
        n2 <- as.numeric(tempN2)
        
        tempSamVar2 <- readline(prompt = "Enter value of sample variance2: ")
        samVar2 <- as.numeric(tempSamVar2)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        samVarFactor <- samVar1/samVar2
        
        lowDenFac <- qf((1-(alpha/2)),(n1-1),(n2-1))
        upperFac <- qf((1-(alpha/2)),(n2-1),(n1-1))
        
        lowLimit <- samVarFactor/lowDenFac
        upperLimit <- samVarFactor*upperFac
        cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit)
      }  
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}

NONPARAMANALYSIS <- function() {
  
  choice <- 'y'
  
  while(choice == 'y'){
    cat('\f')
    cat('\t\t***** NON-PARAMETRIC ANALYSIS ******\n\n')
    cat('1. SIGN TEST\n')
    cat('2. WILCOXON SIGNED-RANK TEST\n')
    cat('3. MANN-WHITNEY TEST\n')
    cat('4. KRUSKAL-WALLIS TEST\n')
    
    
    tempSubChoice8 <- readline(prompt = "Enter the choice: ")
    subChoice8 <- as.integer(tempSubChoice8)
    
    if(subChoice8 > 0 && subChoice8 < 5){
      if(subChoice8 == 1){
        cat('\n\t\tSIGN TEST\n\n')
        USERINPUT()
        x <<- as.numeric(y)
        
        tempPopMean <- readline(prompt = "Enter value of population mean: ")
        popMean <- as.numeric(tempPopMean)
        
        retVector <- SIGNTEST(x,popMean)
        countPlus <- retVector[1]
        finalN <- retVector[2]
        
        samMean <- finalN*0.5
        samSD <- (finalN*0.5*0.5)^(0.5)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        calValue <- qnorm(1-alpha,samMean,samSD)
        
        if(calValue < 0){
          calValue <- -calValue
        }
        
        cat("\nTAIL-TEST\n1. ONE-TAILED\n2. TWO-TAILED\n")
        tailInput <- readline(prompt = "Enter the input: ")
        
        if(tailInput == 1){
          obsZ <- qnorm((1-alpha))
        }
        
        if(tailInput == 2){
          obsZ <- qnorm((1-(alpha/2)))
        }
        
        if(calValue >= obsZ){
          cat("Calculated value is",calValue, "and observed is", obsZ,'\n')
          cat("Because Calculated is greater than observed, therefore REJECT")
          pValue <- 1 - pnorm(calValue)
          cat("\nP-value is",pValue)
        }
        
        if(calValue < obsZ){
          cat("Calculated value is",calValue, "and observed is", obsZ,'\n')
          cat("Because Calculated is less than observed, therefore ACCEPT")
          pValue <- 1 - pnorm(calValue)
          cat("\nP-value is",pValue)
        }
      } 
      
      if(subChoice8 == 2){
        
      } 
      
      if(subChoice8 == 3){
        
      } 
      
      if(subChoice8 == 4){
        
      } 
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}

VISUALIZATIONS <- function() {
  choice <- 'y'
  
  while(choice == 'y') {
    cat('\f')
    cat('\t\t***** VISUALIZATIONS ******\n\n')
    cat('1. HISTOGRAM\n')
    cat('2. LINE GRAPH\n')
    cat('3. BAR GRAPH\n')
    cat('4. PIE CHART\n')
    cat('5. SCATTER PLOT\n')
    cat('6. BOX PLOT\n')
    cat('7. Q-Q PLOT\n')
    cat('8. STEM LEAF PLOT\n')
    cat('9. PARETO CHART\n')
    
    tempSubChoice9 <- readline(prompt = "Enter the choice: ")
    subChoice9 <- as.integer(tempSubChoice9)
    
    if(subChoice9 > 0 && subChoice9 < 10){
      if(subChoice9 == 1){
        cat('\n\t\tHISTOGRAM\n\n')
        INPUT()
        x1 <- sort(x1)
        HISTOGRAM(x1)
      }  
      
      if(subChoice9 == 2){
        cat('\n\t\tLINE GRAPH\n\n')
        INPUT()
        LINEGRAPH(x1)
      }  
      
      if(subChoice9 == 3){
        cat('\n\t\tBAR GRAPH\n\n')
        INPUT()
        BARGRAPH(x1)
      }  
      
      if(subChoice9 == 4){
        cat('\n\t\tPIE CHART\n\n')
        INPUT()
        PIECHART(x1)
      }  
      
      if(subChoice9 == 5){
        cat('\n\t\tSCATTER PLOT\n\n')
        cat("Choose a CSV File from Local Storage\n")
        READFROMCSV()
        
        SCATTERPLOT(x1,y)
      }  
      
      if(subChoice9 == 6){
        cat('\n\t\tBOX PLOT\n\n')
        INPUT()
        BOXPLOT(x1)
      }  
      
      if(subChoice9 == 7){
        cat('\n\t\tQQ PLOT\n\n')
        cat("Choose a CSV File from Local Storage\n")
        READFROMCSV()
        
        QQPLOT(x1,y)
      }  
      
      if(subChoice9 == 8){
        cat('\n\t\tSTEM LEAF PLOT\n\n')
        INPUT()
        STEMLEAFPLOT(x1)
      }  
      
      if(subChoice9 == 9){
        cat('\n\t\tPARETO CHART\n\n')
        INPUT()
        PARETOCHART(x1)
      }  
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}


#MAIN_MENU
MAIN <- function(){
  choice <- 'y'
  while(choice == 'y'){
    cat('\f')
    cat('\t\t***** STATISTICS CALCULATOR ******\n\n')
    cat('1. DESCRIPTIVE ANALYSIS\n')
    cat('2. PREDICTIVE ANALYSIS\n')
    cat('3. PROBABILITY ANALYSIS\n')
    cat('4. DISCRETE DISTRIBUTION FUNCTION\n')
    cat('5. CONTINOUS DISTRIBUTION FUNCTION\n')
    cat('6. SAMPLE DISTRIBUTION TEST STATISTIC\n')
    cat('7. INTERVAL ESTIMATION\n')
    cat('8. NON-PARAMETRIC ANALYSIS\n')
    cat('9. VISUALIZATIONS\n')
    
    tempMainMenuChoice <- readline(prompt = "Enter the choice: ")
    mainMenuChoice <- as.integer(tempMainMenuChoice)
    
    if(mainMenuChoice > 0 && mainMenuChoice < 10){
      
      if(mainMenuChoice == 1){
        DESCRIPTIVEANALYSIS()
      }
      
      if(mainMenuChoice == 2){
        PREDANALYSIS()
      }
      
      if(mainMenuChoice == 3){
        PROBABILITYANALYSIS()
      }
      
      if(mainMenuChoice == 4){
        DISCRETEDISTRIBUTION()
      }
      
      if(mainMenuChoice == 5){
        CONTINOUSDISTRIBUTION()
      }
      
      if(mainMenuChoice == 6){
        SAMPLEDISTTEST()
      }
      
      if(mainMenuChoice == 7){
        INTERVALESTIMATION()
      }
      
      if(mainMenuChoice == 8){
        NONPARAMANALYSIS()
      }
      
      if(mainMenuChoice == 9){
        VISUALIZATIONS()
      }
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
      
    choice <- readline(prompt = "Do you want to go to main menu? ")
  }
}

cat('\f')
MAIN()