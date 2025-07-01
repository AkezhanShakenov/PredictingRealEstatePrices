myData <- read.delim("APPENC07.txt", header = FALSE)


# Create an 8 by 8 matrix with numbers from 1 to 64
resultM <- matrix(1:64, nrow = 8, ncol = 8)



print(resultM)



emptyDeleter <- function (a){
  i = 1
  while (i <= length(a)){
    if (a[i] == ""){
      a <- a[-i]
      i <- i-1
    }
    i <- i + 1
  }
  return(a)
}
temp <- list()
for (i in myData){
  print(i)
  temp <- append(temp, i)
}
print(temp)
clearData <- list()
for(i in temp){
  i <- strsplit(i, " ")[[1]]
  i <- emptyDeleter(i)
  i <- as.numeric(i)
  print(i)
  
  clearData <-append(clearData,i)
}

print((clearData))


indices_to_delete <- c(5, 7, 10)

# Generate the sequence of indices for every 13th step
steps <- seq(1, length(clearData), by = 13)

# Calculate the actual indices to delete
delete_indices <- unlist(lapply(steps, function(x) x + indices_to_delete - 1))

# Remove indices that exceed the length of the list
delete_indices <- delete_indices[delete_indices <= length(clearData)]

# Delete the specified elements from the list
clearData <- clearData[-delete_indices]

# Print the modified list (optional)
print(length(clearData))


allYs <- list()
i <- 2
while (i <= length(clearData)){
  allYs <- append(allYs, clearData[i])
  i <- i + 10
}

allXs <- list()
i <- 3
while (i <= length(clearData)){
  j <- 0
  while (j <= 7){
    allXs <- append(allXs, clearData[i+j])
    j<- j+1
  }
  
  
  i <- i + 10
}

# Convert the list to a vector
allXs_vector <- unlist(allXs)

# Convert the vector to a 522 by 8 matrix
allXs_matrix <- matrix(allXs_vector, nrow = 522, ncol = 8, byrow = TRUE)

# Print the matrix (optional)
print(allXs_matrix)

# Convert the list to a vector
allYs_vector <- unlist(allYs)

# Convert the vector to a 522 by 8 matrix
allYs_matrix <- matrix(allYs_vector, nrow = 522, ncol = 1, byrow = TRUE)

# Print the matrix (optional)
print(allYs_matrix)



new_column <- rep(1, 522)

# Combine the original matrix with the new column
allXs_matrix_extended <- cbind(new_column,allXs_matrix)

# Print the new matrix (optional)
print(allXs_matrix_extended)





# ASSESSMENT OF 8 PREDICTOR MODEL -- beginning
print(allXs_matrix)
s <- t(allXs_matrix_extended) %*% allXs_matrix_extended 
b8 <- (solve(s) %*% t(allXs_matrix_extended))  %*% allYs_matrix
print(b8)
SSE8 <- t(allYs_matrix - allXs_matrix_extended %*% b8)%*%(allYs_matrix - allXs_matrix_extended %*% b8)
J <-  matrix(1/522, nrow = 522, ncol = 522)
rows <- 522
cols <- 522

# Create a matrix of zeros
matrix_with_identity <- matrix(0, nrow = rows, ncol = cols)

# Define the size of the identity matrix
identity_size <- min(rows, cols)

# Create the identity matrix of size identity_size
identity_matrix <- diag(1, nrow = identity_size)

# Place the identity matrix in the top-left corner of the matrix
matrix_with_identity[1:identity_size, 1:identity_size] <- identity_matrix
Rsq <- 1 - (SSE8)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
RsqL <- 1 - (SSE8/522)/(((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)/521)
Cp <- SSE8 / (SSE8/522) - (522-18)
AICp <- 522*log(SSE8) - 522 * log(522) + 2*9
SBCp <- 522 * log(SSE8) - 522*log(522) + log(522)*9
i <- 1
PressP <- 0

while(i <= length(allYs_matrix)){
  PressP <- PressP + (allYs_matrix[i] - (allXs_matrix_extended%*%  b8)[i]) *(allYs_matrix[i] - (allXs_matrix_extended%*%  b8)[i])
  print((allXs_matrix_extended%*%  b8)[i] *(allYs_matrix[i] - (allXs_matrix_extended%*%  b8)[i]))
  i <- i + 1
}

resultM[8,1]<- "8"
resultM[8,2]<- SSE8
resultM[8,3]<- Rsq
resultM[8,4]<- RsqL
resultM[8,5]<- Cp
resultM[8,6]<- AICp
resultM[8,7]<- SBCp
resultM[8,8]<- PressP
print(resultM)

# ASSESSMENT OF 8 PREDICTOR MODEL -- ending

# ASSESSMENT OF 7 PREDICTOR MODEL -- beginning
# variables -- allYsmatrix, allXs_matrix_extended, b7, set of Rsq values
n <- 2
Rsq7 <- -1
mainN <- 0
while (n <= 9){
  tempMat <- allXs_matrix_extended[, -n]
  s <- t(tempMat) %*% tempMat 
  print(tempMat)
  b7 <- (solve(s) %*% t(tempMat))  %*% allYs_matrix
  SSE7 <- t(allYs_matrix - tempMat %*% b7)%*%(allYs_matrix - tempMat %*% b7)
  
  
  if( Rsq7 < 1 - (SSE7)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)){
    Rsq7 <- 1 - (SSE7)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
    mainN <- n
  }
  n <- n + 1 
}

print(mainN)
SevenPredModel <- allXs_matrix_extended[,-mainN]

s <- t(SevenPredModel) %*% SevenPredModel 
b7 <- solve(s) %*% (t(SevenPredModel)  %*% allYs_matrix)
print(b7)
SSE7 <- t(allYs_matrix - SevenPredModel %*% b7)%*%(allYs_matrix - SevenPredModel %*% b7)
print(allYs_matrix- SevenPredModel %*% b7)
SSE71 <- allYs_matrix - (SevenPredModel %*% b7)
sum <- 0
for( i in SSE71){
  sum <- sum + i * i
}
print(allXs_matrix_extended)
print(b7)
print(sum)
print(SSE7)
print(SSE71)
print(SevenPredModel[1,] %*% b7)


Rsq7 <- 1 - (SSE7)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
RsqL7 <- 1 - (SSE7/522)/(((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)/521)
Cp7 <- SSE8 / (SSE7/522) - (522-16)
AICp7 <- 522*log(SSE7) - 522 * log(522) + 2*8
SBCp7 <- 522 * log(SSE7) - 522*log(522) + log(522)*8
i <- 1
PressP7 <- 0

while(i <= length(allYs_matrix)){
  PressP7 <- PressP7 + (allYs_matrix[i] - (SevenPredModel%*%  b7)[i]) *(allYs_matrix[i] - (SevenPredModel%*%  b7)[i])
  
  i <- i + 1
}

resultM[7,1]<- "7"
resultM[7,2]<- SSE7
resultM[7,3]<- Rsq7
resultM[7,4]<- RsqL7
resultM[7,5]<- Cp7
resultM[7,6]<- AICp7
resultM[7,7]<- SBCp7
resultM[7,8]<- PressP7

print(resultM)
# ASSESSMENT OF 7 PREDICTOR MODEL -- ending


# ASSESSMENT OF 6 PREDICTOR MODEL -- beginning
# variables -- allYsmatrix, allXs_matrix_extended, b6, set of Rsq values
n <- 2
m <- 2
Rsq6 <- -1
mainN <- 0
mainM <- 0
while (n <= 9){
  tempMat <- allXs_matrix_extended[, -n]
  m <- n
  while (m <= 8){
  
  tempMat <- tempMat[, -m]
  s <- t(tempMat) %*% tempMat 
  print(tempMat)
  b6 <- (solve(s) %*% t(tempMat))  %*% allYs_matrix
  SSE6 <- t(allYs_matrix - tempMat %*% b6)%*%(allYs_matrix - tempMat %*% b6)
  
  if( Rsq6 < 1 - (SSE6)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)){
    Rsq6 <- 1 - (SSE6)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
    mainN <- n
    mainM <- m+1
     
  }
  tempMat <- allXs_matrix_extended[, -n]
  m <- m + 1
}
  n <- n + 1 
}

print(mainN)
print(mainM)


SixPredModel <- allXs_matrix_extended[,-mainN]
SixPredModel <- SixPredModel[,-(mainM-1)]

s <- t(SixPredModel) %*% SixPredModel 
b6 <- (solve(s) %*% t(SixPredModel))  %*% allYs_matrix
SSE6 <- t(allYs_matrix - SixPredModel %*% b6)%*%(allYs_matrix - SixPredModel %*% b6)
Rsq6 <- 1 - (SSE6)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
RsqL6 <- 1 - (SSE6/522)/(((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)/521)
Cp6 <- SSE6 / (SSE6/522) - (522-14)
AICp6 <- 522*log(SSE6) - 522 * log(522) + 2*7
SBCp6 <- 522 * log(SSE6) - 522*log(522) + log(522)*7
i <- 1
PressP6 <- 0

while(i <= length(allYs_matrix)){
  PressP6 <- PressP6 + (allYs_matrix[i] - (SixPredModel%*%  b6)[i]) *(allYs_matrix[i] - (SixPredModel%*%  b6)[i])

  i <- i + 1
}

resultM[6,1]<- "6"
resultM[6,2]<- SSE6
resultM[6,3]<- Rsq6
resultM[6,4]<- RsqL6
resultM[6,5]<- Cp6
resultM[6,6]<- AICp6
resultM[6,7]<- SBCp6
resultM[6,8]<- PressP6

print(resultM)


# ASSESSMENT OF 6 PREDICTOR MODEL -- ending

# ASSESSMENT OF 5 PREDICTOR MODEL -- beginning
# variables -- allYsmatrix, allXs_matrix_extended, b6, set of Rsq values
n <- 2
m <- 2
l <- 2
Rsq5 <- -1
mainN <- 0
mainM <- 0
mainL <- 0
counter <- 0
while (n <= 9){
  tempMat <- allXs_matrix_extended[, -n]
  m <- n
  while (m <= 8){
    tempMat <- tempMat[, -m]
    l <- m
    while (l <= 7){
      counter <- counter + 1
    tempMat <- tempMat[, -l]
    s <- t(tempMat) %*% tempMat 
    print(tempMat)
    
    b5 <- (solve(s) %*% t(tempMat))  %*% allYs_matrix
    SSE5 <- t(allYs_matrix - tempMat %*% b5)%*%(allYs_matrix - tempMat %*% b5)
    
    if( Rsq5 < 1 - (SSE5)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)){
      Rsq5 <- 1 - (SSE5)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
      mainN <- n
      mainM <- m+1
      mainL <- l+1
      
    }
    tempMat <- allXs_matrix_extended[, -n]
    tempMat <- tempMat[, -m]
    l <- l + 1
    }
    tempMat <- allXs_matrix_extended[, -n]
    m <- m + 1
    
  }
  n <- n + 1 
}
print(counter)
print(mainN)
print(mainM)
print(mainL)



FivePredModel <- allXs_matrix_extended[,-mainN]
FivePredModel <- FivePredModel[,-(mainM-1)]
FivePredModel <- FivePredModel[,-(mainL-2)]

s <- t(FivePredModel) %*% FivePredModel 
b5 <- (solve(s) %*% t(FivePredModel))  %*% allYs_matrix
SSE5 <- t(allYs_matrix - FivePredModel %*% b5)%*%(allYs_matrix - FivePredModel %*% b5)
Rsq5 <- 1 - (SSE5)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
RsqL5 <- 1 - (SSE5/522)/(((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)/521)
Cp5 <- SSE5 / (SSE5/522) - (522-12)
AICp5 <- 522*log(SSE5) - 522 * log(522) + 2*6
SBCp5 <- 522 * log(SSE5) - 522*log(522) + log(522)*6
i <- 1
PressP5 <- 0

while(i <= length(allYs_matrix)){
  PressP5 <- PressP5 + (allYs_matrix[i] - (FivePredModel%*%  b5)[i]) *(allYs_matrix[i] - (FivePredModel%*%  b5)[i])
  
  i <- i + 1
}

resultM[5,1]<- "5"
resultM[5,2]<- SSE5
resultM[5,3]<- Rsq5
resultM[5,4]<- RsqL5
resultM[5,5]<- Cp5
resultM[5,6]<- AICp5
resultM[5,7]<- SBCp5
resultM[5,8]<- PressP5

print(resultM)

# ASSESSMENT OF 5 PREDICTOR MODEL -- ending

# ASSESSMENT OF 4 PREDICTOR MODEL -- beginning
# variables -- allYsmatrix, allXs_matrix_extended, b6, set of Rsq values
n <- 2
m <- 2
l <- 2
a <- 2
Rsq4 <- -1
mainN <- 0
mainM <- 0
mainL <- 0
mainA <- 0
counter <- 0
while (n <= 9){
  tempMat <- allXs_matrix_extended[, -n]
  m <- n
  while (m <= 8){
    tempMat <- tempMat[, -m]
    l <- m
    while (l <= 7){
      tempMat <- tempMat[, -l]
      a <- l
      while (a <= 6){
        counter <- counter + 1
        tempMat <- tempMat[, -a]
        s <- t(tempMat) %*% tempMat 
        print(tempMat)
        b4 <- (solve(s) %*% t(tempMat))  %*% allYs_matrix
        SSE4 <- t(allYs_matrix - tempMat %*% b4)%*%(allYs_matrix - tempMat %*% b4)
        
        if( Rsq4 < 1 - (SSE4)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)){
          Rsq4 <- 1 - (SSE4)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
          mainN <- n
          mainM <- m+1
          mainL <- l+2
          mainA <- a+3
          
        }
        tempMat <- allXs_matrix_extended[, -n]
        tempMat <- tempMat[, -m]
        tempMat <- tempMat[, -l]
        a <- a+1
      }
      tempMat <- allXs_matrix_extended[, -n]
      tempMat <- tempMat[, -m]
      l <- l + 1
    }
    tempMat <- allXs_matrix_extended[, -n]
    m <- m + 1
    
  }
  n <- n + 1 
}
print(counter)
print(mainN)
print(mainM)
print(mainL)
print(mainA)



n <- 2
m <- 2
l <- 2
a <- 2
Rsq4 <- -1
mainN <- 0
mainM <- 0
mainL <- 0
mainA <- 0
counter <- 0
while (n <= 9){
  tempMat <- allXs_matrix_extended[, -n]
  m <- n
  while (m <= 8){
    tempMat <- tempMat[, -m]
    l <- m
    while (l <= 7){
      tempMat <- tempMat[, -l]
      a <- l
      while (a <= 6){
        counter <- counter + 1
        tempMat <- tempMat[, -a]
        s <- t(tempMat) %*% tempMat 
        print(tempMat)
        b4 <- (solve(s) %*% t(tempMat))  %*% allYs_matrix
        SSE4 <- t(allYs_matrix - tempMat %*% b4)%*%(allYs_matrix - tempMat %*% b4)
        
        if( Rsq4 < 1 - (SSE4)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)){
          Rsq4 <- 1 - (SSE4)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
          mainN <- n
          mainM <- m+1
          mainL <- l+2
          mainA <- a+3
          
        }
        tempMat <- allXs_matrix_extended[, -n]
        tempMat <- tempMat[, -m]
        tempMat <- tempMat[, -l]
        a <- a+1
      }
      tempMat <- allXs_matrix_extended[, -n]
      tempMat <- tempMat[, -m]
      l <- l + 1
    }
    tempMat <- allXs_matrix_extended[, -n]
    m <- m + 1
    
  }
  n <- n + 1 
}
print(counter)
print(mainN)
print(mainM)
print(mainL)
print(mainA)





FourPredModel <- allXs_matrix_extended[,-mainN]
FourPredModel <- FourPredModel[,-(mainM-1)]
FourPredModel <- FourPredModel[,-(mainL-2)]
FourPredModel <- FourPredModel[,-(mainA-3)]

s <- t(FourPredModel) %*% FourPredModel 
b4 <- (solve(s) %*% t(FourPredModel))  %*% allYs_matrix
SSE4 <- t(allYs_matrix - FourPredModel %*% b4)%*%(allYs_matrix - FourPredModel %*% b4)
Rsq4 <- 1 - (SSE4)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
RsqL4 <- 1 - (SSE4/522)/(((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)/521)
Cp4 <- SSE4 / (SSE4/522) - (522-10)
AICp4 <- 522*log(SSE4) - 522 * log(522) + 2*5
SBCp4 <- 522 * log(SSE4) - 522*log(522) + log(522)*5
i <- 1
PressP4 <- 0

while(i <= length(allYs_matrix)){
  PressP4 <- PressP4 + (allYs_matrix[i] - (FourPredModel%*%  b4)[i]) *(allYs_matrix[i] - (FourPredModel%*%  b4)[i])
  
  i <- i + 1
}

resultM[4,1]<- "4"
resultM[4,2]<- SSE4
resultM[4,3]<- Rsq4
resultM[4,4]<- RsqL4
resultM[4,5]<- Cp4
resultM[4,6]<- AICp4
resultM[4,7]<- SBCp4
resultM[4,8]<- PressP4

print(resultM)


# ASSESSMENT OF 4 PREDICTOR MODEL -- ending




# ASSESSMENT OF 3 PREDICTOR MODEL -- beginning
# variables -- allYsmatrix, allXs_matrix_extended, b6, set of Rsq values
n <- 2
m <- 2
l <- 2
a <- 2
t <- 2
Rsq3 <- -1
mainN <- 0
mainM <- 0
mainL <- 0
mainA <- 0
mainT <- 0
while (n <= 9){
  tempMat <- allXs_matrix_extended[, -n]
  m <- n
  while (m <= 8){
    tempMat <- tempMat[, -m]
    l <- m
    while (l <= 7){
      tempMat <- tempMat[, -l]
      a <- l
      while (a <= 6){
        tempMat <- tempMat[, -a]
        t <- a
        while(t <= 5) {
          tempMat <- tempMat[, -t]
          s <- t(tempMat) %*% tempMat 
          b3 <- (solve(s) %*% t(tempMat))  %*% allYs_matrix
          SSE3 <- t(allYs_matrix - tempMat %*% b3)%*%(allYs_matrix - tempMat %*% b3)
          
          if( Rsq3 < 1 - (SSE3)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)){
            Rsq3 <- 1 - (SSE3)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
            mainN <- n
            mainM <- m+1
            mainL <- l+2
            mainA <- a+3
            mainT <- t+4
            
          }
          tempMat <- allXs_matrix_extended[, -n]
          tempMat <- tempMat[, -m]
          tempMat <- tempMat[, -l]
          tempMat <- tempMat[, -a]
          t <- t+1
        }
        tempMat <- allXs_matrix_extended[, -n]
        tempMat <- tempMat[, -m]
        tempMat <- tempMat[, -l]
        a <- a+1
      }
      tempMat <- allXs_matrix_extended[, -n]
      tempMat <- tempMat[, -m]
      l <- l + 1
    }
    tempMat <- allXs_matrix_extended[, -n]
    m <- m + 1
    
  }
  n <- n + 1 
}

print(mainN)
print(mainM)
print(mainL)
print(mainA)
print(mainT)



ThreePredModel <- allXs_matrix_extended[,-mainN]
ThreePredModel <- ThreePredModel[,-(mainM-1)]
ThreePredModel <- ThreePredModel[,-(mainL-2)]
ThreePredModel <- ThreePredModel[,-(mainA-3)]
ThreePredModel <- ThreePredModel[,-(mainT-4)]

s <- t(ThreePredModel) %*% ThreePredModel 
b3 <- (solve(s) %*% t(ThreePredModel))  %*% allYs_matrix
SSE3 <- t(allYs_matrix - ThreePredModel %*% b3)%*%(allYs_matrix - ThreePredModel %*% b3)
Rsq3 <- 1 - (SSE3)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
RsqL3 <- 1 - (SSE3/522)/(((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)/521)
Cp3 <- SSE3 / (SSE3/522) - (522-8)
AICp3 <- 522*log(SSE3) - 522 * log(522) + 2*4
SBCp3 <- 522 * log(SSE3) - 522*log(522) + log(522)*4
i <- 1
PressP3 <- 0

while(i <= length(allYs_matrix)){
  PressP3 <- PressP3 + (allYs_matrix[i] - (ThreePredModel%*%  b3)[i]) *(allYs_matrix[i] - (ThreePredModel%*%  b3)[i])
  
  i <- i + 1
}

resultM[3,1]<- "3"
resultM[3,2]<- SSE3
resultM[3,3]<- Rsq3
resultM[3,4]<- RsqL3
resultM[3,5]<- Cp3
resultM[3,6]<- AICp3
resultM[3,7]<- SBCp3
resultM[3,8]<- PressP3

print(resultM)

# ASSESSMENT OF 3 PREDICTOR MODEL -- ending


# ASSESSMENT OF 2 PREDICTOR MODEL -- beginning
# variables -- allYsmatrix, allXs_matrix_extended, b6, set of Rsq values
n <- 2
m <- 2
l <- 2
a <- 2
t <- 2
h <- 2
Rsq2 <- -1
mainN <- 0
mainM <- 0
mainL <- 0
mainA <- 0
mainT <- 0
mainH <- 0
while (n <= 9){
  tempMat <- allXs_matrix_extended[, -n]
  m <- n
  while (m <= 8){
    tempMat <- tempMat[, -m]
    l <- m
    while (l <= 7){
      tempMat <- tempMat[, -l]
      a <- l
      while (a <= 6){
        tempMat <- tempMat[, -a]
        t <- a
        while(t <= 5) {
          tempMat <- tempMat[, -t]
          h <- t
          while (h <= 4){
            tempMat <- tempMat[, -h]
            s <- t(tempMat) %*% tempMat 
            b2 <- (solve(s) %*% t(tempMat))  %*% allYs_matrix
            SSE2 <- t(allYs_matrix - tempMat %*% b2)%*%(allYs_matrix - tempMat %*% b2)
            
            if( Rsq2 < 1 - (SSE2)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)){
              Rsq2 <- 1 - (SSE2)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
              mainN <- n
              mainM <- m+1
              mainL <- l+2
              mainA <- a+3
              mainT <- t+4
              mainH <- h+5
              
            }
            tempMat <- allXs_matrix_extended[, -n]
            tempMat <- tempMat[, -m]
            tempMat <- tempMat[, -l]
            tempMat <- tempMat[, -a]
            tempMat <- tempMat[, -t]
            h <- h + 1
          }
          tempMat <- allXs_matrix_extended[, -n]
          tempMat <- tempMat[, -m]
          tempMat <- tempMat[, -l]
          tempMat <- tempMat[, -a]
          t <- t+1
        }
        tempMat <- allXs_matrix_extended[, -n]
        tempMat <- tempMat[, -m]
        tempMat <- tempMat[, -l]
        a <- a+1
      }
      tempMat <- allXs_matrix_extended[, -n]
      tempMat <- tempMat[, -m]
      l <- l + 1
    }
    tempMat <- allXs_matrix_extended[, -n]
    m <- m + 1
    
  }
  n <- n + 1 
}

print(mainN)
print(mainM)
print(mainL)
print(mainA)
print(mainT)
print(mainH)




TwoPredModel <- allXs_matrix_extended[,-mainN]
TwoPredModel <- TwoPredModel[,-(mainM-1)]
TwoPredModel <- TwoPredModel[,-(mainL-2)]
TwoPredModel <- TwoPredModel[,-(mainA-3)]
TwoPredModel <- TwoPredModel[,-(mainT-4)]
TwoPredModel <- TwoPredModel[,-(mainH-5)]

s <- t(TwoPredModel) %*% TwoPredModel 
b2 <- (solve(s) %*% t(TwoPredModel))  %*% allYs_matrix
SSE2 <- t(allYs_matrix - TwoPredModel %*% b2)%*%(allYs_matrix - TwoPredModel %*% b2)
Rsq2 <- 1 - (SSE2)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
RsqL2 <- 1 - (SSE2/522)/(((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)/521)
Cp2 <- SSE2 / (SSE2/522) - (522-6)
AICp2 <- 522*log(SSE2) - 522 * log(522) + 2*3
SBCp2 <- 522 * log(SSE2) - 522*log(522) + log(522)*3
i <- 1
PressP2 <- 0

while(i <= length(allYs_matrix)){
  PressP2 <- PressP2 + (allYs_matrix[i] - (TwoPredModel%*%  b2)[i]) *(allYs_matrix[i] - (TwoPredModel%*%  b2)[i])
  
  i <- i + 1
}

resultM[2,1]<- "2"
resultM[2,2]<- SSE2
resultM[2,3]<- Rsq2
resultM[2,4]<- RsqL2
resultM[2,5]<- Cp2
resultM[2,6]<- AICp2
resultM[2,7]<- SBCp2
resultM[2,8]<- PressP2

print(resultM)

# ASSESSMENT OF 2 PREDICTOR MODEL -- ending


# ASSESSMENT OF 1 PREDICTOR MODEL -- beginning
# variables -- allYsmatrix, allXs_matrix_extended, b6, set of Rsq values

n <- 2
m <- 2
l <- 2
a <- 2
t <- 2
h <- 2
y <- 2
Rsq1 <- -1
mainN <- 0
mainM <- 0
mainL <- 0
mainA <- 0
mainT <- 0
mainH <- 0
mainY <- 0
while (n <= 9){
  tempMat <- allXs_matrix_extended[, -n]
  m <- n
  while (m <= 8){
    tempMat <- tempMat[, -m]
    l <- m
    while (l <= 7){
      tempMat <- tempMat[, -l]
      a <- l
      while (a <= 6){
        tempMat <- tempMat[, -a]
        t <- a
        while(t <= 5) {
          tempMat <- tempMat[, -t]
          h <- t
          while (h <= 4){
            tempMat <- tempMat[, -h]
            y <- h
            while (y <= 3) {
            tempMat <- tempMat[, -h]
            s <- t(tempMat) %*% tempMat
            print(tempMat)
            b1 <- (solve(s) %*% t(tempMat))  %*% allYs_matrix
            SSE1 <- t(allYs_matrix - tempMat %*% b1)%*%(allYs_matrix - tempMat %*% b1)
            
            if( Rsq1 < 1 - (SSE1)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)){
              Rsq1 <- 1 - (SSE1)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
              mainN <- n
              mainM <- m+1
              mainL <- l+2
              mainA <- a+3
              mainT <- t+4
              mainH <- h+5
              mainY <- y+6
              
            }
            tempMat <- allXs_matrix_extended[, -n]
            tempMat <- tempMat[, -m]
            tempMat <- tempMat[, -l]
            tempMat <- tempMat[, -a]
            tempMat <- tempMat[, -t]
            tempMat <- tempMat[, -y]
            y <- y + 1
            }
            
            tempMat <- allXs_matrix_extended[, -n]
            tempMat <- tempMat[, -m]
            tempMat <- tempMat[, -l]
            tempMat <- tempMat[, -a]
            tempMat <- tempMat[, -t]
            h <- h + 1
          }
          tempMat <- allXs_matrix_extended[, -n]
          tempMat <- tempMat[, -m]
          tempMat <- tempMat[, -l]
          tempMat <- tempMat[, -a]
          t <- t+1
        }
        tempMat <- allXs_matrix_extended[, -n]
        tempMat <- tempMat[, -m]
        tempMat <- tempMat[, -l]
        a <- a+1
      }
      tempMat <- allXs_matrix_extended[, -n]
      tempMat <- tempMat[, -m]
      l <- l + 1
    }
    tempMat <- allXs_matrix_extended[, -n]
    m <- m + 1
    
  }
  n <- n + 1 
}

print(mainN)
print(mainM)
print(mainL)
print(mainA)
print(mainT)
print(mainH)
print(mainY)



OnePredModel <- allXs_matrix_extended[,-mainN]
OnePredModel <- OnePredModel[,-(mainM-1)]
OnePredModel <- OnePredModel[,-(mainL-2)]
OnePredModel <- OnePredModel[,-(mainA-3)]
OnePredModel <- OnePredModel[,-(mainT-4)]
OnePredModel <- OnePredModel[,-(mainH-5)]
OnePredModel <- OnePredModel[,-(mainY-6)]

s <- t(OnePredModel) %*% OnePredModel 
b1 <- (solve(s) %*% t(OnePredModel))  %*% allYs_matrix
SSE1 <- t(allYs_matrix - OnePredModel %*% b1)%*%(allYs_matrix - OnePredModel %*% b1)
Rsq1 <- 1 - (SSE1)/((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)
RsqL1 <- 1 - (SSE1/522)/(((t(allYs_matrix) %*% (identity_matrix - J)) %*% allYs_matrix)/521)
Cp1 <- SSE1 / (SSE1/522) - (522-4)
AICp1 <- 522*log(SSE1) - 522 * log(522) + 2*2
SBCp1 <- 522 * log(SSE1) - 522*log(522) + log(522)*2
i <- 1
PressP1 <- 0

while(i <= length(allYs_matrix)){
  PressP1 <- PressP1 + (allYs_matrix[i] - (OnePredModel%*%  b1)[i]) *(allYs_matrix[i] - (OnePredModel%*%  b1)[i])
  
  i <- i + 1
}

resultM[1,1]<- "1"
resultM[1,2]<- SSE1
resultM[1,3]<- Rsq1
resultM[1,4]<- RsqL1
resultM[1,5]<- Cp1
resultM[1,6]<- AICp1
resultM[1,7]<- SBCp1
resultM[1,8]<- PressP1

print(resultM)



# ASSESSMENT OF 1 PREDICTOR MODEL -- ending

