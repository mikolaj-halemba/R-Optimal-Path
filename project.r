# Mikolaj Halemba
rm(list=ls())

toClearPath <- function(m, exit){
  row = exit[1]
  col = exit[2]
  
  m[row,col] = "e"
  number_of_rows = nrow(m)
  number_of_cols = ncol(m)
  
  i = 1
  while (i <= number_of_rows){
    j = 1
    while (j <= number_of_cols){
      value = m[i,j]
      if (value != 's' && value!= 'e' && value != 'x'){
        numeric = as.numeric(value)
        if (numeric > 0){
          m[i,j] = " "
        }
        else{
          m[i,j] = toString(-1 * numeric)
        }
      }
      j = j + 1
    }
    i = i + 1
  }
  
  return (m)
}

toSavePath = function(m, start, exit){
  row = exit[1]
  col = exit[2]
  
  counter = as.numeric(m[row,col]) - 1
  
  if (counter == -1){
    return (m)
  }
  counter = toString(counter)
  
  number_of_rows = nrow(m)
  number_of_cols = ncol(m)
  
  number = toString(-1 * as.numeric(counter))
  m[row,col] = number
  
  #right side
  if (col + 1 <= number_of_cols){
    value = m[row,col+1]
    if (value == counter){
      new_exit = c(row,col+1)
      m = toSavePath(m = m, start = start, exit = new_exit)
    }
  }
  
  #down side
  if (row + 1 <= number_of_rows){
    value = m[row + 1,col]
    if (value == counter){
      new_exit = c(row+1,col)
      m = toSavePath(m = m, start = start, exit = new_exit)
    }
  }
  
  #up side
  if (row - 1 > 0){
    value = m[row-1,col]
    if (value == counter){
      new_exit = c(row-1,col)
      m = toSavePath(m = m, start = start, exit = new_exit)
    }
  }
  
  #left side
  if (col - 1 > 0){
    value = m[row,col-1]
    if (value == counter){
      new_exit = c(row,col-1)
      m = toSavePath(m = m, start = start, exit = new_exit)
    }
  }
  
  return(m)
}

readMap <- function(path){
  data_input = readLines(path)
  len_1 = length(data_input)
  dim = nchar(data_input[1])
  matrix = matrix(0,0,dim)
  
  i = 1
  while (i <= len_1){
    row = data_input[i]
    len_2 = nchar(row)
    j = 1
    arr = c()
    while (j <= len_2){
    char = substring(row, j, j)
    arr = c(arr,char)
    j = j + 1
    }
    matrix = rbind(matrix, arr)
    i = i + 1
  }
  return(matrix)
}

findWay <- function(m){
  start = findStart(m = m)
  exit = findExit(m = m)
  
  matrix = Node(m = m, start = start, exit = exit)
  path = toSavePath(m  = matrix, start = start, exit = exit)
  matrix_path = toClearPath(m = path, exit = exit)
  
  return (matrix_path)
  
}

Node = function(m, start, exit, counter = 1){
  row = start[1]
  col = start[2]
  
  number_of_rows = nrow(m)
  number_of_cols = ncol(m)
  
  #right side
  if (col + 1 <= number_of_cols){
    value = m[row,col+1]
    if (value == " "){
      new_start = c(row,col+1)
      m[row,col+1] = counter
      m = Node(m = m, start = new_start, exit = exit, counter = counter + 1)
    }
    if (value == "e"){
      m[row,col+1] = counter
    }
  }
  
  #left side
  if (col - 1 > 0){
    value = m[row,col-1]
    if (value == " "){
      new_start = c(row,col-1)
      m[row,col-1] = counter
      m = Node(m = m, start = new_start, exit = exit, counter = counter + 1)
    }
    if (value == "e"){
      m[row,col-1] = counter
    }
  }
  
  #down side
  if (row + 1 <= number_of_rows){
    value = m[row + 1,col]
    if (value == " "){
      new_start = c(row+1,col)
      m[row+1,col] = counter
      m = Node(m = m, start = new_start, exit = exit, counter = counter + 1)
    }
    if (value == "e"){
      m[row+1,col] = counter
    }
  }
  
  #up side
  if (row - 1 > 0){
    value = m[row-1,col]
    if (value == " "){
      new_start = c(row-1,col)
      m[row-1,col] = counter
      m = Node(m = m, start = new_start, exit = exit, counter = counter + 1)
    }
    if (value == "e"){
      m[row-1,col] = counter
    }
  }
  
  return (m)
}

findStart = function(m){
  number_of_rows = nrow(m)
  number_of_cols = ncol(m)
  
  j = 1
  while (j <= number_of_cols){
    i = 1
    while (i<= number_of_rows){
      value = m[i,j]
      if (value == 's'){
        return (c(i,j))
      }
      i = i + 1
    }
    j = j + 1
  }
}

findExit <- function(m){
  number_of_rows = nrow(m)
  number_of_cols = ncol(m)
  
  j = 1
  while (j <= number_of_cols){
    i = 1
    while (i<= number_of_rows){
      value = m[i,j]
      if (value == 'e'){
        return (c(i,j))
      }
      i = i + 1
    }
    j = j + 1
  }
}

plotMap <- function(m){
  
  number_of_rows = nrow(m)
  number_of_cols = ncol(m)
  
  vectorX = c()
  vectorY = c()
  
  i = 1
  while (i<= number_of_rows){
    j = 1
    while(j <= number_of_cols){
      value = m[i,j]
      if (value == 'x'){
        x = c(number_of_rows - i + 1)
        y = c(j)
        vectorX = c(vectorX,x)
        vectorY = c(vectorY,y)
      }
        
      j = j + 1
    }
    i = i + 1
  }
  
  plot(vectorY, 
       vectorX, 
       col='black',
       axes = FALSE,
       pch = 15,
       cex = 4.09,
       xlab="", 
       ylab="",
       xlim =c(1,number_of_cols), 
       ylim=c(1,number_of_rows),)

}

plotPath <- function(a){
  start = findStart(m = a)
  
  x_start = start[1]
  y_start = start[2]
  
  number_of_rows = nrow(a)
  number_of_cols = ncol(a)
  
  vector_startX = c(number_of_rows - x_start + 1)
  vector_startY = c(y_start)
  
  vectorX = getX(a = a, x_start = x_start, y_start = y_start, vectorX = vector_startX, counter = 0)
  vectorY = getY(a = a, x_start = x_start, y_start = y_start, vectorY = vector_startY, counter = 0)
  len = length(vectorX)
  
  points(vectorY[1],vectorX[1],col = 'black',pch=1, cex=1.0)
  points(vectorY[len],vectorX[len],col='black',pch=1, cex=1.0)
  
  lines(vectorY, 
       vectorX, 
       xlab="", 
       ylab="",
       type = "b", 
       pch = 19,
       cex = 1.2,
       col = "red",
       lty = 7)
       
}

getX <- function(a, x_start, y_start, vectorX, counter){
  number_of_rows = nrow(a)
  number_of_cols = ncol(a)
  row = x_start
  col = y_start
  main_value = toString(counter)
  
  #right side
  if (col + 1 <= number_of_cols){
    value = a[row,col+1]
    if (value == main_value){
      new_y_start = col + 1
      vector = c(number_of_rows - row + 1)
      vectorX = c(vectorX,vector)
      main_vector = getX(a = a, x_start = x_start, y_start = new_y_start,vectorX = vectorX, counter = counter + 1)
    }
    if (value == "e"){
      vector = c(number_of_rows - row + 1)
      vector2 = c(number_of_rows - row + 1)
      main_vector = c(vectorX,vector,vector2)
    }
  }
  
  #down side
  if (row + 1 <= number_of_rows){
    value = a[row+1,col]
    if (value == main_value){
      new_x_start = row + 1
      vector = c(number_of_rows - row)
      vectorX = c(vectorX,vector)
      main_vector = getX(a = a, x_start = new_x_start, y_start = y_start,vectorX = vectorX,counter = counter + 1)
    }
    if (value == "e"){
      vector = c(number_of_rows - row + 1)
      vector2 = c(number_of_rows - row)
      main_vector = c(vectorX,vector,vector2)
    }
  }
  
  #left side
  if (col - 1 > 0){
    value = a[row,col-1]
    if (value == main_value){
      new_y_start = col - 1
      a[row,col] = ""
      vector = c(number_of_rows - row + 1)
      vectorX = c(vectorX,vector)
      main_vector = getX(a = a, x_start = x_start, y_start = new_y_start,vectorX = vectorX, counter = counter + 1)
    }
    if (value == "e"){
      a[row,col] = ""
      a[row,col-1] = ""
      vector = c(number_of_rows - row + 1)
      vector2 = c(number_of_rows - row + 1)
      main_vector = c(vectorX,vector,vector2)
    }
  }
  
  #up side
  if (row - 1 > 0){
    value = a[row-1,col]
    if (value == main_value){
      new_x_start = row - 1
      vector = c(number_of_rows - row + 2)
      vectorX = c(vectorX,vector)
      main_vector = getX(a = a, x_start = new_x_start, y_start = y_start,vectorX = vectorX, counter = counter + 1)
    }
    if (value == "e"){
      vector = c(number_of_rows - row + 1)
      vector2 = c(number_of_rows - row + 2)
      main_vector = c(vectorX,vector,vector2)
    }
  }
  return(main_vector)
}


getY <- function(a, x_start, y_start, vectorY, counter){
  number_of_rows = nrow(a)
  number_of_cols = ncol(a)
  row = x_start
  col = y_start
  main_value = toString(counter)
  
  #right side
  if (col + 1 <= number_of_cols){
    value = a[row,col+1]
    if (value == main_value){
      new_y_start = col + 1
      a[row,col] = ""
      vector = c(col+1)
      vectorY = c(vectorY,vector)
      main_vector = getY(a = a, x_start = x_start, y_start = new_y_start,vectorY = vectorY, counter = counter + 1)
    }
    if (value == "e"){
      a[row,col] = ""
      a[row,col+1] = ""
      vector = c(col)
      vector2 = c(col+1)
      main_vector = c(vectorY,vector,vector2)
    }
  }
  
  #down side
  if (row + 1 <= number_of_rows){
    value = a[row+1,col]
    if (value == main_value){
      new_x_start = row + 1
      a[row,col] = " "
      vector = c(col)
      vectorY = c(vectorY,vector)
      main_vector = getY(a = a, x_start = new_x_start, y_start = y_start,vectorY = vectorY, counter = counter + 1)
    }
    if (value == "e"){
      a[row,col] = ""
      a[row+1,col] = ""
      vector = c(col)
      vector2 = c(col)
      main_vector = c(vectorY,vector,vector2)
    }
  }
  
  #left side
  if (col - 1 > 0){
    value = a[row,col-1]
    if (value == main_value){
      new_y_start = col - 1
      a[row,col] = ""
      vector = c(col-1)
      vectorY = c(vectorY,vector)
      main_vector = getY(a = a, x_start = x_start, y_start = new_y_start,vectorY = vectorY, counter = counter + 1)
    }
    if (value == "e"){
      a[row,col] = ""
      a[row,col-1] = ""
      vector = c(col)
      vector2 = c(col-1)
      main_vector = c(vectorY,vector,vector2)
    }
  }
  
  #up side
  if (row - 1 > 0){
    value = a[row-1,col]
    if (value == main_value){
      new_x_start = row - 1
      a[row,col] = ""
      vector = c(col)
      vectorY = c(vectorY,vector)
      main_vector = getY(a = a, x_start = new_x_start, y_start = y_start,vectorY = vectorY, counter = counter + 1)
    }
    if (value == "e"){
      a[row,col] = ""
      a[row-1,col] = ""
      vector = c(col)
      vector2 = c(col)
      main_vector = c(vectorY,vector,vector2)
    }
  }
  return(main_vector)
}

m <- readMap(path = "./maps/maze_0.map")
a <- findWay(m = m)
plotMap(m = m)
plotPath(a = a)

m2 <- readMap(path = "./maps/maze_1.map")
a2 <- findWay(m = m2)
plotMap(m = m2)
plotPath(a = a2)

