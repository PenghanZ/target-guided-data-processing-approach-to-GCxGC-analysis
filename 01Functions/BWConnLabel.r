
BWConnLabel <- function(BinaryMatrix){

  ############## Step 1 create data sheet to record visits and labels ############ 
  
RowNum <- dim(BinaryMatrix)[1]
ColNum <- dim(BinaryMatrix)[2]
visited <- matrix(data= 1, nrow = RowNum, ncol = ColNum, byrow = FALSE, dimnames = NULL)
visited <- visited == 0
Labeled <- matrix(data= 0, nrow = RowNum, ncol = ColNum, byrow = FALSE, dimnames = NULL);
ID_counter = 1;

####################### Step 2 check location is visited or not  ###################

for(col in 1:ColNum) {
for(row in 1:RowNum) {

if(BinaryMatrix[row,col] == 0) {
visited[row,col] = TRUE

### if the location have been visited, continue
} else if(visited[row,col]) {
next

} else {
  
  ########### Step 3 for unvisited location, check the location and its neighbers ############
  ### initial a stack bin to racord all locations
  stack = rbind(c(0,0),t(matrix(c(row,col))))
  
  ### if the bin is not empty
while (length(stack) > 2){
  
  ### take one location and remove it from the stack bin
loc = stack[2,]
stack <- stack[-2,]

### if the location have been visited, next
if(visited[loc[1],loc[2]]){
next
}

			#end of if
### mark the location is visited and give a unique ID
visited[loc[1],loc[2]] <- TRUE
Labeled[loc[1],loc[2]] = ID_counter

### check the 4 neighbors
locs_y <- c(loc[1]-1, loc[1]+1, loc[1], loc[1])
locs_x <- c(loc[2], loc[2], loc[2]-1, loc[2]+1)

### remove the locations out of bounds  
OutBounds <- locs_y < 1 | locs_y > RowNum | locs_x < 1 | locs_x > ColNum
InBounds <- OutBounds == FALSE
locs_y <- locs_y[InBounds]
locs_x <- locs_x[InBounds]

### remove the visited locations
ind1 <- (locs_x-1)*RowNum + locs_y
IsVisited <- visited[ind1]
NonVisited <- IsVisited == FALSE
locs_y <- locs_y[NonVisited]
locs_x <- locs_x[NonVisited]

### romove the lication with 0 value
ind2 <- (locs_x-1)*RowNum + locs_y
is_1 <- BinaryMatrix[ind2] == 1
locs_y <- locs_y[is_1]
locs_x <- locs_x[is_1]

### add remaining neighbers
stack = rbind(stack, cbind(locs_y,locs_x)) ### locs_y is the row number


}
			#end of while loop 

ID_counter = ID_counter + 1

}
			#end of else
}
			#end of for(col in 1:ColNum)
}
			#end of for(row in 1:RowNum)
return(Labeled)
}