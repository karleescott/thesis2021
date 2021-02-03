#smallest distance between two functions
minimumDistance <- function(fun1,fun2){
  dis <- 180
  for (i in 1:nrow(fun1)){
    for(j in 1:nrow(fun2)){
      num <- sqrt((fun1[i,3]-fun2[j,3])^2+(fun1[i,4]-fun2[j,4])^2)
      if(num<dis){
        dis <- num
        loc1 <- i
        loc2 <- j
      }
    }
  }
  minDisLoc <- list(dis,loc1,loc2)
  return(minDisLoc)
}

#combines closest functions to form best path
totalPath <- function(df1,df2){
  fun1 <- df1 %>%
    filter(group == 1)
  fun2 <- df1 %>%
    filter(group == 2)
  fun3 <- df1 %>%
    filter(group == 3)
  fun4 <- df1 %>%
    filter(group == 4)
  fun5 <- df2 %>%
    filter(group == 5)
  fun6 <- df2 %>%
    filter(group == 6)
  fun7 <- df2 %>%
    filter(group == 7)
  fun8 <- df2 %>%
    filter(group == 8)
  
  mindis15 <- data.frame(minimumDistance(fun1,fun5))
  colnames(mindis15) <- c("Distance","Loc1","Loc2")
  mindis16 <- data.frame(minimumDistance(fun1,fun6))
  colnames(mindis16) <- c("Distance","Loc1","Loc2")
  mindis17 <- data.frame(minimumDistance(fun1,fun7))
  colnames(mindis17) <- c("Distance","Loc1","Loc2")
  mindis18 <- data.frame(minimumDistance(fun1,fun8))
  colnames(mindis18) <- c("Distance","Loc1","Loc2")
  
  mindis25 <- data.frame(minimumDistance(fun2,fun5))
  colnames(mindis25) <- c("Distance","Loc1","Loc2")
  mindis26 <- data.frame(minimumDistance(fun2,fun6))
  colnames(mindis26) <- c("Distance","Loc1","Loc2")
  mindis27 <- data.frame(minimumDistance(fun2,fun7))
  colnames(mindis27) <- c("Distance","Loc1","Loc2")
  mindis28 <- data.frame(minimumDistance(fun2,fun8))
  colnames(mindis28) <- c("Distance","Loc1","Loc2")
  
  mindis35 <- data.frame(minimumDistance(fun3,fun5))
  colnames(mindis35) <- c("Distance","Loc1","Loc2")
  mindis36 <- data.frame(minimumDistance(fun3,fun6))
  colnames(mindis36) <- c("Distance","Loc1","Loc2")
  mindis37 <- data.frame(minimumDistance(fun3,fun7))
  colnames(mindis37) <- c("Distance","Loc1","Loc2")
  mindis38 <- data.frame(minimumDistance(fun3,fun8))
  colnames(mindis38) <- c("Distance","Loc1","Loc2")
  
  mindis45 <- data.frame(minimumDistance(fun4,fun5))
  colnames(mindis45) <- c("Distance","Loc1","Loc2")
  mindis46 <- data.frame(minimumDistance(fun4,fun6))
  colnames(mindis46) <- c("Distance","Loc1","Loc2")
  mindis47 <- data.frame(minimumDistance(fun4,fun7))
  colnames(mindis47) <- c("Distance","Loc1","Loc2")
  mindis48 <- data.frame(minimumDistance(fun4,fun8))
  colnames(mindis48) <- c("Distance","Loc1","Loc2")
  
  mindis <- rbind(mindis15,mindis16,mindis17,mindis18,mindis25,mindis26,mindis27,mindis28,mindis35,mindis36,mindis37,mindis38,mindis45,mindis46,mindis47,mindis48)
  
  minIndex <- which.min(mindis[,1])
  selMinDis <- mindis[minIndex,]
  
  if(minIndex/4<=1){
    Fun1 <- fun1 %>%
      filter(tz < as.numeric(selMinDis[2]))
    Fun2 <- df2[df2$group==minIndex+4,] %>%
      filter(tz < as.numeric(selMinDis[3]))
    
    combinedFunction <- rbind(Fun1,Fun2)
  } 
  
  else if(minIndex/4<=2){
    Fun1 <- fun2 %>%
      filter(tz < as.numeric(selMinDis[2]))
    Fun2 <- df2[df2$group==minIndex,] %>%
      filter(tz < as.numeric(selMinDis[3]))
    
    combinedFunction <- rbind(Fun1,Fun2)
  }
  
  else if(minIndex/4<=3){
    Fun1 <- fun3 %>%
      filter(tz < as.numeric(selMinDis[2]))
    Fun2 <- df2[df2$group==minIndex-4,] %>%
      filter(tz < as.numeric(selMinDis[3]))
    
    combinedFunction <- rbind(Fun1,Fun2)
  }
  
  else {
    Fun1 <- fun4 %>%
      filter(tz < as.numeric(selMinDis[2]))
    Fun2 <- df2[df2$group==minIndex-8,] %>%
      filter(tz < as.numeric(selMinDis[3]))
    
    combinedFunction <- rbind(Fun1,Fun2)
    
  }
  
  for (i in 1:nrow(combinedFunction)){
    combinedFunction[i,2] <- i
  }
  
  return(combinedFunction)
  
}
