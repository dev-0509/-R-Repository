
issueSprints <- "c(\"com.atlassian.greenhopper.service.sprint.Sprint@50eaa052[rapidViewId=11,state=CLOSED,name= ARCADE Pisa sprint 113,startDate=2016-10-06T14:40:52.687+01:00,endDate=2016-10-20T14:40:00.000+01:00,completeDate=2016-10-21T06:32:06.424+01:00,sequence=3713,id=3713]\", \"com.atlassian.greenhopper.service.sprint.Sprint@2f7478ff[rapidViewId=11,state=CLOSED,name=ARCADE sprint 114,startDate=2016-10-20T14:25:40.557+01:00,endDate=2016-11-03T14:25:00.000Z,completeDate=2016-11-04T14:16:23.604Z,sequence=3748,id=3748]\")"

str <- strsplit(issueSprints, ",")

str <- grep("name=", str[[1]], value = T)

k <- 3
index <- 1
pos <- 2
vec <- vector(mode = "character", length = 10)

data <- data.frame("team"=character(), "sprint"=as.numeric(), stringsAsFactors = F)

for(i in str)
{
  i <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", i, perl=TRUE)

  s <- strsplit(i, "[= ]")
  
  name <- s[[1]][2]
  
  for(j in s[[1]])
  {
    if( identical(j, "Sprint") || identical(j, "sprint") )
      break
    else
    {
      if( identical(s[[1]][pos+1], "Sprint") || identical(s[[1]][pos+1], "sprint") )
        break
      else
        pos = pos + 1
      
      name <- paste(name, s[[1]][k], sep = " ")
      pos = k
      k = k + 1
      index = index + 1
    }
  }
  
  if(index == 1)
  {
    data[nrow(data) + 1,] <- c(s[[1]][pos], s[[1]][pos+2])
  }
  else
    data[nrow(data) + 1,] <- c(name, s[[1]][pos+2])
  
  pos <- 2
  k <- 3
  index = 1
}

print(data)
print(str(data))