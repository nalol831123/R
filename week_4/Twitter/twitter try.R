library(devtools)
library(twitteR)
library(data.table)
install.packages("RCurl")

tweets <- searchTwitter('#nba', 
                        n=50, 
                        since = '2018-03-19', 
                        until = '2018-03-26')

tweets.list <- twListToDF(tweets)
names.list <-  rbindlist(lapply(tweets.list$screenName, 
                                as.data.frame))

names(names.list)[1] <- "Name"

alldata <- data.frame()

for (i in 1:3){ #Cursor
  tryCatch(
    {
      # get name from '#_______' users list
      tag.user <- names.list$Name[i]
      
      # print query location
      print(paste(i, tag.user))
      
      # get User's twitter account
      tag.user.account <- getUser(tag.user)
      
      # get account's friend (if accessible)
      user.friends <- tag.user.account$getFriends(retryOnRateLimit=180)
      print(length(user.friends))
      
      # limit
      if (length(user.friends) < 3000){
        
        # Make data.table of user's friends data list.
        friends.df <- rbindlist(lapply(user.friends, as.data.frame))
        
        # Get the only friends name column.
        friends.name.df <- data.frame(tempname=c(friends.df$name))
        
        # Change column name
        colname <- toString(tag.user)
        setnames(friends.name.df, c(colname))
        
        # Write table
        write.table(friends.name.df, file = paste(colname, ".csv"))
        
        # bind data in the same data.frame
        alldata <- rbind.fill(alldata, friends.name.df)
        #data <- cbind(list(data, friends.name.df))
      }
      else{
        print(paste(i, tag.user, "<== friends count > 500"))
      }
    },
    warning = function(w){},
    error = function(e){
      #ERROR (need to store it?)
      print(paste("ERROR", tag.user))
    },
    finally = {
      print("End Try&Catch")
    })
  
  i = i+1
}
