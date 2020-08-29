#Generalized recommendation engine based on User ranking, (and further user analysis)
#  Engines like these are not only employed by frontend of ecommerce, but can also be used in mapping store/warehouse layout: such that items are placed closer together,
#  when bought more often bought or at the same time. It can also be employed in loyalty Programmes:To reactivate churned costumers by re-engaging them with customised offers 
#  based on their purchase history.

#Load Packages
Packages <- c("data.table", "tidyverse", "knitr", "recommenderlab", 
              "stringi", "reshape2", "dplyr", "rts", "plyr")
#install.packages(Packages)
lapply(Packages, library, character.only=TRUE)

#Data: UserID, itemID, rating, timestamp
Data=read.csv("C:/Users/rasmu/OneDrive/Skrivebord/Github/Data/ratings.csv") #UserID (int), itemID (int), rating (num), timestamp (int)
names(Data) <- c("UserID", "ItemID", "Rating", "Timestamp")
Dim_Data = read.csv("C:/Users/rasmu/OneDrive/Skrivebord/Github/Data/dimension.csv") #ItemID (int), ProductName (chr), ProductCategory (chr)
names(Dim_Data) <- c("ItemID", "ProductName", "ProductCategory")

#Create ratings matrix with rows as users and columns as movies. We don't need timestamp
mRating = dcast(Data, UserID~ItemID, value.var = "Rating", na.rm=FALSE)
mRating = as.matrix(mRating[,-1])   #We can now remove user ids
mRating = as(mRating, "realRatingMatrix") #Convert ratings matrix to real rating matrx which makes it dense
mRating = normalize(mRating) #Normalize the ratings matrix

#Create Recommender Model. The parameters are UBCF and Cosine similarity. We take 10 nearest neighbours
y <- 10 #Number of nearest neighbours we want included
modRating = Recommender(mRating, method = "UBCF", param=list(method="Cosine",nn=y))

#Preparing for loop
RecList <- vector("list", length(Data$UserID))
k <- 20 #Number of steps in progress bar
pbar <- create_progress_bar('text')
pbar$init(k)
for (i in 1:length(Data$UserID)) { #For all UserID's, estimate recommendations using nearest neighbour specified above
  Top_5_pred = predict(modRating, mRating[i], n=5)# Obtain top 5 recommendations for X'st userID entry in dataset
  Top_5_List = as(Top_5_pred, "list") #Convert the recommendations to a list
  Top_5_df=data.frame(Top_5_List) #We convert the list to a dataframe and change the column name to ItemID
  colnames(Top_5_df)="ItemID" #Chaning the name of the vector
  Top_5_df$ItemID=as.numeric((Top_5_df$ItemID))#Since ItemID is of type integer in Dim_Data, we typecast id in our recommendations as well
  
  names=left_join(Top_5_df, Dim_Data, by="ItemID")# Merge the ItemID with names to get ProductName and ProductCategory
  RecList[[i]] <- names
  pbar$step()
}
#What is recommended for UserID entry 1?
RecList[[1]]

#What recommendations are more present than others?
Category <- 0
CatTable <- 0
for (i in 1:length(RecList)) {
  Category = RecList[[i]]$ItemID
  CatTable = rbind(CatTable, Category)
}
TopCategories <- names(sort(table(CatTable),decreasing=TRUE)[1:10])
TopCategories <- Dim_Data[Dim_Data$ItemID %in% TopCategories,]
TopCategories
