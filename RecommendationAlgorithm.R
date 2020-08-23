#Generalized recommendation engine based on User ranking, (and further user analysis)
#Load Packages
Packages <- c("data.table", "tidyverse", "knitr", "recommenderlab", "stringi", "reshape2", "dplyr", "rts", "plyr")
lapply(Packages, library, character.only=TRUE)

#Data: UserID, itemID, rating, timestamp
Data=read.csv("C:/Users/PC/Desktop/ratings.csv") #UserID, itemID, rating, timestamp
names(Data) <- c("UserID", "ItemID", "Rating", "Timestamp")
Dim_Data = read.csv("C:/Users/PC/Desktop/dimension.csv") #ItemID, ProductName, ProductCategory
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
Top_5_df$ItemID=as.numeric(levels(Top_5_df$ItemID))#Since ItemID is of type integer in Dim_Data, we typecast id in our recommendations as well

names=left_join(Top_5_df, Dim_Data, by="ItemID")# Merge the ItemID with names to get ProductName and ProductCategory
RecList[[i]] <- names
pbar$step()
}

#What recommendations are more present than others?
Category <- 0
CatTable <- 0
for (i in 1:length(RecList)) {
  Category = RecList[[i]]$ItemID
  CatTable = rbind(CatTable, Category)
  }
TopCategories <- sort(table(CatTable),decreasing=TRUE)[1:10]
List <- as.numeric(names(TopCategories))


#Work In progress: Trying to connect the ItemID's stored in TopCategories, with the Categories of Dim_Data.
TopCategories <- which(apply(Dim_Data, 1, function(List) any(List %in% Dim_Data)))

#What UserID's are ranking higher than others?

