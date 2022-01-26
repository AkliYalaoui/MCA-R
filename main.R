library(FactoMineR)

# Import our data

data <- read.table(file.choose(),sep =",",header = T)


##############################Data cleaning

#Check our variables type
summary(data)

# this is just an index, we remove it
data$Number <- NULL

# check for NA values

for (i in 1: ncol(data)){
  print(sum(is.na(data[,i])))
}

# Check for empty values

for (i in 1: ncol(data)){
  print(sum((data[,i] == "")))
}

# This function returns the mode of the categorical variable

Mode <- function(u){
  tmp <- unique(u)
  tmp[which.max(tabulate(match(u,tmp)))]
}

# substitute empty values with the mode of the variables

for (i in 1: ncol(data)){
  data[data[,i] == "",i] <- Mode(data[,i])
}
################################## Data visualization
Cat_plot <- function(y,title){
  n <- table(y)
  barplot(n[order(n, decreasing = TRUE)],las = 2,main=title,ylab = "Count")
  
}

Cat_plot(data$Nationality,"Frequence of each modality in : Nationality")

#The majority (77) of the students surveyed were Chinese 
#followed by Saudi Arabia, and Korea at 6
#The majority of the countries either had 2 or 1 student surveyed.

# AFCM

# transform variables to factors
for(i in 1: ncol(data)){
  data[,i] <- as.factor(data[,i])
}

# make the complete disjunctive table
tdc <- tab.disjonctif(data)

#compute frequencies of levels
ns <- apply(tdc,2,sum)
fs <- ns / nrow(tdc)

# apply mca to data
acm <- MCA(data)

# eigen values
head(acm$eig)
barplot(acm$eig[,1],acm$eig[,2])

# absolute contribution
acm$var$contrib

# quality of representation of variables
acm$var$cos2
########################################### AFC

tc <- table(data$Age,data$Gender)

afc <- CA(tc)
afc













