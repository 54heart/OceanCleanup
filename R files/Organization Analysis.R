setwd("C:\Users\16971\Downloads\Data Challenge")
cleanup <- read.csv("clean13.csv")

cleanup <- clean13



group_info <- data.frame(cleanup$`Group Name` ,cleanup$People)
group_info <- group_info[!duplicated(group_info$cleanup..Group.Name.),]
colnames(group_info) <- c("GroupName","Size") 
group_info <- group_info[order(group_info$GroupName),]


# active group/individual per year
year2015 <- subset(cleanup,cleanup$year==2015 & cleanup$People>=1)
year2015$x <- 1
active2015 <- aggregate(year2015$x,list(year2015$`Group Name`),sum)
active2015 <- active2015[order(active2015$x),]
colnames(active2015) <- c("GroupName","Attendence")
active2015 <- merge(active2015,group_info,by="GroupName")
active2015$GroupYes <- ifelse(active2015$Size!=1,1,0)
write.csv(active2015,file="Z:/Downloads/Data Challenge/active2015.csv")

year2016 <- subset(cleanup,cleanup$year==2016 & cleanup$People>=1)
year2016$x <- 1
active2016 <- aggregate(year2016$x,list(year2016$`Group Name`),sum)
active2016 <- active2016[order(active2016$x),]
colnames(active2016) <- c("GroupName","Attendence")
active2016 <- merge(active2016,group_info,by="GroupName")
active2016$GroupYes <- ifelse(active2016$Size!=1,1,0)
write.csv(active2016,file="Z:/Downloads/Data Challenge/active2016.csv")

year2017 <- subset(cleanup,cleanup$year==2017 & cleanup$People>=1)
year2017$x <- 1
active2017 <- aggregate(year2017$x,list(year2017$`Group Name`),sum)
active2017 <- active2017[order(active2017$x),]
colnames(active2017) <- c("GroupName","Attendence")
active2017 <- merge(active2017,group_info,by="GroupName")
active2017$GroupYes <- ifelse(active2017$Size!=1,1,0)
write.csv(active2017,file="Z:/Downloads/Data Challenge/active2017.csv")

year2018 <- subset(cleanup,cleanup$year==2018 & cleanup$People>=1)
year2018$x <- 1
active2018 <- aggregate(year2018$x,list(year2018$`Group Name`),sum)
active2018 <- active2018[order(active2018$x),]
colnames(active2018) <- c("GroupName","Attendence")
active2018 <- merge(active2018,group_info,by="GroupName")
active2018$GroupYes <- ifelse(active2018$Size!=1,1,0)
write.csv(active2018,file="Z:/Downloads/Data Challenge/active2018.csv")


###Organization Problem
# % of children/adult per year
children <- aggregate(cleanup$Children,list(cleanup$year),sum)
adult <- aggregate(cleanup$Adults,list(cleanup$year),sum)


children_adult <- data.frame(children,adult$x)
colnames(children_adult) <- c("year","children","adult")
children_adult$percentage <- children_adult$children/children_adult$adult
write.csv(children_adult,file="Z:/Downloads/Data Challenge/children_adult_year.csv")

# % of children/adult per state
children <- aggregate(cleanup$Children,list(cleanup$State),sum)
adult <- aggregate(cleanup$Adults,list(cleanup$State),sum)

children_adult <- data.frame(children,adult$x)
colnames(children_adult) <- c("state","children","adult")
children_adult$percentage <- children_adult$children/children_adult$adult
write.csv(children_adult,file="Z:/Downloads/Data Challenge/children_adult_state.csv")

# Regression: when children go with adults to collect garbage, does the collection efficiency of adults go up??
cleanup$ChildrenYes <- ifelse(cleanup$Children>=1,1,0)
childrenAnalysis <- subset(cleanup,cleanup$People>=1)
childrenAnalysis$efficiency_pounds <- childrenAnalysis$Pounds/childrenAnalysis$People
childrenAnalysis$efficiency_items <- childrenAnalysis$`Total Items Collected`/childrenAnalysis$People
summary(childrenAnalysis$efficiency_pounds)
IQR_max_pounds <- 6.5+1.5*(6.5-0.5)
IQR_min_pounds <- 0.5-1.5*(6.5-0.5)
IQR_max_pounds
IQR_min_pounds
childrenAnalysis <- subset(childrenAnalysis,childrenAnalysis$efficiency_pounds <= 15.5 & childrenAnalysis$efficiency_items<=192.75)
summary(childrenAnalysis$efficiency_items)
IQR_max_items <- 82.5+1.5*(82.5-9)
IQR_min_items <- 9-1.5*(82.5-9)
IQR_max_items
IQR_min_items

# what's the relationship with ratio of children to adult and group cleanup efficiency?
childrenAnalysis$ratio <- childrenAnalysis$Children/childrenAnalysis$Adults
ratio <- aggregate(childrenAnalysis$efficiency_pounds,list(childrenAnalysis$ratio),mean)
write.csv(ratio,file="C:\\Users\\16971\\Downloads\\Data Challenge\\ratio.csv")

childrenEffect_pounds <- aggregate(childrenAnalysis$efficiency_pounds,list(childrenAnalysis$ChildrenYes),mean)
childrenEffect_items <- aggregate(childrenAnalysis$efficiency_items,list(childrenAnalysis$ChildrenYes),mean)
childrenEffect <- data.frame(childrenEffect_pounds,childrenEffect_items$x)

colnames(childrenEffect) <- c("ChildrenYes","Efficiency_pounds","Efficiency_items")
write.csv(childrenEffect,file="Z:/Downloads/Data Challenge/childrenEffect.csv")

childrenEffect_Pounds_items <- childrenAnalysis$Pounds/(childrenAnalysis$`Total Items Collected`*childrenAnalysis$People)
children_weight_Perference <- aggregate(childrenEffect_Pounds_items,list(childrenAnalysis$ChildrenYes),mean)

### a group member or individual collect more?
group <- subset(cleanup,cleanup$People>=1)
group <- subset(group,group$group_items <=15.5 & group$group_pounds<=192.75)
group$IndividualYes <- ifelse(group$People==1,1,0)
IndividualYes_pounds <- aggregate(group$group_pounds,list(group$IndividualYes),mean)
IndividualYes_items <- aggregate(group$group_items,list(group$IndividualYes),mean)
groupEffect <- data.frame(IndividualYes_pounds,IndividualYes_items$x)
colnames(groupEffect) <- c("IndividualYes","Pounds Per Person","Items Per Person")
write.csv(groupEffect,file="z:/Downloads/Data Challenge/GroupEffect.csv")

# individual <- subset(group,group$People==1)
# small_group <- subset(group,1<group$People<=5)
# median_group <- subset(group,5<group$People & group$People<=20)
# large_group <- subset(group,20<group$People)

group$group_pounds <- group$Pounds/group$People
group$group_items <- group$`Total Items Collected`/group$People
group$normalized_pounds <- scale(group$Pounds) # normalized pounds
group$normalized_items <- scale(group$Total.Items.Collected) # normalized items
group$collection_capands/group$normalized_items

group_pounds <- aggregate(group$group_pounds,list(group$People),mean)
group_items <- aggregate(group$group_items,list(group$People),mean)
colnames(group_pounds) <- c("GroupSize","Pounds_Per_Person")
colnames(group_items) <- c("GroupSize","Items_Per_Person")
groupEffect1 <- data.frame(group_pounds,group_items$Items_Per_Person)


write.csv(groupEffect1,file="z:/Downloads/Data challenge/GroupEffect1.csv")

# How often do groups with different size to do collections?
group$x <- 1
Attendance <- aggregate(group$x,list(group$`Group Name`),sum)
colnames(Attendance) <- c("GroupName","Times")
Attendance <- merge(group_info,Attendance,by="GroupName")
times <- aggregate(Attendance$Times,list(Attendance$Size),mean)
colnames(times) <- c("Group Size","times")
write.csv(times,file="Z:/Downloads/Data Challenge/Times.csv")

Attendance1 <- subset(Attendance, Attendance$Size>=770 & Attendance$Size<=880)
group$GroupYes <- ifelse(group$People>=1,1,0)
group$Ratio_children_adult <- group$Children/group$Adults
reg1 <- lm(group_pounds~People+Adults+ChildrenYes,data=group)

#individual
single_pounds <- mean(individual$Pounds/individual$People)
icollection_capability1 <- mean(individual$Total.Items.Collected/individual$People)
icollection_capability


#small
scollection_capability <- mean(small_group$Pounds/small_group$People)
scollection_capability1 <- mean(small_group$Total.Items.Collected/small_group$People)
scollection_capability

#median
mcollection_capability <- mean(median_group$Pounds/median_group$People)
mcollection_capability1 <- mean(median_group$Total.Items.Collected/median_group$People)

#large
lcollection_capability <- mean(large_group$Pounds/large_group$People)
lcollection_capability1 <- mean(large_group$Total.Items.Collected/large_group$People)


collection_capability <- data.frame(icollection_capability,scollection_capability,mcollection_capability,lcollection_capability) 
collection_capability1 <- data.frame(icollection_capability1,scollection_capability1,mcollection_capability1,lcollection_capability1)



