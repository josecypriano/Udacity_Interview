install.packages("dplyr")
install.packages("gplots")

library(dplyr)
library(gplots)

#Upload Data Frames

df_applications <- read.table("https://raw.githubusercontent.com/josecypriano/Udacity_Interview/master/applications.csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
df_questions <- read.table("https://raw.githubusercontent.com/josecypriano/Udacity_Interview/master/questions.csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
df_intervals <- read.table("https://raw.githubusercontent.com/josecypriano/Udacity_Interview/master/intervals.csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
df_questions_category <- data.frame("Id_Question"=c(1,2,3), "Question2"=c("Education","Purpose","No Response"))
df_week <- data.frame("WeekNuM"=sort(rep(c(1:26),7),decreasing=FALSE),"From"=c(1:182))
df_week$Category <- paste("Week","WeekNuM"=sort(rep(c(1:26),7),decreasing=FALSE))
df_week$WeekNuM <- NULL

df_questions_educ <- subset(df_questions,question=="education")
df_questions_educ$question <- NULL

#Treatment DataBase * Questions *

df_questions$question2 <- ifelse(df_questions$response!="",df_questions$question,"No Response")

df_questions_analysis <- subset(df_questions,question2!="No Response")

df_questions_analysis$question<-NULL
df_questions_analysis$response<-NULL

df_questions_analysis$Education <- ifelse(df_questions_analysis$question2=="education",10,0)
df_questions_analysis$Purpose <- ifelse(df_questions_analysis$question2=="purpose",5,0)
df_questions_analysis$Total <- df_questions_analysis$Education + df_questions_analysis$Purpose
df_questions_analysis <- aggregate(df_questions_analysis$Total,
                                   list(df_questions_analysis$applicant_id),
                                   FUN="sum")
df_questions_analysis$TypeofApplication <- with(df_questions_analysis, ifelse(x==15,"Education & Purpose",ifelse(x==10,"Education","Purpose")))
df_questions_analysis$x <- NULL

#Treatment DataBase * Applications *

df_applications$submitted_it <- ifelse(df_applications$submitted_at=="","0","1")
df_applications <- left_join(df_applications,df_questions_educ %>% 
                               select(applicant_id,response),
                             by = c("applicant_id" = "applicant_id"))
df_applications$response[df_applications$response==""] <- "No Response"
df_applications$response[is.na(df_applications$response)] <- "No Response"
df_applications <- left_join(df_applications,df_questions_analysis %>% 
                               select(Group.1,TypeofApplication),
                             by = c("applicant_id" = "Group.1"))

df_applications <- left_join(df_applications,df_intervals %>% 
                               select(from,interval),
                             by = c("submitted_at" = "from"))
df_applications$interval[is.na(df_applications$interval)] <- "No Response"

df_applications$TypeofApplication[is.na(df_applications$TypeofApplication)] <- "No Response"

df_applications$DateDiff <- as.integer(difftime(as.Date(df_applications$submitted_at),
                                                as.Date(df_applications$application_created_at),
                                                units = "days"))
df_applications$DateDiff[is.na(df_applications$DateDiff)] <- as.integer(1)
df_applications$DateDiff <- ifelse(df_applications$DateDiff==as.integer(0),as.integer(1),df_applications$DateDiff)
df_applications <- merge(df_applications,df_week,by.x = "DateDiff",by.y = "From",all.x = TRUE)
df_applications$DateDiff <- NULL

# Question 1: How Many Applicant submitted their application? How Many Didnt?

table(df_applications$submitted_it)
prop.table(table(df_applications$submitted_it))

Q1Plot <- barplot(table(df_applications$submitted_it),
                  main = "Analysis of Successful Submitted",
                  ylab = "Total of Applications",
                  col = c("darkred","darkgreen"),
                  names.arg=c("No","Yes"),
                  border = NA)

text(x = Q1Plot, y= table(df_applications$submitted_it), label = table(df_applications$submitted_it),pos = 3, xpd=NA)

# Question 2: What’s the frequency distribution of applicants by different

Q2Plot <- barplot(prop.table(table(df_applications$TypeofApplication)),
                  space=F,
                  main="Barplot of Category Distribution",
                  ylab = "Frequency",
                  border = NA)

text(x = Q2Plot, y= prop.table(table(df_applications$TypeofApplication)), label = round(prop.table(table(df_applications$TypeofApplication)),3),pos = 3, xpd=NA)

#Question 4: HeatMap

df_heatmap1<-subset(df_applications,submitted_at!="")
df_heatmap1<-df_heatmap1[,-c(1,3,4,5,6,7)]
df_heatmap1<-as.data.frame.matrix(prop.table(table(df_heatmap1)))

my_colours <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

Q3Plot = heatmap(as.matrix(df_heatmap1),
                 scale="column",
                 col=my_colours,
                 main="HeatMap Applications per Week",
                 Rowv = NA,
                 Colv = NA)

#Question 4: HeatMap

df_heatmap2<-subset(df_applications,submitted_at!="")
df_heatmap2<-df_heatmap2[,-c(1,2,3,4,5,6)]
df_heatmap2<-as.data.frame.matrix(prop.table(table(df_heatmap2)))

my_colours <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

Q3Plot = heatmap(as.matrix(df_heatmap2),
                 scale="column",
                 col=my_colours,
                 main="HeatMap Applications per Week",
                 Rowv = NA,
                 Colv = NA)

write.csv(df_applications,"dfitems.csv",row.names = FALSE)