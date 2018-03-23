
install.packages("dplyr")
library(dplyr)


#Upload Data Frames

df_applications = read.table("https://raw.githubusercontent.com/josecypriano/Udacity_Interview/master/applications.csv",header = TRUE,sep=",")
df_questions = read.table("https://raw.githubusercontent.com/josecypriano/Udacity_Interview/master/questions.csv",header = TRUE,sep=",")
df_questionscategory = data.frame("Id_Question"=c(1,2,3), "Question2"=c("Education","Purpose","No Response"))


df_questions_educ = subset(df_questions,question=="education")
df_questions_educ$question = NULL

df_applications
df_applications$submitted_it = ifelse(df_applications$submitted_at=="","0","1")


df_questions$Id_Question = ifelse(df_questions$response!="",df_questions$question,"3")
df_questions = merge(x=df_questions,y=df_questionscategory,by="Id_Question",all.x = TRUE)
df_questions$Id_Question = NULL
  
# Question 1: How Many Applicant submitted their application? How Many Didnt?

table(df_applications$submitted_it)
prop.table(table(df_applications$submitted_it))

Q1Plot = barplot(table(df_applications$submitted_it),
                 main = "Analysis of Successful Submitted",
                 ylab = "Total of Applications",
                 col = c("darkred","darkgreen"),
                 names.arg=c("No","Yes")
)
text(x = Q1Plot, y= table(df_applications$submitted_it), label = table(df_applications$submitted_it),pos = 3, xpd=NA)







write.csv(df_applications,"dfitems.csv",row.names = FALSE)
