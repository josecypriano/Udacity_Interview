install.packages("dplyr")
library(dplyr)

#Upload Data Frames

df_applications = read.table("https://raw.githubusercontent.com/josecypriano/Udacity_Interview/master/applications.csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
df_questions = read.table("https://raw.githubusercontent.com/josecypriano/Udacity_Interview/master/questions.csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
df_questionscategory = data.frame("Id_Question"=c(1,2,3), "Question2"=c("Education","Purpose","No Response"))

df_questions_educ = subset(df_questions,question=="education")
df_questions_educ$question = NULL


#Treatment Data Analysis * Applications *

df_applications$submitted_it = ifelse(df_applications$submitted_at=="","0","1")

df_applications = left_join(df_applications,df_questions_educ %>% 
                              select(applicant_id,response),
                            by = c("applicant_id" = "applicant_id"))

df_applications$response[df_applications$response==""] = "No Response"

#Treatment Data Analysis * Questions *

df_questions$question2 = ifelse(df_questions$response!="",df_questions$question,"No Response")
df_questions2 = subset(df_questions,question2!="No Response")

df_questions2 = table(df_questions2)

df_questions2

head(df_questions2)
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
