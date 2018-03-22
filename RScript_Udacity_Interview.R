
file_applications = "https://raw.githubusercontent.com/josecypriano/Interview_Udacity/master/applications.csv"
file_questions = "https://raw.githubusercontent.com/josecypriano/Interview_Udacity/master/questions.csv"

df_applications = read.table(file_applications,header = TRUE,sep=",")
df_questions = read.table(file_questions,header = TRUE,sep=",")

df_applications$submitted_it = ifelse(df_applications$submitted_at=="","0","1")

df_questions$responde_it = ifelse(df_questions$response=="","0","1")

# Question 1: How Many Applicant submitted their application? How Many Didnt?

table(df_applications$submitted_it)
prop.table(table(df_applications$submitted_it))

Q1Plot = barplot(table(df_applications$submitted_it),
                 main = "Number of Applications Submitted",
                 ylab = "Total Applications",
                 col = c("darkred","lightblue"),
                 names.arg=c("No","Yes")
)
text(x = Q1Plot, y= table(df_applications$submitted_it), label = table(df_applications$submitted_it),pos = 3, xpd=NA)
