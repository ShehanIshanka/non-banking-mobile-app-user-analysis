# Read dataset
df <- read.csv("dataset/data.csv")
col_names <- t(t(colnames(df)))

for (col in 1:5) {
  barplot(table(df[col]), xlab=col_names[col], ylab="Count")
}