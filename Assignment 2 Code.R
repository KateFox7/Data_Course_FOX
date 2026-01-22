## 4) Write a command that lists all of the .csv files found in the Data/ directory to see a bunch of data files
getwd()
csv_files <- list.files("C:/Users/kateh/Data_Course_FOX/Data_Course/Data", pattern = ".csv", recursive = FALSE)
csv_files

## 5) Find out how many files match that description using the length() function
length(csv_files)


## 6) Open the wingspan_vs_mass.csv file and store the contents as an R object named "df" using the read.csv() function
df <- read.csv(file.path("C:/Users/kateh/Data_Course_FOX/Data_Course/Data", "wingspan_vs_mass.csv"))
df

## 7) Inspect the first 5 lines of this data set using the head() function
head(df, 5)

## 8) Find any files(recursively) in the Data/ directory that begin with the letter "b" (lowercase)
b_files <- list.files("C:/Users/kateh/Data_Course_FOX/Data_Course/Data", pattern = "^b", recursive = TRUE)
b_files

## 9) Write a command that displays the first line of each of those "b" files (this is tricky... use a for-loop)
for (f in b_files) {
  cat("First line of", f, ":\n")
  con <- file(file.path("C:/Users/kateh/Data_Course_FOX/Data_Course/Data", f))
  first <- readLines(con, n = 1)
  print(first)
  close(con)
}

## 10) Do the same thing for all files that end in ".csv"
for (f in csv_files) {
  cat("First line of", f, ":\n")
  con <- file(file.path("C:/Users/kateh/Data_Course_FOX/Data_Course/Data", f))
  first <- readLines(con, n = 1)
  print(first)
  close(con)
}
