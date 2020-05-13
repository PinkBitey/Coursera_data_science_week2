# Mad Libs Function
mad_libs <- function(...){
  args<-list(...)
  place<-args[["place"]]
  adjective<-args[["adjective"]]
  noun<-args[["noun"]]
  paste("News from", place, "today where", adjective, "students took to the streets in protest of the new", noun, "being installed on campus.")
}

# Finding the pollutant mean in specdata

pollutantmean<-function(directory,pollutant,id=1:332) {
  my_files<-list.files(path=directory, pattern="*.csv")
  my_data<-lapply(my_files[id],read.csv)
  my_df<-do.call("rbind", my_data)
  mean(my_df[[pollutant]],na.rm=TRUE)
}

# My working directory
"/Users/jaq/Desktop/OneDrive/Coursera/R Programming/specdata"

# Return number of completely observed cases in each data file

## StackOverflow answer 

complete<-function(directory,id=1:332) {
  count_complete <- function(fname) sum(complete.cases(read.csv(fname)))
  fnames <- list.files(directory, full.names=TRUE)[id]
  data.frame(id = id, complete = unlist(lapply(fnames, count_complete)))
}

## My answer

complete<-function(directory,id=1:332) {
  my_files<-list.files(path=directory, pattern="*.csv")
  my_data<-lapply(my_files[id],read.csv)
  count_complete<-lapply(my_data,complete.cases)
  sum_count_complete<-lapply(count_complete,sum)
  data.frame(id=id,nobs=unlist(sum_count_complete))
}

# Correlation between nitrate and sulfate for completely observed cases over threshold

corr<-function(directory,threshold=0) {
  my_files<-list.files(path=directory, pattern="*.csv")
  my_data<-lapply(my_files,read.csv)
  count_complete<-lapply(my_data,complete.cases)
  sum_count_complete<-lapply(count_complete,sum)
  my_df<-data.frame(id=my_files,nobs=unlist(sum_count_complete))
  my_df2<-my_df[which(my_df$nobs > threshold),]
  my_files2<-my_df2["id"]
  my_files3<-my_files[unlist(my_files2)]
  my_data2<-lapply(my_files3,read.csv)
  my_data3<-lapply(my_data2, function(x) {
    x[complete.cases(x),]} )
  my_corr<-lapply(my_data3, function(x) {
    cor(x["sulfate"],x["nitrate"])})
  unlist(my_corr)
}


my_df2<-my_df[nobs>threshold]
my_files2<-my_data[lapply(my_data,complete.cases)>threshold]
lapply(my_data2,subset)

}