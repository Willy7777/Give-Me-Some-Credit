install.packages('rpart',repos='http://cran.us.r-project.org')
install.packages('ROCR',repos='http://cran.us.r-project.org')

library('rpart')
library('ROCR')

#calculus auc
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}


auc_num<- function(train,test) {
  
    # model using decision tree
    model <- rpart(delinquency_Formula,
                 data=train,control=rpart.control(cp = 0.001,minsplit = 1000,
                                                             minbucket = 1000,
                                                             maxdepth=5))
    
    auc_out <- calcAUC(predict(model,newdata=test),test[,outcome])
  
    return(auc_out)
}
#combine train and validation function
new_training_set <- function(i) {
  if(i != fold_num) {
    
    trainings_set <- subset(tr_data , subset = (tr_data$gp < (i-1) / fold_num | tr_data$gp > (i+1) / fold_num))
    validation_set <- subset(tr_data ,subset = (i / fold_num < tr_data$gp & tr_data$gp <= (i+1) / fold_num ))
    
    new_trainings_set <- rbind(trainings_set,validation_set)
    return(new_trainings_set)
  }else {
    
    trainings_set <- subset(tr_data , subset = (1 /fold_num <tr_data$gp & tr_data$gp < (i-1) / fold_num))
    validation_set <- subset(tr_data , subset = tr_data$gp <= 1 / fold_num)
    
    new_trainings_set <- rbind(trainings_set,validation_set)
    return(new_trainings_set)
  }
}

# parse parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript DecisionTree.R --fold k --input file --output out.csv", call.=FALSE)
}
i<-1 
while(i < length(args))
{
  if(args[i] == "--fold"){
    fold_num<-as.numeric(args[i+1])
    i<-i+1
  }else if(args[i] == "--train") {
    train_path <- args[i+1]
    i <- i+1
  }else if(args[i] == "--test"){
    test_path <- args[i+1]
    i <- i+1
  }else if(args[i] == "--report"){
    report_path <- args[i+1]
    i <- i+1
  }else if(args[i] == "--predict"){
    predict_path <- args[i+1]
    i <- i+1
  }
  else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("fold :", fold_num))
print(paste("performance file:", report_path))
print(paste("predict file:", predict_path))

# read input data
if(file.exists(file1 = train_path)) {
  tr_data <- read.table(train_path , header = T, sep = ",")
  te_data <- read.table(test_path , header = T, sep = ",")
}else {
  stop(paste("file doesn't exist : " , basename(train_path)), call. = FALSE)
}
#Data process
outcome <- "SeriousDlqin2yrs"
pos <- 1
set.seed(123)
tr_data <- data.frame(tr_data , gp = runif(dim(tr_data)[1]) )
colnames(tr_data) <- gsub("X","ID",colnames(tr_data))
colnames(te_data) <- gsub("X","ID",colnames(te_data))

delinquencyVars <- setdiff(colnames(tr_data),list('gp','SeriousDlqin2yrs'))

delinquency_Formula <- as.formula(paste(outcome,"==1 ~ ",paste(delinquencyVars,collapse=' + '),sep=''))



#three-way-split
auc_test_sets <- c()
auc_validation_sets <- c()
auc_training_sets <- c()
name <- c()


#select model with training_set and validation_set
for (i in 1:fold_num) {
  
  if (i != fold_num) {
    
    #training_data
    trainings_set <- subset(tr_data , subset = (tr_data$gp < (i-1) / fold_num | tr_data$gp > (i+1) / fold_num))
    
    #validation_data
    validation_set <- subset(tr_data ,subset = (i / fold_num < tr_data$gp & tr_data$gp <= (i+1) / fold_num ))
    
    
    #test_data
    test_set <- subset(tr_data , subset = ((i-1)/ fold_num <= tr_data$gp & tr_data$gp <= i / fold_num))
    
    
    auc_training_sets <- c(auc_training_sets , round(auc_num(trainings_set,trainings_set),digits = 2))
    auc_validation_sets <- c(auc_validation_sets , round(auc_num(trainings_set,validation_set),digits = 2))
    auc_test_sets <- c(auc_test_sets , round(auc_num(new_training_set(i),test_set),digits = 2))
    
    name <- c(name , paste("fold" , as.character(i) ,sep = ""))
  }else {
    
    #training_data
    trainings_set <- subset(tr_data , subset = (1 /fold_num <tr_data$gp & tr_data$gp < (i-1) / fold_num))
    
    #validation_data
    validation_set <- subset(tr_data , subset = tr_data$gp <= 1 / fold_num)
    
    
    #test_data
    test_set <- subset(tr_data , subset = ((i-1)/ fold_num <= tr_data$gp & tr_data$gp <= i / fold_num))
    
    auc_training_sets <- c(auc_training_sets , round(auc_num(trainings_set,trainings_set),digits = 2))
    auc_validation_sets <- c(auc_validation_sets , round(auc_num(trainings_set,validation_set),digits = 2))
    auc_test_sets <- c(auc_test_sets , round(auc_num(new_training_set(i),test_set),digits = 2))
    
    name <- c(name , paste("fold" , as.character(i) ,sep = ""))
  }
  
}
#find max_validation_accurcy_fold
index<-which.max(auc_test_sets)

print(paste("k = ",index,"with the highest test AUC in k-fold cross-validation"))
#predcit Survived for best_model
new_data <- new_training_set(index)


best_model <- rpart(delinquency_Formula,
                    data=new_data,control=rpart.control(cp = 0.001,minsplit = 1000,
                                                        minbucket = 1000,
                                                        maxdepth=5))

#mean k-fold_accuracy
auc_test_sets <- c(auc_test_sets , round(mean(auc_test_sets),digits = 2))
auc_validation_sets <- c(auc_validation_sets , round(mean(auc_validation_sets),digits = 2))
auc_training_sets <- c(auc_training_sets , round(mean(auc_training_sets),digits = 2))
#output data
out_data <- data.frame(set = c(name , "ave."),training = auc_training_sets ,
                       validation = auc_validation_sets, test = auc_test_sets,stringsAsFactors = F)

pred_data <- data.frame(Id = te_data$ID , Probability = predict(best_model, newdata = te_data))
                                                                

#output file
outputname <- strsplit(report_path,"/",fixed = TRUE)[[1]]

output_pred_name <- strsplit(predict_path, "/",fixed = TRUE)[[1]]

folder_name <- outputname[c(1:length(outputname) - 1)]
folder_name <- paste(folder_name,collapse = "/")

folder_pred_name <- output_pred_name[c(1:length(output_pred_name) - 1)]
folder_pred_name <- paste(folder_pred_name,collapse = "/")
#Check in R if a Directory Exists and Create if It doesn¡¦t
#does not crash if the directory already exists, it just prints out a warning. 
dir.create(folder_name,showWarnings = FALSE)

dir.create(folder_pred_name,showWarnings = FALSE)

#write outfile
write.table(out_data, file=report_path, row.names = F, sep = ",",quote = F)
write.table(pred_data, file=predict_path, row.names = F, sep = ",",quote = F)