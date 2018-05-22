#funtion to do with muliple choice
multiplechoice<-function(data,attribute,len){
  #data=数据集
  #attribute=代表多选题的变量名
  #length代表多选题一共的选项数
  #分隔符为","
  #attribute<-as.character(attribute)
  
  lev<-as.character(1:len)
  mnames <- gsub(" ", "_", paste(attribute, lev, sep = "_"))#创建每一个选项的二分变量名
  result <- matrix(data = F, nrow = length(data[[attribute]]), ncol = len)#创建一个空白的结果表格
  
  char <- data[[attribute]]
  char <- as.character(char)
  for (i in 1:len) {
    result[grep(lev[i], char,fixed = TRUE),i] <- T
    #grep 用于返回在char里存在lev[i]的被试编号
  }
  result <- data.frame(result, stringsAsFactors = TRUE)#转化为frame格式
  colnames(result) <- mnames
  data<-cbind(data,result)
  data
}