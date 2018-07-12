#mydata1 = read.csv("/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/bus_1-7_1_2010.csv", header=T)
#mydata2 = read.csv("/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/bus_8-15_1_2010.csv", header=T)

 n = c(2, 3, 5) 
 s = c("aa", "bb", "cc") 
 b = c(TRUE, FALSE, TRUE) 
 df = data.frame(n, s, b)
 
 n = c(3, 3, 3) 
 s = c("3", "3", "3") 
 b = c(FALSE, FALSE, FALSE) 
 df2 = data.frame(n, s, b)
myfulldata = rbind(df, df2)