install.packages("dplyr")
install.packages("plyr")
library(plyr)
library(tidyr)
library(rio)
library(dplyr)
library(moments)


abd<-read.csv("C:/Users/priya/OneDrive/Documents/Abandoned.csv", header=T, na.strings="")
abd
rs<-read.csv("C:/Users/priya/OneDrive/Documents/Reservation.csv", header=T, na.strings="")

#matching the email column
match_email=abd$Email[complete.cases(abd$Email)] %in% rs$Email[complete.cases(rs$Email)] 
table(match_email)
#matching the incoming phone column
match_incoming=abd$Incoming_Phone[complete.cases(abd$Incoming_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)] 
table(match_incoming)

#matching the contact number column
match_contact=abd$Contact_Phone[complete.cases(abd$Contact_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)] 
table(match_contact)
#matching the abd$incoming to rs$contact number
match_incoming_contact= abd$Incoming_Phone[complete.cases(abd$Incoming_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)] 
table(match_incoming_contact)

#matching the abd$contactnumber to rs$incoming
match_contact_incoming= abd$Contact_Phone[complete.cases(abd$Contact_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)] 
table(match_contact_incoming)

# flags for matching email column
abd$match_email<-0
abd$match_email[complete.cases(abd$Email)] <- 1* match_email
sum(abd$match_email)  #75 matches

#creating flags for matching incoming number column
abd$match_incoming<-0

abd$match_incoming[complete.cases(abd$Incoming_Phone)] <- 1* match_incoming
sum(abd$match_incoming)  

#creating flags for matching incoming number column
abd$match_contact<-0
abd$match_contact[complete.cases(abd$Contact_Phone)] <- 1* match_contact
sum(abd$match_contact)  

#creating flags for matching incoming to contact column
abd$match_incoming_contact<-0
abd$match_incoming_contact[complete.cases(abd$Incoming_Phone)] <- 1* match_incoming_contact
sum(abd$match_incoming_contact)  

#creating flags for matching contact to incoming column
abd$match_contact_incoming<-0
abd$match_contact_incoming[complete.cases(abd$Contact_Phone)] <- 1* match_contact_incoming
sum(abd$match_contact_incoming)  

#creating a column if the customer bought or not
abd$pur <-0
abd$pur <- 1*(abd$match_email|abd$match_incoming|abd$match_contact|abd$match_incoming_contact|abd$match_contact_incoming)
sum(abd$pur)

#creating a treatment column with 0 and 1 where 1== test and 0 == control

abd$mytest <- abd$Test_Control == 'test'
abd$mytest<-1*abd$mytest
abd$mytest


#Customers in the control who bought
#So the value of treat should be 0 and pur column should be 1
control_buy= nrow(abd[abd$pur==1 & abd$my_test==0])
control_buy

#Customers in the control group who did not buy  
#in this case the value of treatment = 0 and converted should be 0
control_notbuy=nrow(abd[abd$pur==0 & abd$my_test==0])
control_notbuy

#Customers in the treatment group who bought  
#in this case the value of treatment = 1 and converted should be 1
treat_buy=nrow(abd[abd$pur==1 & abd$my_test==1])
treat_buy
#Customers in the treatment group who did not buy  
#in this case the value of treatment = 1 and converted should be 0
treat_notbuy=nrow(abd[abd$pur==0 & abd$my_test==1])
treat_notbuy

# create a table of buy/no buy and treatment/control
data1=c(treat_buy,treat_notbuy,control_buy,control_notbuy)
data= matrix(data1, ncol=2,nrow=2, byrow=TRUE)

# specify the column names and row names of matrix
colnames(data) = c('Buy','No Buy')
rownames(data) <- c('Treament','Control')

#making a cross tabulation to show the number of customers 
#who bought or not in the treat and control groups
cross_tabulation=as.table(data)
cross_tabulation

#Function to make cross-tabulation for 5 different states
table_state=function(state)
{
  
  #making subset of abandoned data frame 
  abd_subset_state= subset(abd, abd$Address==state)
  abd_subset=select(abd_subset_state,my_test,pur)
  
  #no. of customers in control and buy
  cust_c_buy= nrow(abd_subset[abd_subset$pur==1 & abd_subset$my_test==0])
  
  #no. of customers in control and didn't buy
  cust_c_nbuy= nrow(abd_subset[abd_subset$pur==0 & abd_subset$my_test==0])
  
  #no. of customers in treatment and buy
  cust_t_buy= nrow(abd_subset[abd_subset$pur==1 & abd_subset$my_test==1])
  
  #no. of customers treatment and didn't buy
  cust_t_nbuy= nrow(abd_subset[abd_subset$pur==0 & abd_subset$my_test==1])
  
  # create a table of buy/no buy and treatment/control
  data2=c(ncust_t_buy,ncust_t_nbuy,ncust_c_buy,ncust_c_nbuy)
  data3= matrix(data2, ncol=2,nrow=2, byrow=TRUE)
  
  # specify the column names and row names of matrix
  colnames(data3) = c('Buy','No Buy')
  rownames(data3) <- c('Treament','Control')
  
  #making a cross tabulation to show the number of customers 
  #who bought or not in the treat and control groups
  cross_tabulation_state=as.table(data3)
  print(cross_tabulation_state)
}

#passing 5 random states as argument in the function to generate tables
table_FL=table_state("FL")
table_NY=table_state("NY")
table_CA=table_state("CA")
table_OH=table_state("OH")
table_NE=table_state("NE")

#creating a csv file for data cleansing
new_data=subset(abd, abd$pur == 1, select = c("Caller_id","Test_control","pur","Address","Email"))
write.csv(cleansed_data,"C:\\Users\\risha\\OneDrive\\Desktop\\qmb\\midterm project\\cleansed_data.csv",row.names = FALSE)


#creating email and address column for analyses
abandoned$email=1*complete.cases(abandoned$email)
abandoned$address=1*complete.cases(abandoned$address)

#linear regression on abandoned data set
linear_regression_result=lm(converted~treatment,data=abandoned)
summary(linear_regression_result)


#t_test on abandoned data set
t_test_result=t.test(abandoned$converted,abandoned$treatment,paired = "FALSE")
t_test_result

#multiple regression on abandoned data set
mul_reg_result=lm(converted~treatment+email+address, data=abandoned)
summary(mul_reg_result)

#regression based on interactions with treatment of email and address
regression_int_result= lm(converted~treatment*email+treatment*address, data= abandoned)
summary(regression_int_result)


#summary of test/control variable
summary(abandoned$treatment)


#subset of abandoned where the states are given
#and finding summary of test variable
states_given= subset(abandoned,abandoned$address!="",select=c("treatment","address"))
states_given
summary(states_given$treatment)



my_data<-subset(abd,abd$pur==1)
my_data
my_data$D_Email<- ifelse(new_df$Email != 'NA',1,0)
my_data["D_Email"][is.na(new_df["D_Email"])] <- 0

my_data$D_State<- ifelse(new_df$Address != 'NA',1,0)
my_data["D_State"][is.na(new_df["D_State"])] <- 0

my_file<- data.frame(
  Customer_ID=new_df$Caller_ID,
  Test_Variable =new_df$Test_Control,
  Outcome = new_df$pur,
  D_Email = new_df$D_Email,
  D_State = new_df$D_State
)
my_file
write.csv(my_file,"C:/Users/priya/OneDrive/Desktop/My_Excel.csv")

my_model<-lm(abd$pur~abd$Test_Control, data = abd)
summary(my_model)

my_anova=aov(abd$pur ~ abd$Test_Control, data = abd)
summary(my_anova)

new_model<- lm(abd$pur ~ abd$Test_Control + abd$Email+ abd$Address, data = abd)
summary(new_model)

test_group<-subset(abd,abd$Test_Control=="test")

match_email=test_group$Email[complete.cases(test_group$Email)] %in% rs$Email[complete.cases(rs$Email)]

match_incoming=test_group$Incoming_Phone[complete.cases(test_group$Incoming_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]

match_contact=test_group$Contact_Phone[complete.cases(test_group$Contact_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)]

match_incoming_contact= test_group$Incoming_Phone[complete.cases(test_group$Incoming_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)] 

match_contact_incoming= test_group$Contact_Phone[complete.cases(test_group$Contact_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]

test_group$match_email <-0
test_group$match_email[complete.cases(test_group$Email)] <- 1* match_email



control<-subset(abd,abd$Test_Control=="control")

match_email=control$Email[complete.cases(control$Email)] %in% rs$Email[complete.cases(rs$Email)]

match_incoming=control$Incoming_Phone[complete.cases(control$Incoming_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]

match_contact=control$Contact_Phone[complete.cases(control$Contact_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)]

match_incoming_contact= control$Incoming_Phone[complete.cases(control$Incoming_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)] 

match_contact_incoming= control$Contact_Phone[complete.cases(control$Contact_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]

control$match_email <-0
control$match_email[complete.cases(control$Email)] <- 1* match_email

control$match_incoming <- 0
control$match_incoming[complete.cases(control$Incoming_Phone)] <- 1* match_incoming

control$match_contact <- 0
control$match_contact[complete.cases(control$Contact_Phone)] <- 1* match_contact

control$match_incoming_contact <- 0
control$match_incoming_contact[complete.cases(control$Incoming_Phone)] <- 1* match_incoming_contact

control$match_contact_incoming <- 0
control$match_contact_incoming[complete.cases(control$Contact_Phone)] <- 1* match_contact_incoming


control$pur<-0
control$pur <- 1*(control$match_contact | control$match_contact_incoming | control$match_email | control$match_incoming_contact | control$match_incoming)
sum(control$pur)


test_group$match_incoming <- 0
test_group$match_incoming[complete.cases(test_group$Incoming_Phone)] <- 1* match_incoming

test_group$match_contact <- 0
test_group$match_contact[complete.cases(test_group$Contact_Phone)] <- 1* match_contact

test_group$match_incoming_contact <- 0
test_group$match_incoming_contact[complete.cases(test_group$Incoming_Phone)] <- 1* match_incoming_contact

test_group$match_contact_incoming <- 0
test_group$match_contact_incoming[complete.cases(test_group$Contact_Phone)] <- 1* match_contact_incoming


test_group$pur<-0
test_group$pur <- 1*(test_group$match_contact | test_group$match_contact_incoming | test_group$match_email | test_group$match_incoming_contact | test_group$match_incoming)
sum(test_group$pur)


control<-subset(abd,abd$Test_Control=="control")

match_email=control$Email[complete.cases(control$Email)] %in% rs$Email[complete.cases(rs$Email)]

match_incoming=control$Incoming_Phone[complete.cases(control$Incoming_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]

match_contact=control$Contact_Phone[complete.cases(control$Contact_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)]

match_incoming_contact= control$Incoming_Phone[complete.cases(control$Incoming_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)] 

match_contact_incoming= control$Contact_Phone[complete.cases(control$Contact_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]

control$match_email <-0
control$match_email[complete.cases(control$Email)] <- 1* match_email

control$match_incoming <- 0
control$match_incoming[complete.cases(control$Incoming_Phone)] <- 1* match_incoming

control$match_contact <- 0
control$match_contact[complete.cases(control$Contact_Phone)] <- 1* match_contact

control$match_incoming_contact <- 0
control$match_incoming_contact[complete.cases(control$Incoming_Phone)] <- 1* match_incoming_contact

control$match_contact_incoming <- 0
control$match_contact_incoming[complete.cases(control$Contact_Phone)] <- 1* match_contact_incoming


control$pur<-0
control$pur <- 1*(control$match_contact | control$match_contact_incoming | control$match_email | control$match_incoming_contact | control$match_incoming)
sum(control$pur)
state_t1 = subset(test_group,test_group$Address=="MN")
nrow(state_t1)

state_c1 = subset(control,control$Address=="MN")
nrow(state_c1)

state_t2<-subset(test_group,test_group$Address=="NY")
nrow(state_t2)

state_c2 = subset(control,control$Address=="NY")
nrow(state_c2)


state_t3<-subset(test_group,test_group$Address=="LA")
nrow(state_t3)

state_c3 = subset(control,control$Address=="LA")
nrow(state_c3)

state_t4<-subset(test_group,test_group$Address=="VA")
nrow(state_t4)

state_c4 = subset(control,control$Address=="VA")
nrow(state_c4)

state_t5<-subset(test_group,test_group$Address=="ND")
nrow(state_t5)

state_c5 = subset(control,control$Address=="ND")
nrow(state_c5)
sum(state_t1$pur)
sum(state_c1$pur)
sum(state_t2$pur)
sum(state_c2$pur)
sum(state_t3$pur)
sum(state_c3$pur)
sum(state_t4$pur)
sum(state_c4$pur)
sum(state_t5$pur)
abd$email<-1*complete.cases(abd$Email)
abd$state<-1*complete.cases(abd$Address)
abd$treat<-1*(abd$Test_Control=="test")

sum(statmulti_linear_model <- lm(pur~Test_Control * email +Test_Control * state, data=abd)e_c5$pur)
my_model<-lm(abd$pur~abd$Test_Control+abd$email+abd$state,data = abd)
summary(my_model)
my_model_2 <- lm(abd$pur~abd$Test_Control * abd$email +abd$Test_Control * abd$state, data=abd)
summary(my_model_2)

my_model_3<-lm(abd$pur~abd$Test_Control * abd$email +abd$Test_Control * abd$state+abd$Test_Control*abd$treat, data=abd)
summary(my_model_3)
install.packages("stargazer")
library(stargazer)
stargazer(my_model,my_model_2,my_model_3,type = "html",out="midterm.htm")
abd[abd==""] <- NA
address <- abd[complete.cases(abd['Address']),]
table(address$Test_Control)
