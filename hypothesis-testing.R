proportionfun <- function(raw_sample){
  sum(raw_sample)/length(raw_sample)
}

bootstrapfun <- function(raw_sample, sampling_frequency, confidence){
  bootsrap_distribution <- lapply(1:sampling_frequency, function(i) 
    sample(raw_sample, replace = T))
  bootsrap_prop_dist <- sapply(bootsrap_distribution, proportionfun)
  bootstrap_std <- sqrt(var(bootsrap_prop_dist))
  hist(bootsrap_prop_dist)
  bootstrap_std <- sqrt(var(bootsrap_prop_dist))
  bootstrap_mean <- mean(bootsrap_prop_dist)
  error <- qnorm(confidence)*bootstrap_std/sqrt(sampling_frequency)
  left <- bootstrap_mean-error
  right <- bootstrap_mean+error
  return(list(left,right))
}

singleProportionFun <- function(nh,p,n,test){
  z <- (p - nh)/sqrt(nh*(1-nh)/n)
  if (test == "one_tail") return (pnorm(z))
  if (test == "two_tail") return (2*pnorm(z))
}

diffProportionFun <- function(x1,x2,x3,x4,test){
  p_hat <- (x1+x2)/(x1+x2+x3+x4)
  p1 <- x1/(x1+x3)
  p2 <- x2/(x2+x4)
  z <- (p1 - p2)/sqrt(p_hat*(1-p_hat)/(x1+x3) + p_hat*(1-p_hat)/(x2+x4))
  if (test == "one_tail") return (pnorm(z))
  if (test == "two_tail") return (2*pnorm(z))
}

associationTestFun <- function(dataList,n){
  expectedList <- list()
  chi <- 0
  i <- 1
  while(i < length(dataList)+1){
    rowTotal <- sum(dataList[[i]]) 
    j <- 1
    l <- list()
    while(j < (length(dataList[[i]])+1)){
      colTotal <- 0
      m <- 1
      while(m < length(dataList)+1){
        colTotal <- colTotal + dataList[[m]][j]
        m <- m + 1
      }
      expected <- rowTotal*colTotal/n
      x <- (dataList[[i]][j] - expected)**2/expected
      chi <- chi + x
      l[[j]] <- expected
      j <- j + 1
    }
    expectedList[[i]] <- l
    i <- i + 1
  }
  df <- (length(dataList)-1)*(length(dataList[[1]])-1)
  print(chi)
  print(expectedList)
  return (1 - pchisq(chi,df))
}


df <- read.csv("dataset/data.csv")
col_names <- t(t(colnames(df)))

###########################################################################

# Most of the people have heard of mobile banking applications
x <- 0 + (df$Have.you.heard.of.banking.mobile.apps.before.==" Yes")
bootstrapfun(c(x),10000,0.95)

###########################################################################

# Most of the people have earlier used banking mobile apps.
people_earlier_used_banking_app <- nrow(df[(df$Have.you.earlier.used.banking.mobile.apps.before.==" Yes"),])
people_earlier_not_used_banking_app <- nrow(df[(df$Have.you.earlier.used.banking.mobile.apps.before.==" No"),])
tot <- people_earlier_used_banking_app + people_earlier_not_used_banking_app
singleProportionFun(0.5,people_earlier_not_used_banking_app/tot,tot,"one_tail")

# Most of the males have have earlier used banking mobile apps.
males_used_mobile_banking_apps <- nrow(df[(df$gender ==" Male")  & (df$Have.you.earlier.used.banking.mobile.apps.before.==" Yes"),])
males_not_used_mobile_banking_apps <- nrow(df[(df$gender ==" Male")  & (df$Have.you.earlier.used.banking.mobile.apps.before.==" No"),])
females_used_mobile_banking_apps <- nrow(df[(df$gender ==" Female")  & (df$Have.you.earlier.used.banking.mobile.apps.before.==" Yes"),])
females_not_used_mobile_banking_apps <- nrow(df[(df$gender==" Female")  & (df$Have.you.earlier.used.banking.mobile.apps.before.==" No"),])
#diffProportionFun(males_used_mobile_banking_apps,
#                  females_used_mobile_banking_apps,
#                  males_not_used_mobile_banking_apps,
#                  females_not_used_mobile_banking_apps,
#                  "one_tail")

# Majority of private sector employees have earlier used banking mobile apps.
private_sector_employees_used_mobile_banking_apps <- nrow(df[(df$occupation =="  Private Sector")  & (df$Have.you.earlier.used.banking.mobile.apps.before.==" Yes"),])
private_sector_employees_not_used_mobile_banking_apps <- nrow(df[(df$occupation =="  Private Sector")  & (df$Have.you.earlier.used.banking.mobile.apps.before.==" No"),])
rest_of_employees_used_mobile_banking_apps <- nrow(df[(df$occupation !="  Private Sector")  & (df$Have.you.earlier.used.banking.mobile.apps.before.==" Yes"),])
rest_of_employees_not_used_mobile_banking_apps <- nrow(df[(df$occupation !="  Private Sector")  & (df$Have.you.earlier.used.banking.mobile.apps.before.==" No"),])
#diffProportionFun(private_sector_employees_used_mobile_banking_apps,
#                  rest_of_employees_used_mobile_banking_apps,
#                  private_sector_employees_not_used_mobile_banking_apps,
#                  rest_of_employees_not_used_mobile_banking_apps,
#                  "one_tail")

#Association between earlier usage of banking mobile apps and education level
degree_people_earlier_used_banking_app <- nrow(df[(df$educational_qualification==" Degree") & (df$Have.you.earlier.used.banking.mobile.apps.before.==" Yes"),])
degree_people_earlier_not_used_banking_app <- nrow(df[(df$educational_qualification==" Degree") & (df$Have.you.earlier.used.banking.mobile.apps.before.==" No"),])
diploma_people_earlier_used_banking_app <- nrow(df[(df$educational_qualification==" Diploma") & (df$Have.you.earlier.used.banking.mobile.apps.before.==" Yes"),])
diploma_people_earlier_not_used_banking_app <- nrow(df[(df$educational_qualification==" Diploma") & (df$Have.you.earlier.used.banking.mobile.apps.before.==" No"),])
al_people_earlier_used_banking_app <- nrow(df[(df$educational_qualification=="  A/L") & (df$Have.you.earlier.used.banking.mobile.apps.before.==" Yes"),])
al_people_earlier_not_used_banking_app <- nrow(df[(df$educational_qualification=="  A/L") & (df$Have.you.earlier.used.banking.mobile.apps.before.==" No"),])
data <- list(c(degree_people_earlier_used_banking_app,
               diploma_people_earlier_used_banking_app,
               al_people_earlier_used_banking_app),
             c(degree_people_earlier_not_used_banking_app,
               diploma_people_earlier_not_used_banking_app,
               al_people_earlier_not_used_banking_app))

associationTestFun(data,73) 

#Association between earlier usage of banking mobile apps and gender
males_earlier_used_banking_app <- nrow(df[(df$gender==" Male") & (df$Have.you.earlier.used.banking.mobile.apps.before.==" Yes"),])
males_earlier_not_used_banking_app <- nrow(df[(df$gender==" Male") & (df$Have.you.earlier.used.banking.mobile.apps.before.==" No"),])
females_earlier_used_banking_app <- nrow(df[(df$gender==" Female") & (df$Have.you.earlier.used.banking.mobile.apps.before.==" Yes"),])
females_earlier_not_used_banking_app <- nrow(df[(df$gender==" Female") & (df$Have.you.earlier.used.banking.mobile.apps.before.==" No"),])
data <- list(c(males_earlier_used_banking_app,
               females_earlier_used_banking_app),
             c(males_earlier_not_used_banking_app,
               females_earlier_not_used_banking_app))

associationTestFun(data,73) 

###########################################################################

#Most of the people think Mobile Apps would help you to make your bank processes comfortable
x <- 0 + (df$Do.you.think..Banking.Mobile.Apps.would.help.you.to.make.your.bank.processes.comfortable.==" Yes")
bootstrapfun(c(x),10000,0.95)

###########################################################################

#Most of the people use smart phone
x <- 0 + (df$Do.you.own.a.smart.phone.==" Yes")
bootstrapfun(c(x),10000,0.95)

###########################################################################

#Most of the people could install a mobile banking app and use it for your banking processes
x <- 0 + (df$Do.you.think.you.could.install.a.mobile.banking.app.and.use.it.for.your.banking.processes.==" Yes")
bootstrapfun(c(x),10000,0.95)


