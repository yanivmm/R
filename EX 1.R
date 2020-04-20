############################

#Author Yaniv M
#script:  ex' 1. Avi Z.
#version 0.0.1
#date: 2/4/19

############################

#data
study=c("mor","mor","eve","mor","eve")
exam1=c(90,86,90,95,60)
exam2=c(90,100,95,97,55)
exam3=c(95,70,80,75,57)
finalExam=c(80,65,50,40,43)

#factor
study = factor(study)
levels(study)=c(0,1)

#dataframe
data = data.frame(study,exam1,exam2,exam3,finalExam)

#adding average and final grade
examsAve = rep(0,5)
finalGrade =rep(0,5)
examsAve = c(round((exam1+exam3+exam2)/3))
finalGrade = c(round(0.8*finalExam+0.2*examsAve))

data = cbind(data,examsAve,finalGrade)

#passed\failed the original way


passedGeneral = rep("",5)
for (i in 1:5){
  if(finalGrade[i]>60)
     passedGeneral[i] = "Passed"
  else
     passedGeneral[i] = "Failed"
}


#passed\failed the new way

passedNew = rep("",5)
for (i in 1:5){
  if(finalExam[i]>60)
    passedNew[i] = "Passed"
  else
    passedNew[i] = "Failed"
}

#framing to "data"
cbind(data,passedGeneral,passedNew)


#minimum function
minimum = function(V){
min = 0
for (i in 1:(length(V)-1))
  if(V[i]<V[i+1])
    min = V[i]
  return(min)
}

#operating function for Grade

minimum(finalExam)

#graphic Visualization of finalGrade as function of finalExam
plot(finalExam,finalGrade) 

