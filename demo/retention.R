data(students)
data(graduates)

names(students)
names(graduates)
str(students)
str(graduates)

students$Degree = as.character(students$Degree)
students$Persisting = NA
students[which(students$Persist == 'Y'),'Persisting'] = TRUE
students[which(students$Persist == 'N'),'Persisting'] = FALSE

students2 = cohortDetails(students, graduates,
						  studentIdColumn='StudentId',
						  degreeColumn='Degree',
						  persistColumn='Persisting',
						  warehouseDateColumn='CreatedDate', 
						  gradColumn='GraduationDate')
head(students2)
table(students2$Status, students2$Persisting, useNA='ifany')

students3 = studentDetails(students, graduates,
						   studentIdColumn='StudentId',
						   degreeColumn='Degree',
						   persistColumn='Persisting',
						   warehouseDateColumn='CreatedDate', 
						   gradColumn='GraduationDate',
						   months=c(15,24))

t1 = as.Date("2002-10-15")
students.t1 = students3[which(students3$CreatedDate == t1)
table(students3W$students.t1$Months)

cr = cohortRetention(students, graduates,
					 studentIdColumn='StudentId',
					 degreeColumn='Degree',
					 persistColumn='Persisting',
					 warehouseDateColumn='CreatedDate', 
					 gradColumn='GraduationDate' )

plot(cr)

ret = retention(students, graduates,
				studentIdColumn='StudentId',
				degreeColumn='Degree',
				persistColumn='Persisting',
				warehouseDateColumn='CreatedDate', 
				gradColumn='GraduationDate')

plot(ret)

ret15 = ret[which(ret$Month==15),]
View(ret15[order(ret15$Cohort),], 'Retention')

