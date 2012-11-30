require(retention)

data(students)
data(graduates)

names(students)
names(graduates)
str(students)
str(graduates)

#Recode the persisting column to be of logical type
students$Persisting = NA
students[which(students$Persist == 'Y'),'Persisting'] = TRUE
students[which(students$Persist == 'N'),'Persisting'] = FALSE

unique(students$Degree)
table(students[!duplicated(students$StudentId),'Degree'], useNA='ifany')

if(file.exists('CohortRetentionDemo.rda')) {
	load('CohortRetentionDemo.rda')
} else {
	s = proc.time(); cr = cohortRetention(students, graduates,
						 studentIdColumn='StudentId',
						 degreeColumn='Degree',
						 persistColumn='Persisting',
						 warehouseDateColumn='CreatedDate', 
						 gradColumn='GraduationDate' ); proc.time() - s
	save(cr, file='CohortRetentionDemo.rda')
}

summary(cr)
plot(cr)
xtable(cr)

if(file.exists('RetentionDemo.rda')) {
	load('RetentionDemo.rda')
} else {
	s = proc.time(); ret = retention(students, graduates,
					studentIdColumn='StudentId',
					degreeColumn='Degree',
					persistColumn='Persisting',
					warehouseDateColumn='CreatedDate', 
					gradColumn='GraduationDate'); proc.time() - s
	save(ret, file='RetentionDemo.rda')
}

summary(ret)
plot(ret)
plot.RetentionOverall(ret)
xtable(ret)

if(file.exists('CohorteRetentionLevelDemo.rda')) {
	load('CohorteRetentionLevelDemo.rda')
} else {
	cr.level = cohortRetention(students, graduates,
						 studentIdColumn='StudentId',
						 degreeColumn='Degree',
						 persistColumn='Persisting',
						 warehouseDateColumn='CreatedDate', 
						 gradColumn='GraduationDate',
						 grouping='Level')
	save(cr.level, file='CohorteRetentionLevelDemo.rda')
}

summary(cr.level)
plot(cr.level)
xtable(cr.level)

if(file.exists('RetentionLevelDemo.rda')) {
	load('RetentionLevelDemo.rda')
} else {
	ret.level = retention(students, graduates, 
						  studentIdColumn='StudentId',
						  degreeColumn='Degree',
						  persistColumn='Persisting',
						  warehouseDateColumn='CreatedDate', 
						  gradColumn='GraduationDate',
						  grouping='Level')
	save(ret.level, file='RetentionLevelDemo.rda')
}

summary(ret.level)
plot(ret.level)
xtable(ret.level)

fysum = fySummary(ret$Summary, which(ert$Summary$Month == 15), 
				  rateCol='RetentionRate', grouping='Group')

View(cr.level$Summary, 'Cohort Summary by Level')

ret15 = ret[which(ret$Month==15),]
View(ret15[order(ret15$Cohort),], 'Retention')

