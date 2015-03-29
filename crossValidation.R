
getTrainingData <- function(driverDataPath, driverID, r=0.8) {
	probs = NULL
		actuals = NULL
#randomDriver <- sample(c(1:(driverID-1), min(3612,driverID+1):3612), 1)
		randomDriver = 1278
		for (k in 1:5) {
			row = 200
				n.train = floor(row*r)
				train.idx = sample(1:row,n.train)
				test.idx = c(1:row)
				test.idx = test.idx[! test.idx %in% train.idx]

				train<-data.frame()
				test<-data.frame()
				for(idx in train.idx) {
					readPath=paste(driverDataPath,driverID,sep='/')
						tripID=paste(idx,"csv",sep=".")
						readPath=paste(readPath,tripID,sep='/')
						data=read.csv(readPath)
						data$label <- 0
						train<-rbind(train,data)
				}

			for(idx in test.idx) {
				readPath=paste(driverDataPath,driverID,sep='/')
					tripID=paste(idx,"csv",sep=".")
					readPath=paste(readPath,tripID,sep='/')
					data=read.csv(readPath)
					data$label <- 1
					test<-rbind(test,data)  
			}

			for(idx in train.idx) {
				readPath=paste(driverDataPath,randomDriver,sep='/')
					tripID=paste(idx,"csv",sep=".")
					readPath=paste(readPath,tripID,sep='/')
					data=read.csv(readPath)
					data$label <- 0
					train<-rbind(train,data)
			}  

			for(idx in test.idx) {
				readPath=paste(driverDataPath,randomDriver,sep='/')
					tripID=paste(idx,"csv",sep=".")
					readPath=paste(readPath,tripID,sep='/')
					data=read.csv(readPath)
					data$label <- 0
					test<-rbind(test,data)
			}  

			prob = do.classification(train, test, 'lr')
				actual = test$label
				probs = c(probs,prob)
				actuals = c(actuals,actual)
		}

	  result = data.frame(probs,actuals)
		pred = prediction(result$probs,result$actuals)
		perf = performance(pred, "tpr","fpr")
		plot(perf)  
}

do.classification <- function(train.set, test.set, 
		cl.name, verbose=F) {
	switch(cl.name, 
			lr = { # logistic regression
			model = glm(label~., family=binomial, data=train.set)
			if (verbose) {
			print(summary(model))             
			}
			prob = predict(model, newdata=test.set, type="response") 
			prob
			}
		  )
}
