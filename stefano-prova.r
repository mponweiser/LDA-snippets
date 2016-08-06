
mycorpus<-read.csv("prova.csv",header=TRUE, sep=";",stringsAsFactors = FALSE) 

library(tm)

corpus<-Corpus(VectorSource(mycorpus$Abstract))

cleancorpus<-tm_map(corpus,content_transformer(removePunctuation))
cleancorpus<-tm_map(cleancorpus,content_transformer(removeNumbers))
cleancorpus<-tm_map(cleancorpus,content_transformer(tolower))
cleancorpus<-tm_map(cleancorpus,content_transformer(function(x) removeWords(x,stopwords(kind="en"))))

cleancorpus


tdm<-DocumentTermMatrix(cleancorpus)

#weith with tfidf
tdm2 <- as.DocumentTermMatrix(tdm,weighting=weightTfIdf)

#selected of terms based on frequences
tdm<-tdm2[apply(tdm2,1,sum)>9,]

tdm

# install.packages("topicmodels", repos='http://cran.us.r-project.org')

library(topicmodels)
lda <- LDA(tdm, k = 50) # find 20 topics; for 50 you need some patience, depending on your computer

str(lda)

terms_10 <- terms(lda, 10) # first 10 terms of every topic
terms_10 <- apply(terms_10, MARGIN = 2, paste, collapse = ", ")

topic_most_likely <- topics(lda, 1)

topics_years <- data.frame(date=(mycorpus$anno),topic_most_likely)

library(ggplot2)

#qplot(date, ..count.., data=topics_years, geom="density", fill=terms_10[topic_most_likely])
qplot(date, ..count.., data=topics_years, geom="density", fill=paste(topic_most_likely))

# I find ggplot more explicit (a commenter on StackOverflow said: "ignore qplot..."):

ggplot(topics_years, aes(x=date, y=..count..)) + 
  geom_density(aes(fill=paste(topic_most_likely)), position="stack")

table(mycorpus$anno)

years <- levels(factor(mycorpus$anno))
topics_n <- lda@k
theta <- posterior(lda)$topics # theta is the per-document likelihood for a topic

theta_mean_by_year_by <- by(theta, mycorpus$anno, colMeans)
theta_mean_by_year <- do.call("rbind",theta_mean_by_year_by)
colnames(theta_mean_by_year) = paste(1:topics_n)

theta_mean_by_year_ts <- ts(theta_mean_by_year, start = as.integer(years[1]))
theta_mean_by_year_time <- time(theta_mean_by_year)

theta_mean_lm <- apply(theta_mean_by_year, 2, function(x) lm(x ~ theta_mean_by_year_time))
theta_mean_lm_coef <- lapply(theta_mean_lm, function(x) coef(summary(x)))
theta_mean_lm_coef_sign <- sapply(theta_mean_lm_coef, '[',"theta_mean_by_year_time","Pr(>|t|)")
theta_mean_lm_coef_slope <- sapply(theta_mean_lm_coef, '[',"theta_mean_by_year_time","Estimate")

theta_mean_lm_coef_slope_pos <- theta_mean_lm_coef_slope[theta_mean_lm_coef_slope >= 0]
theta_mean_lm_coef_slope_neg <- theta_mean_lm_coef_slope[theta_mean_lm_coef_slope < 0]

p_level <- c(0.5, 0.01, 0.001, 0.0001)
significance_total <- sapply(p_level, function(x) (theta_mean_lm_coef_sign[theta_mean_lm_coef_sign < x]))
significance_neg <- sapply(1:length(p_level), function(x) intersect(names(theta_mean_lm_coef_slope_neg), names(significance_total[[x]])))
significance_pos <- sapply(1:length(p_level), function(x) intersect(names(theta_mean_lm_coef_slope_pos), names(significance_total[[x]])))

significance_neg
significance_pos

topics_hot <- as.numeric(names(sort(theta_mean_lm_coef_slope[significance_pos[[1]]], decreasing=TRUE)))
topics_cold <- as.numeric(names(sort(theta_mean_lm_coef_slope[significance_neg[[1]]], decreasing=FALSE)))

topics_hot
topics_cold

cold_and_hot_ts <- cbind(
    theta_mean_by_year_ts[,topics_cold[1:5]],
    theta_mean_by_year_ts[,topics_hot[1:5]], deparse.level=0)

colnames(cold_and_hot_ts) <- as.character(c(topics_cold[1:5], topics_hot[1:5]))

library(lattice)

print(xyplot(theta_mean_by_year_ts[,names(sort(theta_mean_lm_coef_slope))],
	#layout = c(5, 5),
	#screens = c(rep("cold topics", 5), rep("hot topics", 5)),
	screens = rep(1:5, each = 10),
	superpose = TRUE,
	col = "blue",
	alpha = 0.3,
	#ylim = c(0, 0.015),
        #ylab = "Mean theta",
        ylab = expression(paste("Mean ",theta)),
	xlab = "Year",
	type = c("l", "g"),
	#aspect = "xy",
	#auto.key = list(space = "right"), 
	auto.key = FALSE,
	scales = list(x = list(alternating = FALSE)),
	#par.settings = standard.theme(color = FALSE)
	))

print(xyplot(cold_and_hot_ts,
	layout = c(1, 2),
        screens = c(rep("Cold topics", 5), rep("Hot topics", 5)),
	superpose = TRUE,
        #ylim = c(0.04, 0.06),
	ylab = expression(paste("Mean ",theta)),
	xlab = "Year",
	type = c("l", "g"),
	auto.key = list(space = "right"), 
	scales = list(x = list(alternating = FALSE))
    # scales= list( relation ="free")
	#par.settings = standard.theme(color = FALSE)
        ))

terms_10[c(topics_cold[1:5], topics_hot[1:5])]
