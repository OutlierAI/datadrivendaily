#--- Read in survey responses ---#
# Note: For privacy purposes, I've not made available the actual responses of our respondents, however, you
# should be able to replace these lines of code with your own data and produce similar results.
survey_responses <- read.csv("~/Desktop/Data Driven Daily - The Data Driven Survey (Responses) - Form Responses.csv", stringsAsFactors = FALSE)
kpi <- survey_responses$What.Key.Performance.Indicators..KPIs..do.you.use.to.run.your.business.
kpi <- kpi[kpi != ""] # remove no responses

library(tm)
corpus <- VCorpus(VectorSource(kpi))
corpus <- tm_map(corpus, removePunctuation)
tf     <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
tf_idf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))

tf_matrix     <- as.matrix(tf)
tf_idf_matrix <- as.matrix(tf_idf)

tf_ranking     <- sort(colSums(tf_matrix), decreasing = TRUE)
tf_idf_ranking <- sort(colSums(tf_idf_matrix), decreasing = TRUE)

library(wordcloud)
png("~/Desktop/tf_wordcloud.png")
wordcloud(names(tf_ranking),     tf_ranking,     min.freq = 1, rot.per = 0.25, random.order = FALSE, colors = c(paste0("grey", 80:20), "black"))
dev.off()
png("~/Desktop/tf_idf_wordcloud.png")
wordcloud(names(tf_idf_ranking), tf_idf_ranking, min.freq = 0, rot.per = 0.25, random.order = FALSE, colors = c(paste0("grey", 80:20), "black"))
dev.off()
