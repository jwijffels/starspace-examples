library(tools)
library(ctv)
library(data.table)
library(ruimtehol)
library(tokenizers)
##
## Get to which task views each R package belongs (one package can belong to several task views)
##
cran_taskviews <- available.views()
cran_taskviews <- lapply(cran_taskviews, FUN=function(x){
  data.table(Taskview = gsub(" ", "-", x$name), 
             Taskview_Maintainer = x$email, 
             Package = x$packagelist$name)
})
cran_taskviews <- rbindlist(cran_taskviews)
cran_taskviews <- cran_taskviews[, list(Taskviews = paste(Taskview, collapse = "&"), 
                                        Taskviews_Amount = length(unique(Taskview))), by = list(Package)]

##
## Get the text of the title and description of the package
##
crandb <- CRAN_package_db()
crandb <- merge(crandb, cran_taskviews, by = "Package", all.x=TRUE, all.y=FALSE)
crandb$target <- ifelse(is.na(crandb$Taskviews), "None", crandb$Taskviews)
crandb$target <- strsplit(crandb$target, "&")
crandb$text <- paste(crandb$Title, crandb$Description, sep = "\n")
crandb$text <- tokenize_words(crandb$text, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, 
                              stopwords = stopwords::stopwords(language = "en", source = "snowball"))
crandb$text <- sapply(crandb$text, FUN=function(x) paste(x, collapse = " "))
if(TRUE){
  ## optional - lemmatise with udpipe (version >= 0.8.1) - note this takes some time 
  library(udpipe)
  x <- setNames(strsplit(crandb$text, " "), crandb$Package)
  x <- sapply(x, FUN=function(x) paste(x, collapse = "\n"))
  x <- udpipe(x, "english-ewt", udpipe_model_repo = "jwijffels/udpipe.models.ud.2.3", tokeniser = "vertical", parser = "none", trace = 500)
  x <- paste.data.frame(x, term = "lemma", group = "doc_id", sep = " ")
  crandb <- merge(crandb, x[, c("doc_id", "lemma")], 
                  by.x = "Package", by.y = "doc_id", all.x = TRUE, all.y = FALSE, order = FALSE)
}else{
  crandb$lemma <- crandb$text
}
saveRDS(crandb, file = "crandb.rds")

##
## Build tagspace model using dot similarity (basically focussing on many words which are similar)
##
set.seed(321)
traindata <- subset(crandb, !is.na(Taskviews))
model <- embed_tagspace(x = traindata$lemma, y = traindata$target, 
                        dim = 50, loss = "hinge", similarity = "dot", adagrad = TRUE, epoch = 50, lr = 0.05, 
                        margin = 1.5, negSearchLimit = 1, minCount = 2, ngrams = 1, label = "#")
plot(model)

##
## Find closest terms to each task view  
##
dict <- starspace_dictionary(model)
embedding_closest <- predict(model, 
                             newdata = data.frame(doc_id = dict$labels, text = dict$labels, stringsAsFactors = FALSE), 
                             type = "knn", k = 15)
embedding_closest

##
## Find closest taskview to your own package
##
embedding_taskviews <- as.matrix(model, type = "labels", prefix = FALSE)
embedding_packages  <- predict(model, 
                               newdata = data.frame(doc_id = crandb$Package, text = crandb$text, stringsAsFactors = FALSE),
                              type = "embedding")
scores <- embedding_similarity(embedding_packages, embedding_taskviews, type = "dot", top_n = 3)
scores <- setnames(scores, old = c("term1", "term2", "similarity", "rank"), new = c("Package", "Taskview", "similarity", "rank"))
subset(scores, Package %in% c("ruimtehol", "BTM"))
subset(scores, Package %in% c("cronR", "taskscheduleR"))

##
## Find closest packages to each taskview
##
scores <- embedding_similarity(embedding_taskviews, embedding_packages, type = "dot", top_n = 50)
scores <- setnames(scores, old = c("term1", "term2", "similarity", "rank"), new = c("Taskview", "Package", "similarity", "rank"))
subset(scores, Taskview %in% c("NaturalLanguageProcessing"))
subset(scores, Taskview %in% c("Survival"))
subset(scores, Taskview %in% c("NumericalMathematics"))
subset(scores, Taskview %in% c("Bayesian"))

##
## Although the model was not built for this you can also
## find closest package to your own package in that tagspace embedding which focussed on identifying taskviews
##
scores <- embedding_similarity(embedding_packages[c("BTM", "udpipe", "textrank"), ], 
                               embedding_packages, type = "dot", top_n = 5)
scores <- setnames(scores, old = c("term1", "term2", "similarity", "rank"), new = c("Package", "PackageComparison", "similarity", "rank"))
scores
