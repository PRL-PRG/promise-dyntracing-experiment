library(promisedyntracer)

dyntrace_promises({
# ------------------------------------------------------------------
# This material is distributed under the GNU General Public License
# Version 2. You may review the terms of this license at
# http://www.gnu.org/licenses/gpl-2.0.html
#
# Copyright (c) 2012-2016, Bernd Bischl, Michel Lang
# TU Dortmund University
#
# All rights reserved.
#
# naiveBayes algorithm, implemented in R
# ------------------------------------------------------------------

# Train NaiveBayes.
#
# @param target [character(1)]
#   Name of target variable in data.
# @param data [data.frame]
#   Data set for fitting.
# @param threshold [numeric(1)]
#   Replace 0 entry cells with this value.
# @return [list]. NaiveBayes model containing:
#   target [character(1)]: Name of target variable.
#   classes [character]: Levels of classes.
#   priors [numeric]: Named vector of empirical priors of classes.
#   probs [list]: List, one element per class.
#     Each element is again a list, one element per feature.
#     Each element of the second list is a numeric vector, containing
#     the proportions for all feature levels for this class / feature.
#     The numeric vector is named by the feature levels.
trainNaiveBayes = function(target, data, threshold=0.001) {
  # some abbreviations
  n = nrow(data)
  y = data[,target]
  classes = levels(y)
  k = length(classes)
  featnames = setdiff(colnames(data), target)
  feats = data[, featnames]

  # result objects
  priors = setNames(numeric(k), classes)
  probs = vector("list", k)

  # loop thru classes
  for (cl in classes) {
    # subset to class
    feats2 = feats[y == cl,]
    # size of class
    priors[cl] = nrow(feats2) / n
    # for each feature calculate proportions of levels
    # NAs are disregarded automatically
    probs[[cl]] = lapply(feats2, function(x) {
      p = prop.table(table(x))
      p = setNames(as.numeric(p), names(p))
      # if we have some cells with 0 entries replace them by threshold
      ifelse(p == 0, threshold, p)
    })
  }
  list(target=target, classes=classes, priors=priors, probs=probs)
}



# Predict function for our NaiveBayes model for new data.
#
# @param model [list]
#   Our model from trainNaiveBayes.
# @param newdata [data.frame]
#   New data to predict. May or may not contain
#   target column.
# @return [matrix]. Matrix of probabilities, named by classes.
predictNaiveBayes = function(model, newdata) {
  n = nrow(newdata)
  newdata[, model$target] = NULL
  cls = model$classes
  # result object
  pred = matrix(NA, nrow=n, ncol=length(cls))
  colnames(pred) = cls
  # loop thru new observations
  for (i in 1:nrow(newdata)) {
    # feature vector as char vector
    x = sapply(newdata[i,], as.character)
    # loop thru classes and get prob for each class
    for (cl in cls) {
      # index our prob tables
      p = model$probs[[cl]]
      q = sapply(names(x), function(n) p[[n]][x[n]])
      # multiply probs and prior prob
      pred[i, cl] = prod(q, na.rm=TRUE) * model$priors[[cl]]
    }
  }
  # normalize
  pred = t(apply(pred, 1, function(x) x/sum(x)))
  return(pred)
}

# test method on HouseVotes (in mlbench package)
data(HouseVotes84, package = "mlbench")

m1 = trainNaiveBayes("Class", HouseVotes84)
p1 = predictNaiveBayes(m1, HouseVotes84)

if (FALSE) { # compare against implementation in e1071
  library(e1071)
  m2 = naiveBayes(Class~., data = HouseVotes84)
  p2 = predict(m2, newdata = HouseVotes84, type = "raw")
  print(max(abs(p1 - p2)))
}

# test method on Mushroom (in cba package)
data(Mushroom, package = "cba")

# preprocess: remove constant features, fix factor levels
data = Mushroom
data[, "veil-type"] = NULL
ind = sapply(data, function(x) anyDuplicated(levels(x)) > 0L )
suppressWarnings( { data[ind] = lapply(data, as.factor) } )

m1 = trainNaiveBayes("class", data)
p1 = predictNaiveBayes(m1, data)

if (FALSE) { # compare against implementation in e1071
  library(e1071)
  m2 = naiveBayes(class~., data = data)
  p2 = predict(m2, newdata = data, type = "raw")
  print(max(abs(p1 - p2)))
}
}
, 'naiveBayes.sqlite'
, verbose=FALSE
, truncate=TRUE)
