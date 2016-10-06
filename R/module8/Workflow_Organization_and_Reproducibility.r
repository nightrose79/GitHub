
# https://github.com/HenrikBengtsson/future
library(future)

# Implicitly create a future
a %<-% { 2 + 2 }

value(future({2 + 3}))

library(dplyr)
library(glm2)

df <- iris
str(df)

features <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
label <- 'Species'
classes <- df[[label]] %>% unique() %>% sort()

fitBinomialClassifier <- function(label, class, features, data) {
  formula <- paste('(', label,'=="', class,'") ~ ',
                   paste(features, collapse=' + '), sep='')
  print(formula)
  glm(as.formula(formula), family = binomial, data = data)
}

system.time(
for(class in classes) {
  print("*****")
  print(class)
  print(fitBinomialClassifier(label, class, features, df))
}
)

worker <- function(class) {  # A curried function
  fitBinomialClassifier(label, class, features, df)
}

system.time({
models <- lapply(classes, worker)  # map(seq, f)
names(models) <- classes
print(models)
})

df[12:17,] %>%
  select(Petal.Width) %>%
  lapply(function(x) c(-x, pi*(x/2)^2)) %>%
  print()

library(parallel)

cores <- detectCores() - 1

parallelCluster <- makeCluster(cores)
print(parallelCluster)

parLapply(parallelCluster,
          df$Sepal.Width,
          function(x) pi*(x/2)^2)

tryCatch(
  models <- parLapply(parallelCluster,
                      classes, worker),
  error = function(e) print(e)
)

base <- 3

parLapply(parallelCluster, 
          2:4, 
          function(exponent) base^exponent)

clusterExport(parallelCluster, "base")
base <- 2

stopCluster(parallelCluster)

parallelCluster <- makeCluster(detectCores()-1, type = "FORK")
print(parallelCluster)

# build the single argument function we are going to pass to parallel
mkWorker <- function(class, features, df) {
  # make sure each of the three values we need passed 
  # are available in this environment
  force(class)
  force(features)
  force(df)
  # define any and every function our worker function 
  # needs in this environment
  fitBinomialClassifier <- function(label, class, features, data) {
    formula <- paste('(', label, '=="', class, '") ~ ',
                     paste(features, collapse=' + '), sep='')
    glm(as.formula(formula), family = binomial, data = data)
  }
  # Finally: define and return our worker function.
  # The function worker's "lexical closure" 
  # (where it looks for unbound variables)
  # is mkWorker's activation/execution environment 
  # and not the usual Global environment.
  # The parallel library is willing to transport 
  # this environment (which it does not
  # do for the Global environment).
  worker <- function(class) {
    fitBinomialClassifier(label, class, features, df)
  }
  return(worker)
}

system.time({
models <- parLapply(parallelCluster, classes,
                    mkWorker(class, features, df))
names(models) <- classes
print(models)
})

parSapply(parallelCluster, as.character(2:4), 
          function(exponent){
            x <- as.numeric(exponent)
            c(base = base^x, self = x^x)
          })

# A safer way to stop the cluster
if(!is.null(parallelCluster)) {
  stopCluster(parallelCluster)
  parallelCluster <- c()
}

availableCores()

cl <- makeCluster(availableCores() - 1)
plan(cluster, workers=cl)
cl

pid <- Sys.getpid()
pid

a %<-% {
  cat("Calculating a...\n")
  Sys.getpid()
}

b %<-% {
  rm(pid)
  cat("Calculating b...\n")
  Sys.getpid()
}

cat(a, b, pid)

# Works on all OS
plan(multisession)

# Won't work on Windows, the multiprocess plan defaults to this
plan(multicore)

plan(lazy)

stopCluster()

cl <- makeCluster(cores)

cacheParallel <- function() {
  vars <- 1:2
  tmp <- clusterEvalQ(cl, 
                      library(digest))
 
  parSapply(cl, vars, function(var) {
    fn <- function(x) x^2
    dg <- digest(list(fn, var))
    cache_fn <- 
      sprintf("Cache_%s.Rdata", dg)
    if (file.exists(cache_fn)) {
      load(cache_fn)
    } else {
      var <- fn(var); 
      Sys.sleep(5)
      save(var, file = cache_fn)
    }
    return(var)
  })
}

system.time(out <- cacheParallel())
out

system.time(out <- cacheParallel())
out

# Cleanup
file.remove(list.files(pattern = "Cache.+.Rdata"))

stopCluster(cl)
