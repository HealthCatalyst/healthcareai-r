# Make a class via S3

# Goals:
#   1. Make a dog class that has a species and age attribute with basic constructor validation.
#   2. Create a `.bark()` method on that class with a sensisble default for non-dogs

# global generic definition
bark, <- function(x) UseMethod('bark')

# global generic default
bark.default <- function(x){
  print('This isn\'t a dog and cannot bark bark.')
}

# class constructor
dog <- function(species, age){
  if(!is.numeric(age)) stop("age must be numeric")
  if(!is.character(species)) stop('Species must be a string')
  
  # build the list of 'instance variables'
  instance <- list(species=species, age=age)
  
  # assign the class
  class(instance) <- "dog"
  
  print(paste('You just made a dog that is a', age, 'year old', species))
  print('Here is your new dog.')
  print(instance)
  
  return(instance)
}

# Create a new method for the class
bark.dog <- function(dog) {
  for(i in 1:dog$age){
    print(paste('the', dog$species, 'says: woof'))
  }
}

# Adding a method to an existing generic
mean.dog <- function(dog) {
  print(paste('That\'s a mean', dog$species))
}

# instance of class
fido <- dog(species = 'pomeranian', age=7)
boffo <- dog(species = 'mutt', age=3)

# Try some methods on the dogs you instantiated.
bark.dog(fido)
bark.dog(boffo)
mean(fido)

# Access some variables of the dogs
print(fido$age)
print(boffo$species)