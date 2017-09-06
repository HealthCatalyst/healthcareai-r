# Make a class via S3

# Goals:
#   1. Make a dog class that has a species and age attribute with basic constructor validation.
#   2. Create a `.bark()` method on that class with a sensisble default for non-dogs

# global generic definition
bark <- function(x) UseMethod('bark')

# global generic default
bark.default <- function(x){
  print('This isn\'t a dog and cannot bark bark.')
}

# class constructor
dog <- function(species, age) { # defaults can be added here like function(species='Mutt', age=3)
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

# Add a method for the class that takes multiple arguments.
# Global generic
walk <- function(x, ...) UseMethod('walk') # The ... is the additional arguments part.

# Default behavior
walk.default <- function(x) {
  print('Only dogs go for walks')
}

# Using multiple arguments (and their default values)
walk.dog <- function(dog, nearDoor=FALSE, shoes=FALSE, leash=FALSE) {
  walkProbability <- (nearDoor*2 + shoes*3 + leash*5) / 10
  if (walkProbability < .5) {
    print(paste0('Good ', dog$species, 's deserve walks.'))
  } else if (walkProbability < 1) {
    print(paste0('Walk likely for this good ', dog$species,'.'))
  } else {
    print(paste0('A walk! ', dog$age,' treats for everyone.'))
  }
  return(walkProbability)
}

# Adding a method to an existing generic
mean.dog <- function(dog) {
  print(paste('That\'s a mean', dog$species))
}

# instance of class
fido <- dog(species = 'pomeranian', age=7)
boffo <- dog(species = 'mutt', age=3)

# Try some methods on the dogs you instantiated using explicit calls to the class method for speed and clarity
bark.dog(fido)
bark.dog(boffo)
mean.dog(fido)

walk.dog(fido)
walk(fido, nearDoor = TRUE, shoes=TRUE, leash=TRUE)

# Note you can still use the global generic syntax, but it will be slower and harder to maintain
bark(fido)
mean(fido)

# calling the default bark method on not a dog
bark('not a dog')

# Access some variables of the dogs
print(fido$age)
print(boffo$species)

# Understand the dog class a bit
class(fido) # This is the class of fido
methods(bark) # These are the methods that the bark generic can dispatch to
methods(class = 'dog') # These are the generics that can use class dog

