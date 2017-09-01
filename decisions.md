# Preliminary Plans

Well, we've had a couple of R Reading groups!

## Summary:

- We'd like to use tidyverse. Main advantage: It's the reason R is a modern language. One way or another, we are going to end up with tidyverse in there.
- S3 is Simple, Specific, and Supported. Goodbye R6. Why did we go that way original. 
- We'd like to add lintR (code quality for R packages)
- Some are going to use sublime to do the development. Tidyverse, thank you.
- 

## Style

- Verbose variable names. Avoid abbreviations at all costs.
- Explicitly call methods from other classes using the `package::method()` syntax
- Use `generic.class()` syntax to speed up code and reduce cognitive overhead. "...method dispatch in R can be costly. For S3, you can do this by calling generic.class() instead of generic()." - hadley
