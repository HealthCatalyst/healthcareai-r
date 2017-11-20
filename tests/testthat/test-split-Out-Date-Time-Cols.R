context('Checking that splitOutDateTimeCols is working')

test_that("For a DT column with YYYY-MM-DD format for depth='h', function returns expected 
          output", {
            
            ##YYYY-MM-DD ; depth = 'h'
            dtCol <- c('2001-06-09 12:45:05','2002-01-29 09:30:05','2002-02-13 07:36:50')
            y1 <- c(.5,1,3)
            y2 <- c(.8,1,1.2)
            df <- data.frame(dtCol,y1,y2)
            df <- splitOutDateTimeCols(df, 'dtCol')
            
            expecteddf <- structure(list(y1 = c(0.5, 1, 3), y2 = c(0.8, 1, 1.2), 
                                         year = c(2001, 2002, 2002), month = c(6, 1, 2),
                                         weekOfYear = c("23", "04", "06"),
                                         dayOfMonth = c(9L, 29L, 13L), dayOfWeek = c(7, 3, 4),
                                         hour = c(12L, 9L, 7L)), 
                                    .Names = c("y1", "y2", "year", "month", "weekOfYear","dayOfMonth", "dayOfWeek", "hour"), 
                                    row.names = c(NA, -3L), class = "data.frame")
            
            expect_identical(df, expecteddf)
  })

test_that("For a DT column with DD/MM/YYYY format for depth='s', function returns expected
          output", {
            
            ## DD/MM/YYYY ; depth = 's'
            dtCol <- c('09/06/2001 12:45:05','29/01/2002 09:30:05','13/02/2002 07:36:50')
            y1 <- c(.5,1,3)
            y2 <- c(.8,1,1.2)
            df2 <- data.frame(dtCol,y1,y2)
            df2 <- splitOutDateTimeCols(df2, 'dtCol',format="%d/%m/%Y %H:%M:%S",depth='s')
            
            expecteddf2 <- structure(list(y1 = c(0.5, 1, 3), y2 = c(0.8, 1, 1.2), 
                                          year = c(2001, 2002, 2002), month = c(6, 1, 2), 
                                          weekOfYear = c("23", "04", "06"), dayOfMonth = c(9L, 29L, 13L),
                                          dayOfWeek = c(7, 3, 4), hour = c(12L, 9L, 7L), min = c(45L, 30L, 36L),
                                          Sec = c(5, 5, 50)), 
                                     .Names = c("y1", "y2", "year", "month", "weekOfYear","dayOfMonth", "dayOfWeek","hour", "min", "sec"),
                                     row.names = c(NA, -3L), class = "data.frame")
            expect_identical(df2, expecteddf2)
  })

test_that("For a DT column with MM.DD.YYYY format for depth='m', function returns expected 
          output", {
            
            ## MM.DD.YYYY ; depth = 'm'
            dtCol <- c('06.09.2001 12:45:05','01.29.2002 09:30:05','02.13.2002 07:36:50')
            y1 <- c(.5,1,3)
            y2 <- c(.8,1,1.2)
            df3 <- data.frame(dtCol,y1,y2)
            df3 <- splitOutDateTimeCols(df3, 'dtCol',format="%m.%d.%Y %H:%M:%S",depth='m')
            expecteddf3 <- structure(list(y1 = c(0.5, 1, 3), y2 = c(0.8, 1, 1.2), year = c(2001,2002, 2002),
                                          month = c(6, 1, 2), weekOfYear = c("23", "04", "06"), dayOfMonth = c(9L, 29L, 13L),
                                          dayOfWeek = c(7, 3, 4), hour = c(12L,9L, 7L), min = c(45L, 30L, 36L)),
                                     .Names = c("y1", "y2", "year", "month", "weekOfYear", "dayOfMonth", "dayOfWeek", "hour", "min" ), 
                                     row.names = c(NA, -3L), class = "data.frame")
            
            expect_identical(df3, expecteddf3)
  })

test_that("For a DT column with MM-DD-YYYY format for depth='d', function returns expected
          output",{
  
            ##MM-DD-YYYY ; depth = 'd'
            dtCol <- c('06-09-2001 12:45:05','01-29-2002 09:30:05','02-13-2002 07:36:50')
            y1 <- c(.5,1,3)
            y2 <- c(.8,1,1.2)
            df4 <- data.frame(dtCol,y1,y2)
            df4 <- splitOutDateTimeCols(df4, 'dtCol',format="%m-%d-%Y",depth='d')
            expecteddf4 <- structure(list(y1 = c(0.5, 1, 3), y2 = c(0.8, 1, 1.2), year = c(2001,2002, 2002), month = c(6, 1, 2),
                                          weekOfYear = c("23", "04", "06"), dayOfMonth = c(9L, 29L, 13L),dayOfWeek = c(7, 3, 4)),
                                     .Names = c("y1","y2", "year", "month", "weekOfYear", "dayOfMonth", "dayOfWeek"),
                                     row.names = c(NA, -3L), class = "data.frame")
            
            expect_identical(df4, expecteddf4)
  })
