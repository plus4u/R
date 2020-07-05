### pareto 

# Import the qcc package

library(qcc)                  

# Create a vector with the number of defects per defect type
defects <- c(27, 389, 65, 9, 15, 30, 12, 109, 45, 321)            

# Create a vector with the names of the defects 
names(defects) <- c("Defect 1", "Defect 2", "Defect 3", "Defect 4",
                    "Defect 5", "Defect 6", "Defect 7", "Defect 8",
                    "Defect 9", "Defect 10")   

# Create the Pareto chart
pareto.chart(defects,
             ylab = "Frequency",
             ylab2 ="Cumulative Percentage",
             main = "Pareto Chart",
             cumperc = seq(0, 100, by = 20))


### Run Chart


# Import the SixSigma package

# install.packages("SixSigma")

library(SixSigma)

# Create 50 random data points
x <- rnorm(50 ,15, 5)

# Build the run chart
plot(x,                        
     type = "b",              
     pch  = 16,                 
     ylim = c(0,30),           
     axes = FALSE,             
     main = "Run Chart Title", 
     sub  = "Chart Subtitle",  
     xlab = "Run",            
     ylab = "y (measure)")     
axis(1,                        
     at = 0:50,                
     cex.axis = 0.7)           
axis(2)                        
box()                          
grid()                         
abline(h = 15 ,               
       lwd = 2)

### Histogram

# rnorm (n, mean, sd)

library(SixSigma)

# Create 50 random data points
x <- rnorm(1000, 35, 7)

# Build the histogram
hist(x,                             
     breaks = "FD",                 
     main = "Histogram Title",      
     sub = "Histogram Subtitle",    
     xlab = "Measure",              
     col = "lightgrey",             
     border = "black")              
grid()                            
box()                   


### Cause-and-effect diagram

library(SixSigma)

# Specify the effect to be analyzed
b.effect <- "Delay"

# Create a vector with the names of the causes classification groups
b.groups <- c("Personnel", "Weather", "Suppliers", "Planning")

# Create a vector that contains the causes
b.causes <- c(vector(mode = "list", length = length(b.groups)))

# Create lists corresponding to the causes for each corresponding group
b.causes[1] <- list(c("Training", "Inadequate"))
b.causes[2] <- list(c("Rain", "Temperature", "Wind"))
b.causes[3] <- list(c("Materials", "Delays", "Rework"))
b.causes[4] <- list(c("Customer", "Permissions", "Errors"))

# Create the cause-and-effect diagram

ss.ceDiag(b.effect,
          b.groups,
          b.causes,
          main = "Cause-and-Effect Diagram (SixSigma package)",
          sub = "Construction Example")

### Scatter Plot

library(SixSigma)

# Create 50 random data points and name them x
x <- rnorm(50, 15, 5)

# Create 50 random data points and name them y
y <- rnorm(50, 30, 4)

# Create a data frame with both data columns
df <- data.frame(x, y)

# Build the scatter plot
plot(y ~ x,                   
     data = df,               
     main = "Scatter Plot",   
     sub  = "Plot Subtitle",  
     xlab = "x",              
     ylab = "y",              
     xlim = c(0,30),          
     ylim = c(0,45),          
     col  = "black",          
     pch  = 16,               
     asp  = 0)               
grid()     

###  Control Charts

library(qcc)

# Create the defects data column
defects <- as.integer(rnorm(50, 3, 1))

# Create the sample size data column
sample_size <- as.integer(rep(20, 50))

# Create a data frame with both columns
df <- data.frame(defects, sample_size)

# Create the np-chart
c_chart <- with(df, qcc(df$defects, df$sample_size, type = "c", data.name = "defects"))

# Get the summary for the chart
summary(c_chart)


###  Check Sheet

# install.packages("grid")

library(grid)

# Build external box
grid.rect(width = 0.5,
          height = unit(9, "inches"),
          x = 0.5)

# Add a title
grid.text("CHECK SHEET",
          x = 0.5,
          y = 0.95,
          just = "top")

# Add basic information about the item to be inspected
grid.text("Item No.:___________________________________ Customer:___________________________________",
          x = 0.28,
          y = 0.875,
          just = "left")
grid.text("Item description:________________________________________________________________________",
          x = 0.28,
          y = 0.825,
          just = "left")

# Build internal box
grid.rect(width = 0.45,
          height = unit(6.8, "inches"),
          x = 0.5,
          y = 0.42)

# Add information about the points to be inspected and the possible answers
grid.text("INSPECTION CHECK POINTS - FILE REVIEW & QUALITY ASSURANCE",
          x = 0.5,
          y = 0.74,
          just = "top")
grid.text("Description                                                     YES               NO               N/A               Comments",
          x = 0.285,
          y = 0.68,
          just = "left")
grid.text("1._________________________\n\n2._________________________\n\n3._________________________\n\n4._________________________\n\n5._________________________\n\n6._________________________\n\n7._________________________\n\n8._________________________\n\n9._________________________\n\n10.________________________",
          x = 0.285,
          y = 0.4,
          just = "left")

# Add information about the inspector
grid.text("INSPECTOR NAME:___________________________ SIGNATURE:___________________________",
          x = 0.5,
          y = 0.1,
          just = "center")






