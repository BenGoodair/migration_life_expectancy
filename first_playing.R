####This is my first attempt to a) build a simulating dataset similar to LS
#### and b) analyse it as per Wallace et al. Immigration, mortality, and 
####national life expectancy in the Nordic region, 1990-2019


library(demography)
library(ggplot2)
setwd('C:/Users/bengo/OneDrive - Nexus365/Documents/Migration_LE_project')

dat <- read.csv("Data/death_rate_uk.csv")
dat$Total <- as.double(dat$Total)
dat$Year <- as.double(dat$ï..Year)
dat$Age <- as.double(dat$Age)
dat <- dat[complete.cases(dat),]

dat <-hmd.mx("GBR_NP", "benjamin.goodair@spi.ox.ac.uk", "Kn0wledge!is!power!", "UK")
#wut <- as.data.frame(dat)


dat2<-read.demogdata("Data/rates.txt", "Data/exposures.txt", type="mortality", label="uk2")

#Things I've learned - the package requires very specific inputs.
#For this it has required death rates and 'exposure to risk' files.
#Organised by year and age group
#Exposure to risk is just how many people were alive at that age in that year
#(I think)
#package requires .txt files I fink


plot(dat)

plot(dat,series=ifelse(!is.null(dat$rate),names(dat$rate)[3],names(dat$pop)[3]), datatype=ifelse(!is.null(dat$rate),"rate"),plot.type=c("time"),xlab="Year")


lt <- lifetable(dat, series = names(dat$rate)[1], years = dat$year,
          ages = dat$age, max.age = min(100, max(dat$age)),
          type = c("period", "cohort"))

lifetable=lifetable(dat,years=c(2020),age=dat$age,type=c("period"))

plot(lifetable, lifetable,  xlab="Age", ylab="Expected number of years left")

lefemale=life.expectancy(dat,series=names(dat$rate)[1],years=c(1922:2020),type=c("period"), age=1)
plot(lefemale,  ylab="Life Expectancy (Female)", xlab="Year")



#Build simulated data


df = read.table('Data/rates.txt', sep="", header = T, skip = 2)

df$Female <- as.double(df$Female)
df$Male <- as.double(df$Male)
df$Total <- as.double(df$Total)

x <- (1000:2000)
minus <- sample(x, 10989, replace = T)/1000
df$minus <- minus

df$Total <- df$Total/df$minus

x <- (1000:2000)
minus <- sample(x, 10989, replace = T)/1000
df$minus <- minus

df$Male <- df$Male/df$minus


x <- (1000:2000)
minus <- sample(x, 10989, replace = T)/1000
df$minus <- minus

df$Female <- df$Female/df$minus

df <- df%>% dplyr::select(-minus)

df[is.na(df)] <- '.'

write.table(df, file='Data/ratesless.txt', sep="\t",row.names = FALSE,  quote = FALSE)

#dat = demogdata(df, pop=df2, ages='Age', years='Year', type = "mortality")

dat2<-read.demogdata("Data/ratesless.txt", "Data/exposures.txt", type="mortality",
                     label="uk2", skip=0, popskip=2)

# dat2$rate$female <- as.double(dat2$rate$female)
# dat2$rate$male <- as.double(dat2$rate$male)
# dat2$rate$total <- as.double(dat2$rate$total)

plot(dat)
plot(dat2)
###


lifetable=lifetable(dat,years=c(2020),age=dat$age,type=c("period"))
plot(lifetable, lifetable,  xlab="Age", ylab="Expected number of years left")



lifetable=lifetable(dat2,years=c(2020),age=dat$age,type=c("period"))
plot(lifetable, lifetable,  xlab="Age", ylab="Expected number of years left")

lefemale=cbind(as.data.frame(life.expectancy(dat,series=names(dat$rate)[1],
                                       years=c(1922:2020),type=c("period"), 
                                       age=1)),
               as.data.frame(life.expectancy(dat2,series=names(dat$rate)[1],
                                       years=c(1922:2020),type=c("period"), 
                                       age=1)))

lefemale <- as.data.frame(life.expectancy(dat,series=names(dat$rate)[1],
                                          years=c(1922:2020),type=c("period"), 
                                          age=1))

lefemale$year <- row.names(lefemale) 
lefemale$migrant <- "Native_born"
lefemale$x <- as.numeric(lefemale$x)

lefemale2 <- as.data.frame(life.expectancy(dat2,series=names(dat$rate)[1],
                                          years=c(1922:2020),type=c("period"), 
                                          age=1))

lefemale2$year <- row.names(lefemale2) 
lefemale2$migrant <- "Migrant" 
lefemale2$x <- as.numeric(lefemale2$x)

lefemale <- rbind(lefemale[c(1:3)], lefemale2[c(1:3)])

library(jtools)

plot <- ggplot(lefemale, aes(x=as.double(year), y=x, group=migrant, color=migrant))+
  geom_line()+
  theme_nice()+
  labs(x="Year", y="Female life expectancy at age 1", title = "Migrants are healthy!",
       color="Migration status")

##







































coreno



stata_describe <- " storage   display    value
variable name   type    format     label      variable label
-------------------------------------------------------------------------------------------
coreno          long    %12.0g                
dobyr           int     %8.0g                 Year of Birth
sex             byte    %8.0g      sex        Gender
hiscen71        byte    %19.0g     hiscen7    Present at 1971 indicator
hiscen81        byte    %19.0g     hiscen8    Present at 1981 indicator
hiscen91        byte    %31.0g     hiscen9    Present at 1991 indicator
hiscen01        byte    %36.0g     hiscen0    Present at 2001 indicator
hiscen11        byte    %8.0g      hiscen11   Present at 2011 Census indicator
trace           byte    %8.0g      trace      Trace Indicator.
dobd001         byte    %49.0g     dobd001    First discrepant birthdate
dobd002         byte    %49.0g     dobd002    Second discrepant birthdate
dobd003         byte    %49.0g     dobd003    Third discrepant birthdate
dobd004         byte    %49.0g     dobd004    Fourth discrepant birthdate
dobd005         byte    %49.0g     dobd005    Fifth discrepant birthdate
dobd006         byte    %49.0g     dobd006    Sixth discrepant birthdate
sex7            byte    %8.0g                 
oldpob7         str4    %9s                   
yrent7          int     %13.0g     yrent7     1971. Year of entry to United
                                                Kingdom=value+1970
pob7            byte    %30.0g     pob7       Country of Birth 1971
cob7_3cats      float   %41.0g     cob_3cats
                                              Country of birth in 3 categories 1971
sex8            int     %8.0g                 
oldcob8         str2    %9s                   
cob8            long    %58.0g     cob8       Country of Birth 1981
sexflag8        float   %9.0g                 
cob8_3cats      float   %41.0g     cob_3cats
                                              Country of birth in 3 categories 1981
sex9            int     %8.0g                 
cob9            int     %33.0g     cob9       Country of Birth 1991
sexflag9        float   %9.0g                 
cob9_3cats      float   %41.0g     cob_3cats
                                              Country of birth in 3 categories 1991
sex0            long    %12.0g                
oldcobp0        str4    %9s                   
cobp0           int     %51.0g     cob01      Country of Birth 2001
sexflag0        float   %9.0g                 
cob0_3cats      float   %41.0g     cob_3cats
                                              Country of birth in 3 categories 2001
oldcobp11       str3    %9s                   
sex11           int     %8.0g                 
yrarrmon11      int     %8.0g                 
yrarryear11     int     %8.0g                 
cobp11          int     %60.0g     cobp11     Country of birth at lowest level.
yrarrmon11_imp  float   %11.0g     imp        
yraryear11_imp  float   %11.0g     imp        
sexflag11       float   %9.0g                 
cob11_3cats     float   %41.0g     cob_3cats
                                              Country of birth in 3 categories 2011
agdc3dde        int     %15.0g     agdc3dde   Age at death of LSM
deyrbde         int     %12.0g                Year of Death of LS member
coede           byte    %8.0g                 
died            float   %9.0g                 
evdybir         byte    %8.0g                 
evmtbir         byte    %8.0g                 
evyrbir         int     %8.0g                 
immig           float   %9.0g                 
evyreem         int     %8.0g                 
evmteem         byte    %8.0g                 
evdyeem         byte    %8.0g                 
embrnumb        float   %9.0g                 embarkation number
embrtotal       float   %9.0g                 total number of embarkations
embark          float   %9.0g                 
-------------------------------------------------------------------------------------------
"

# Function to convert Stata describe output to dataframe
convert_describe_text_to_dataframe <- function(describe_text) {
  # Split the text output into blocks for each variable
  blocks <- strsplit(describe_text, "\n")[[1]]
  
  # Initialize empty vectors for storing variable names and values
  variable_names <- c()
  variable_values <- c()
  
  # Iterate through the blocks and extract variable names and values
  for (block in blocks) {
    # Split the block into lines
    lines <- strsplit(block, "\n")[[1]]
    
    # Extract the variable name
    variable_name <- trimws(lines[1])
    
    # Extract the variable value
    variable_value <- NA
    
    # Find the line with the variable value
    value_line <- grep(".value", lines)
    
    if (length(value_line) > 0) {
      # Extract the variable value from the line
      variable_value <- trimws(gsub("^\\s+", "", lines[value_line]))
    }
    
    # Append the variable name and value to the corresponding vectors
    variable_names <- c(variable_names, variable_name)
    variable_values <- c(variable_values, variable_value)
  }
  
  # Create the dataframe
  df <- data.frame(variable = variable_names, value = variable_values, stringsAsFactors = FALSE)
  
  return(df)
}

# Convert the Stata describe text output to a dataframe
result_df <- convert_describe_text_to_dataframe(stata_describe)
