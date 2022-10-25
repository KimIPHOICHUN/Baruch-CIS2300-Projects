str(googleplaystore)
library(ggplot2)
install.packages("ggplot2")
installed.packages
install.packages(dplyr)
library(dplyr)

######################
##TESTING##
#####################
a= googleplaystore %>% filter(Category == "ART_AND_DESIGN") %>% arrange(desc(a$Installs))

ggplot(a) + aes(x = Category)+ geom_bar() + facet_grid(Type~ Installs) + ggtitle("Admission Rate for Men and Women")

b= googleplaystore %>% filter(Category == "BEAUTY")
c= googleplaystore %>% filter(Category == "BEAUTY") %>% arrange(desc(Installs))

ggplot(c) + aes(x = Category)+ geom_bar() + facet_grid(Type~ Installs) + ggtitle("Admission Rate for Men and Women")


googleplaystore <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/STAT/Stat 3000/google-play-store-apps/googleplaystore.csv") 
str(googleplaystore)


ggplot(c) + aes(x = Category)+ geom_bar() + facet_grid(Type~ Installs) + ggtitle("Admission Rate for Men and Women")
ggplot(googleplaystore) + aes(x=Category, fill=Installs) + geom_bar(position = 'identity', alpha = .3,width = 0.5)+ 
  theme( axis.text.x = element_text(size=10))+ scale_x_discrete(labels = abbreviate)
ggplot(googleplaystore) + aes(x= Category, color = Installs) + scale_x_discrete(labels = abbreviate)+ facet_grid(.~Type) + geom_bar(position='dodge')

new$Scale[new$Rating>=0 & new$Rating<= 0.9] = "0-0.9"
new$Scale[new$Rating>=1 & new$Rating<= 1.9] = "1-1.9"
new$Scale[new$Rating>=2 & new$Rating<= 2.9] = "2-2.9"
new$Scale[new$Rating>=3 & new$Rating<= 3.9] = "3-3.9"
new$Scale[new$Rating>=4 & new$Rating<= 4.9] = "4-4.9"
new$Scale[new$Rating == 5] = "5"
str(new$Scale)

str(new)
new$Scale = factor(new$Scale, ordered = TRUE, levels = c("0-0.9", "1-1.9", "2-2.9", "3-3.9","4-4.9", "5"  ))
new$install_Number[new$Installs =="1+" |new$Installs == "5+" |new$Installs == "10+" |new$Installs == "50+" |new$Installs == "100+"] = "1 - 100"
new$install_Number[new$Installs == "500+" |new$Installs == "1,000+" |new$Installs == "5,000+" |new$Installs == "10,000+" |new$Installs == "50,000+"] = "500 - 50,000"
new$install_Number[new$Installs == "100,000+" |new$Installs == "500,000+" |new$Installs == "1,000,000+" |new$Installs == "5,000,000+" |new$Installs == "10,000,000+"] = "100,000 - 10,000,000"
new$install_Number[new$Installs == "50,000,000+" |new$Installs == "100,000,000+" |new$Installs == "500,000,000+" |new$Installs == "1,000,000,000+"] = "50,000,000 - 1,000,000,000"

str(new$install_Number)
new$install_Number = factor(new$install_Number, ordered = TRUE, levels = c("1 - 100", "500 - 50,000", "100,000 - 10,000,000","50,000,000 - 1,000,000,000"  ))
str(new$Size)
new$Size
new$NewSize[new$Size>=0 & new$Size<=10000] = "0k-1M"

new %>% group_by(Rating[4.0:4.9])


ggplot(new) + aes(x = Category, fill = install_Number)  +
  geom_bar(position='dodge') + ggtitle("Install Numbers in different Category")




###################################
###START
###################################
library(plyr)
install.packages(plyr)


new$Scale[new$Rating>=0 & new$Rating<= 0.9] = "0-0.9"
new$Scale[new$Rating>=1 & new$Rating<= 1.9] = "1-1.9"
new$Scale[new$Rating>=2 & new$Rating<= 2.9] = "2-2.9"
new$Scale[new$Rating>=3 & new$Rating<= 3.9] = "3-3.9"
new$Scale[new$Rating>=4 & new$Rating<= 4.9] = "4-4.9"
new$Scale[new$Rating == 5] = "5"
str(new$Scale)

str(new)
new$Scale = factor(new$Scale, ordered = TRUE, levels = c("0-0.9", "1-1.9", "2-2.9", "3-3.9","4-4.9", "5"  ))
new$install_Number[new$Installs =="1+" |new$Installs == "5+" |new$Installs == "10+" |new$Installs == "50+" |new$Installs == "100+"] = "1 - 100"
new$install_Number[new$Installs == "500+" |new$Installs == "1,000+" |new$Installs == "5,000+" |new$Installs == "10,000+" |new$Installs == "50,000+"] = "500 - 50,000"
new$install_Number[new$Installs == "100,000+" |new$Installs == "500,000+" |
                     new$Installs == "1,000,000+" |new$Installs == "5,000,000+" |new$Installs == "10,000,000+"] = "100,000 - 10,000,000"

new$install_Number[new$Installs == "50,000,000+" |new$Installs == "100,000,000+" |new$Installs == "500,000,000+" |new$Installs == "1,000,000,000+"] = "50,000,000 - 1,000,000,000"

str(new$install_Number)
new$install_Number = factor(new$install_Number, ordered = TRUE, levels = c("1 - 100", "500 - 50,000", "100,000 - 10,000,000","50,000,000 - 1,000,000,000"  ))
str(new$Size)
new$Size



# For basic information
ggplot(new) + aes(x = Category, fill = install_Number)  +
  geom_bar(position='dodge') + ggtitle("Install Numbers in different Category")

#For finding the effect from type(paid or free)
new = googleplaystore %>% filter(Category %in% c("GAME", "BUSINESS", "FINANCE", "EDUCATION","SHOPPING"))
str(new)
ggplot(new) + aes(x= Category, fill = install_Number) + 
  scale_x_discrete(labels = abbreviate)+ facet_grid(.~Type) + 
  geom_bar(position='dodge') + 
  ggtitle("Relation Between Install Numbers in 5 Areas and Tpye(Free and Paid) ")


###For Rating'
#ggplot(new) +aes(x = Category, y = Scale, fill = install_Number)+ scale_x_discrete(labels = abbreviate) + geom_col(position='dodge')

ggplot(new) + aes(x= Category, fill = install_Number) + 
  scale_x_discrete(labels = abbreviate)+ facet_grid(.~Scale) + 
  geom_bar(position='dodge')+
   ggtitle("Relation Between Install Numbers in 5 Areas and Rating ")

##################3
ggplot(new) + aes(x= Scale, fill = install_Number) + 
  scale_x_discrete(labels = abbreviate)+ facet_grid(.~Category) + 
  geom_bar(position='fill')+
  ggtitle("Relation Between Install Numbers in 5 Areas and Rating ")

tab = table(new$Scale, new$install_Number)
chisq.test(tab)

###For contating Rating
#H0 :  Installs Numbers doesn't depends on Content Rating
#H1 : Installs Numbers does depends on Content Rating
ggplot(new) + aes(x= Category, fill = install_Number) + 
  scale_x_discrete(labels = abbreviate)+ 
  facet_grid(.~Content.Rating) + 
  geom_bar(position='dodge')+ 
  ggtitle("Relation Between Install Numbers in 5 Areas and Content Rating ")



str(new$Size)
### For size
ggplot(new)+ aes(x = Category, y = Size, fill = install_Number) + scale_x_discrete(labels = abbreviate) + geom_col(position='dodge')

ggplot(new) + aes(x= Category, color = Installs) + scale_x_discrete(labels = abbreviate)+ facet_grid(.~log(as.numeric(Size))) + geom_bar(position='dodge')

### For Reviews

ggplot(new) + 
  aes(x= Category, y = as.numeric(as.character(new$Reviews)), fill = install_Number) + 
  scale_x_discrete(labels = abbreviate)+  
  geom_col(position='dodge')+ 
  ggtitle("Relation Between Install Numbers in 5 Areas and Reviews Numbers ")+
  ylab("Review Numbers")



######################
as.numeric(new$Reviews)
as.numeric(as.character(new$Size))
new$Size
str(as.numeric(as.character(new$Size)))









































