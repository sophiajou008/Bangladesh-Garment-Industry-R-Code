library(AER)
library(mfx)
library(stargazer)
library(readr)

#Open data files
EconPaperData_Bangladesh <- read_csv("EconPaperData - Bangladesh.csv")
View(EconPaperData_Bangladesh)
EconPaperData_Nepal <- read_csv("EconPaperData - Nepal.csv")
View(EconPaperData_Bangladesh)
EconPaperData_Panel <- read_csv("EconPaperData - Panel.csv")
View(EconPaperData_Panel)

#Running the simple regression (real minimum wages on real garment exports in BDT)
#RealWage = Real monthly garment minimum wage
#RealLovExports = Total value of annual garment industry exports in real local currency (billion BDT)
model1 <- lm(RealWage~ RealLocExports, data=EconPaperData_Bangladesh)

#Regression including controls
#BangladeshUnemploy = unemployment rate
#RealGDPPerCapitaLCU = real Bangladesh GDP per capita in 100 BDT
#RealBangladeshGDPLCU = real Bangladesh GDP in billion BDT 
#SecondarySchoolEnrollment = Percent of secondary school enrollment
model2 <- lm(RealWage~ RealLocExports+BangladeshUnemploy+RealGDPPerCapitaLCU+RealBangladeshGDPLCU+SecondarySchoolEnrollment, data=EconPaperData_Bangladesh)

#Same regressions but using data from Nepal
#All values are in Nepali dollars
#RealLocExports = Total value of annual garment industry exports in real local currency (billion Nepalese rupees)
#RealGDPPerCapitaLCU = Real GDP per capita in 100 Nepalese rupees
#RealNepalGDPLCU = Real GDP in billion Nepalese rupees
model3 <- lm(RealWage~ RealLocExports, data=EconPaperData_Nepal)
model4 <- lm(RealWage~ RealLocExports+NepalUnemploy+RealGDPPerCapitaLCU+RealNepalGDPLCU+SecondarySchoolEnrollment, data=EconPaperData_Nepal)

#Panel regressions with time and country fixed effects
modelfixed <- lm(RealMinWage ~ RealExports + factor(Country) + factor(Year), data = EconPaperData_Panel)
#Adding in controls
#RealMinWage = monthly minimum wage in real USD
#RealExports = Total value of annual garment industry exports in real billions USD
#Unemploy = unemployment rate
#School = percent of secondary schooling
#RealGDP = real 100 million USD
#RealGDPPC = real GDP per capita in USD
modelfixed3 <- lm(RealMinWage ~ RealExports + Unemploy + School + RealGDP + RealGDPPC + factor(Country) + factor(Year), data = EconPaperData_Panel)