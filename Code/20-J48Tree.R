library(RWeka)
library(partykit)
library(Rgraphviz)

dfCD = read.table(textConnection("end_rack,85,85,FALSE,yes
end_rack,80,90,TRUE,yes
cd_spec,83,86,FALSE,no
std_rack,70,96,FALSE,no
std_rack,68,80,FALSE,no
std_rack,65,70,TRUE,yes
cd_spec,64,65,TRUE,yes
end_rack,72,95,FALSE,yes
end_rack,69,70,FALSE,no
std_rack,75,80,FALSE,no
end_rack,75,70,TRUE,no
cd_spec,72,90,TRUE,no
cd_spec,81,75,FALSE,yes
std_rack,71,91,TRUE,yes"), header =  FALSE, sep = ",")
names(dfCD) = c("Placement", "prominence", "pricing", "eye_level", "customer_purchase")

# write as ARFF file
RWeka::write.arff(dfCD, file = "Data/CD.arff")

WOW("J48")
cd1 = J48(customer_purchase ~  pricing + Placement + eye_level, data = dfCD,
                 control = Weka_control(M = 2, C = 0.25))

summary(cd1)
write_to_dot(cd1, ff <- tempfile())
plot(agread(ff))
