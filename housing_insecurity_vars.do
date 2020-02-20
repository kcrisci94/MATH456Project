 *creating vars for merged data
  use "/Users/jwilking/Box/Housing Insecurity Study Fall 2019 Data - Level 1/Merged Demographics and Survey Responses.dta"

*label var studyID is in this data, and it is one I created to match with Fin Aid, but we did not end up using it. It is dropped in the 
*subsequent dataset (data for finaid)
 
sort StartDate
gen directoutreach=.
replace directoutreach=1 in 1099/1416
replace directoutreach=0 in 1/1098
label var directoutreach "contacted student directly via email to take survey"
label values directoutreach directoutreachlbl
label define directoutreachlbl 0 "took survey via sampled class" 1 "student directly contacted by research team to take survey" 

 generate Ethnicity2=.
replace Ethnicity2=1 if Ethnicity=="White - Non-Hispanic"
replace Ethnicity2=2 if Ethnicity == "Hispanic/Latino (any race)"
replace Ethnicity2=3 if Ethnicity == "American Indian/Alaskan Native Only (American Indian) - Non-Hispanic"
replace Ethnicity2=4 if Ethnicity == "Asian - Non-Hispanic"
replace Ethnicity2=5 if Ethnicity == "Black/African American - Non-Hispanic"
replace Ethnicity2=6 if Ethnicity == "Native Hawaiian/Other Pacific Islander - Non-Hispanic"
replace Ethnicity2=7 if Ethnicity == "Two or More Races - Non-Hispanic"
replace Ethnicity2=8 if Ethnicity == "Unknown" 
label var Ethnicity2 "Ethnicity wo missing"
label values Ethnicity2 Ethnicity2lbl
label define Ethnicity2lbl 1 "White" 2 "Hispanic/Latino" 3 "Native American" 4 "Asian" 5 "Black/African-American" 6 "Pacific Islander" 7 "two or more races" 8 "Unknown" 

gen age = (mdy(10,01,2019) - BIRTH_DT)/365.25
gen age_rounded=age
replace age_rounded=round(age_rounded, 1)
gen age_categ=age_rounded
recode age_categ (min/17=1)
recode age_categ (18/19=2)
recode age_categ (20/21=3)
recode age_categ (22/24=4)
recode age_categ (25/29=5)
recode age_categ (30/34=6)
recode age_categ (35/49=7)
recode age_categ (50/64=8)
recode age_categ (65/max=9)
label var age_categ "ages, categorical"
label values age_categ age_categlbl
label define age_categlbl 1 "17 and under" 2 "18-19" 3 "20-21" 4 "22-24" 5 "25-29" 6 "30-34" 7 "35-49" 8 "50-64" 9 "65+"

tabulate Ethnicity, generate (ethnic)
rename ethnic1 NativeAmerican
rename ethnic2 Asian
rename ethnic3 AfricanAmerican
rename ethnic4 Hispanic
rename ethnic5 PacificIslander
rename ethnic6 missingethnicity
rename ethnic7 multiracial
rename ethnic8 unknownethnicity
rename ethnic9 Caucasian

replace NativeAmerican =. if missingethnicity==1
replace Asian =. if missingethnicity==1
replace AfricanAmerican =. if missingethnicity==1
replace Hispanic =. if missingethnicity==1
replace PacificIslander =. if missingethnicity==1
replace multiracial =. if missingethnicity==1
replace Caucasian =. if missingethnicity==1

gen nonwhite=.
replace nonwhite=1 if NativeAmerican==1
replace nonwhite=1 if Asian==1
replace nonwhite=1 if AfricanAmerican==1
replace nonwhite=1 if PacificIslander==1
replace nonwhite=1 if multiracial==1
replace nonwhite=1 if unknownethnicity==1
replace nonwhite=0 if Caucasian==1
replace nonwhite=1 if Hispanic==1
replace nonwhite =. if missingethnicity==1


label var ChicoCumltvGPA "Chico Cumulative GPA" 
label var HS_GPA "High School GPA"
label var TRF_GPA "Transfer GPA"

tabulate Citizenship, generate (citizen)
rename citizen6 USCitizen
drop citizen1-citizen5


gen FirstGenIR =.
replace FirstGenIR=1 if FirstGen =="Y"
replace FirstGenIR=0 if FirstGen=="N"
*the 74 missing variables in the IR data stay missing 



gen differentlyabled=D2_1
gen firstgenselfreport=D2_2
gen foster=D2_3 
label var foster "former foster youth"
gen homelessyouth=D2_4
label var homelessyouth "experienced homelessness before 18"
gen ESL =D2_5
label var ESL "English as a Second Language learner"
gen veteran = D2_6
gen activeduty = D2_7
gen studentathlete = D2_8
gen studentparent =D2_9
gen nontrad = D2_10 
label var nontrad "non-traditional student"

recode differentlyabled .=0
recode firstgenselfreport .=0
recode foster .=0
recode homelessyouth .=0
recode ESL .=0
recode veteran .=0
recode activeduty .=0
recode studentathlete .=0
recode studentparent .=0
recode nontrad .=0

replace differentlyabled =. if D2_12==1
replace firstgenselfreport =. if D2_12==1
replace foster =. if D2_12==1
replace homelessyouth =. if D2_12==1
replace ESL =. if D2_12==1
replace veteran =. if D2_12==1
replace activeduty =. if D2_12==1
replace studentathlete =. if D2_12==1
replace studentparent =. if D2_12==1
replace nontrad =. if D2_12==1

gen heterosexual=D3
recode heterosexual 2 3 4 5 = 0 6 = .

gen sexualmin=D3
recode sexualmin 1=0 2 3 4 5 =1 6=.

gen female = D4
recode female 1 3 =0 2=1 4 5 =.

gen transgender = D4
recode transgender 3=1 1 2 = 0 4 5 =.

gen employed=D5
recode employed 2 3 4 5 6=1 1=0
gen hoursworked = D5
recode hoursworked  1=0 2=1 3=2 4=3 5=4 6=5
label var hoursworked "Hours worked per week" 
label values hoursworked hoursworkedlbl 
label define hoursworkedlbl 1 "10 hours or less" 2  "10-19 hours" 3 "20-29 hours" 4 "30-39 hours" 5 "40 hours or more" 

gen employedpaid=employed
replace employedpaid =0 if D6==1

gen employedordinal=D5
recode employedordinal 1=0 2 3=1 4 5 6 =2
replace employedordinal=0 if D6==1
label var employedordinal "work for money and hours"
label values employedordinal employedordinallbl
label define employedordinallbl 0 "does not work for pay" 1 "1-19 hours per week" 2 "20+ hours per week"

gen employed3=D5
recode employed3 1=0 
replace employed3=0 if D6==1
recode employed3 2=1 3=2 4=3 5=4 6=4


gen moderatework=employedordinal
recode moderatework 2=0

gen intensivework=employedordinal
recode intensivework 1=0 2=1





gen helpfamfin=D7
recode helpfamfin 1=1 2=0 3=.
label var helpfamfin "Do you help to financially support your dependents or other family members?"

gen rentorown=H1
label var rentorown "Do you own or rent your current residence?"
label values rentorown rentorownlbl
label define rentorownlbl 1 "own" 2 "rent" 3 "owned by parents or other relatives" 4 "rented by parents or other relatives" 5 "I do not currently have stable housing" 

gen rentdummy=rentorown
recode rentdummy 1 3 4 5=0 2=1

gen housingcosts=H2
label var housingcosts "% of income used for housing expenses" 
label values housingcosts housingcostslbl
label define housingcostslbl 1 "uses none of income to pay housing costs" 2 "Less than 30% of monthly budget"  3 "about 30% of monthly budget" 4  "between 30-50% of monthly budget" 5 "more than 50% of montly budget" 

gen housingburdendummy=H2
recode housingburdendummy 4 5 =1 1 2 3=0
label var housingburdendummy "Pays 30% or more in housing expenses per month"

tab H3, generate (whopayshousing)
rename whopayshousing4 otherspayhousing
rename whopayshousing1 studentpaysallhousing
replace studentpaysallhousing =1 if whopayshousing2 ==1
replace studentpaysallhousing =. if whopayshousing5==1

gen sharedexpenses=H3
recode sharedexpenses 1 2 4 5=0 3=1

gen difficultrentincrease = H4
recode difficultrentincrease 2=0 
label var difficultrentincrease "Rent or mortgage increase that made it difficult to pay" 
labe values difficultrentincrease difficultrentincreaselbl
label define difficultrentincreaselbl 1 "Yes" 0 "No" 

gen  unabletopayrent = H5
gen laterent = H6
gen unabletopaybill = H7
gen exceedcapacity = H8
gen askedtoleave =H9
gen evicted = H10
gen unsafehsg = H11

recode unabletopayrent 2=0
recode laterent 2=0
recode unabletopaybill 2=0
recode exceedcapacity 2=0
recode askedtoleave 2=0 
recode evicted 2=0
recode unsafehsg 2=0

label var unabletopayrent "Unable to pay or underpaid your rent or mortgage?"
label var laterent "Late paying your rent or mortgage?"
label var unabletopaybill "Unable to pay or underpaid a utility bill?"
label var exceedcapacity "Have you lived with others beyond the expected capacity of your house or apartment?"
label var askedtoleave "Have you been asked to leave your home by someone you lived with?"
label var evicted "Have you been evicted?"
label var unsafehsg "Have you had to stay in a hostile housing environment or abusive relationship bc you had no other place to live?"

label values unabletopayrent unabletopayrentlbl 
label define unabletopayrentlbl 0 "No" 1 "Yes"
label values laterent laterentlbl
label define laterentlbl 0 "No" 1 "Yes"
label values unabletopaybill unabletopaybilllbl
label define unabletopaybilllbl 0 "No" 1 "Yes"
label values exceedcapacity exceedcapacitylbl
label define exceedcapacitylbl 0 "No" 1 "Yes"
label values askedtoleave askedtoleavelbl
label define askedtoleavelbl 0  "No" 1 "Yes"
label values evicted evictedlbl
label define evictedlbl 0 "No" 1 "Yes"
label values unsafehsg unsafehsglbl
label define unsafehsglbl 0 "No" 1 "Yes"


gen unsuresleeping=H12
recode unsuresleeping 1=0 2=1 3=2 4=3 
label var unsuresleeping "How many times have you been unsure of where you were going to sleep at night" 
label values unsuresleeping unsuresleepinglbl
label define unsuresleepinglbl 0 "None" 1 "once" 2 "twice" 3 "three or more times" 

gen unsuresleepingdummy=H12
 recode unsuresleepingdummy 1=0 2 3 4 =1

gen numberofmoves=H13
recode numberofmoves 1=0 2=1 3=2 4=3 5=4
label var numberofmoves "How many times have you moved in the past 12 months" 
label values numberofmoves numberofmoveslbl
label define numberofmoveslbl 0 "none" 1 "once" 2 "twice" 3 "three to five times" 4 "6 times or more" 

egen housingindex= rowtotal (unabletopayrent laterent unabletopaybill exceedcapacity askedtoleave evicted unsafehsg unsuresleepingdummy), missing
label var housingindex "Index of housing insecurity measures"

gen HIdummy1=housingindex
recode HIdummy1 (2/max=1)

gen HIdummy2=housingindex
recode HIdummy2 0 1=0 
recode HIdummy2 (2/max=1)

gen HIdummy3=housingindex
recode HIdummy3 0 1 2=0 
recode HIdummy3 (3/max=1)

gen oncampus=H14_1
recode oncampus 1 2 =1 3=0

gen shelter30=H14_5 
label var shelter30 "Stayed in a shelter in past 30 days"
 gen couchsurf30=H14_6
 label var couchsurf30 "Temporarily stayed with friend of family member until R. could find other hsg - past 30 days"
 gen hotmot30=H14_7
 label var hotmot30 "Stayed in a hotel or motel without having a permanent home, in past 30 days"
 gen transithousing30=H14_8
 label var transithousing30 "Stayed in a transitional housing program, in past 30 days"
 gen grouphome30=H14_9
 label var grouphome30 "Stayed in a grouphome, in past 30 days"
 gen outdoor30 = H14_10
 label var outdoor30 "Stayed in an outdoor location, in past 30 days"
 gen car30= H14_11
 label var car30 "Stayed in a car, truck, van, RV or Camper, in past 30 days"
 gen bldg30=H14_12
 label var bldg30 "Stayed in a closed area or space not meant for human habitation, in past 30 days"
 
 recode shelter30 2 3 =0
 recode  couchsurf30 2 3 =0
 recode  hotmot30 2 3 =0
 recode transithousing30 2 3 =0
 recode grouphome30 2 3 =0
 recode outdoor30 2 3 =0
 recode  car30 2 3 =0
 recode  bldg30 2 3 =0
 
 egen homeless30 = rowtotal (shelter30 couchsurf30 hotmot30 transithousing30 grouphome30 outdoor30 car30 bldg30), missing 
 recode homeless30 (2/max=1)  
 label var homeless30 "Experienced homelessness in past 30 days"
 
 gen shelter12=H14_5
 label var shelter12 "Stayed in a shelter in past 12 mos"
 gen couchsurf12=H14_6
 label var couchsurf12 "Temporarily stayed with friend of family member until R. could find other hsg - past 12 mos"
 gen hotmot12=H14_7
 label var hotmot12 "Stayed in a hotel or motel without having a permanent home, in past 12 mos"
 gen transithousing12=H14_8
 label var transithousing12 "Stayed in a transitional housing program, in past 12 mos"
 gen grouphome12=H14_9
 label var grouphome12 "Stayed in a grouphome, in past 12 mos"
 gen outdoor12 = H14_10
 label var outdoor12 "Stayed in an outdoor location, in past 12 mos"
 gen car12= H14_11
 label var car12 "Stayed in a car, truck, van, RV or Camper, in past 12 mos"
 gen bldg12=H14_12
 label var bldg12 "Stayed in a closed area or space not meant for human habitation, in past 12 mos"
 
 recode shelter12 1 3 =0 2=1
 recode  couchsurf12 1 3 =0 2=1
 recode  hotmot12 1 3 =0 2=1
 recode transithousing12 1 3 =0 2=1
 recode grouphome12 1 3 =0 2=1
 recode outdoor12 1 3 =0 2=1
 recode  car12 1 3 =0 2=1
 recode  bldg12 1 3 =0 2=1
 
 egen homeless12 = rowtotal (shelter12 couchsurf12 hotmot12 transithousing12 grouphome12 outdoor12 car12 bldg12), missing 
 recode homeless12 (2/max=1)  
 label var homeless12 "Experienced homelessness in the past 12 mos"
 
 gen shelterall=H14_5
 label var shelterall "Stayed in a shelter in past 30 days or 12 mos"
 gen couchsurfall=H14_6
 label var couchsurfall "Temporarily stayed with family or friend  in past 30 days or 12 mos"
 gen hotmotall=H14_7
 label var hotmotall "Stayed in a hotel or motel wo having permanent home in past 30 days or 12 mos"
 gen transithousingall=H14_8
 label var transithousingall "Stayed in transitional housing in past 30 days or 12 mos"
 gen grouphomeall=H14_9
 label var grouphomeall "Stayed in a grouphome in past 30 days or 12 mos"
 gen outdoorall = H14_10
 label var outdoorall "Stayed in an outdoor location in past 30 days or 12 mos"
 gen carall= H14_11
 label var carall "Stayed in a car, truck, van, RV or camper, in past 30 days or 12 mos"
 gen bldgall=H14_12
 label var bldgall "Stayed in a closed area or space not meant for human habitation in past 30 days or 12 mos"
 
 
 recode shelterall 2=1 3=0
 recode couchsurfall 2=1 3=0
 recode hotmotall 2=1 3=0
 recode transithousingall 2=1 3=0
 recode grouphomeall 2=1 3=0
 recode outdoorall 2=1 3=0
 recode carall 2=1 3=0
 recode bldgall 2=1 3=0
 
 *gen nonuseprograms=.
*replace nonuseprograms=1 if S3_1==1
*replace nonuseprograms=2 if S3_2==1
*replace nonuseprograms=3 if S3_3==1
*replace nonuseprograms=4 if S3_4==1
*replace nonuseprograms=5 if S3_5==1
*replace nonuseprograms=6 if S3_6==1
*replace nonuseprograms=7 if S3_7==1
*replace nonuseprograms=8 if S3_8==1
*replace nonuseprograms=9 if S3_9==1
*this is a check all that apply question, and when I create the question this way, only unique responses are being included, so I am going to provide just the tallies for eac individual question

*label var nonuseprograms "reasons students do not use existing services"
*label values nonuseprograms nonuseprogramslbl
*label define nonuseprogramslbl 1 "already use programs" 2 "do not need assistance" 3 "do not know how to access" 4 "not eligible" 5 "others have greater need" 6 "do not believe in social services" 7 "uncomfortable disclosing my need" 8 "lack transportation to access the program" 9 "other"






 egen homelessall = rowtotal (shelterall couchsurfall hotmotall transithousingall grouphomeall outdoorall carall bldgall), missing 

 recode homelessall (2/max=1)  
 label var homelessall "Experienced homelessness in past 30 days or 12 mos"
 
 rename H15 HIreasons
 label var HIreasons "Reasons given for housing insecurity"
 
 gen interpersonal=HIreasons
recode interpersonal 4=1 1 2 3 5 6 7 =0

 
 gen wildcatfoodpantry=.
 replace wildcatfoodpantry =1 if S2_1_1==1
 replace wildcatfoodpantry =2 if S2_1_2==1
 replace wildcatfoodpantry =3 if S2_1_3==1
 replace wildcatfoodpantry =4 if S2_1_4==1
 
 label var wildcatfoodpantry "Awarness and use of wildcatfoodpantry"
 label values wildcatfoodpantry wildcatfoodpantrylbl
 label define wildcatfoodpantrylbl 1 "Unwaware of resource" 2 "Aware, but do not use" 3 " Have used in the past" 4 "currently use" 
 gen wildcatfoodpantrydummy =wildcatfoodpantry
 recode wildcatfoodpantrydummy 1=0 2 3 4=1
 
 gen basicneedsproj=.
 replace basicneedsproj =1 if S2_2_1==1
 replace basicneedsproj =2 if S2_2_2==1
 replace basicneedsproj =3 if S2_2_3==1
 replace basicneedsproj =4 if S2_2_4==1
 
 label var basicneedsproj "Awarness and use of Basic Needs Project"
 label values basicneedsproj basicneedsprojlbl
 label define basicneedsprojlbl 1 "Unwaware of resource" 2 "Aware, but do not use" 3 " Have used in the past" 4 "currently use" 
 
 gen basicneedsprojdummy=basicneedsproj
 recode basicneedsprojdummy 1=0 2 3 4=1
 
 gen offcampusss=.
 replace offcampusss =1 if S2_3_1==1
 replace offcampusss =2 if S2_3_2==1
 replace offcampusss =3 if S2_3_3==1
 replace offcampusss =4 if S2_3_4==1
 
 label var offcampusss "Awarness and use of Off Campus Student Services"
 label values offcampusss offcampussslbl
 label define offcampussslbl 1 "Unwaware of resource" 2 "Aware, but do not use" 3 " Have used in the past" 4 "currently use" 
 
 gen offcampusssdummy=offcampusss
 recode offcampusssdummy 1=0 2 3 4=1
 
 gen studentemergencygrant=.
 replace studentemergencygrant =1 if S2_4_1==1
 replace studentemergencygrant =2 if S2_4_2==1
 replace studentemergencygrant =3 if S2_4_3==1
 replace studentemergencygrant =4 if S2_4_4==1
 
 label var studentemergencygrant "Awarness and use of student emergency grant"
 label values studentemergencygrant studentemergencygrantlbl
 label define studentemergencygrantlbl 1 "Unwaware of resource" 2 "Aware, but do not use" 3 " Have used in the past" 4 "currently use" 
 
 gen studentemergencygrantdummy=studentemergencygrant
 recode studentemergencygrantdummy 1=0 2 3 4=1
 
 gen emerghousing=.
 replace emerghousing=1 if S2_5_1==1
 replace emerghousing=2 if S2_5_2==1
 replace emerghousing=3 if S2_5_3==1
 replace emerghousing=4 if S2_5_4==1
 
 label var emerghousing "Awarness and use of emergency housing"
 label values emerghousing emerghousinglbl
 label define emerghousinglbl 1 "Unwaware of resource" 2 "Aware, but do not use" 3 " Have used in the past" 4 "currently use" 
 
 gen emerghousingdummy=emerghousing
 recode emerghousingdummy 1=0 2 3 4=1
 
 gen FAadvisor=.
 replace FAadvisor=1 if S2_6_1==1
 replace FAadvisor=2 if S2_6_2==1
 replace FAadvisor=3 if S2_6_3==1
 replace FAadvisor=4 if S2_6_4==1
 
 label var FAadvisor "Awarness and use of financial aid advising"
 label values FAadvisor FAadvisorlbl
 label define FAadvisorlbl 1 "Unwaware of resource" 2 "Aware, but do not use" 3 " Have used in the past" 4 "currently use" 
 
 gen FAadvisordummy=FAadvisor
 recode FAadvisordummy 1=0 2 3 4=1
 
 egen servicesindex= rowtotal (wildcatfoodpantrydummy basicneedsprojdummy offcampusssdummy studentemergencygrantdummy emerghousingdummy FAadvisordummy), missing
 
 gen useprogramsdummy=S3_1
 gen servnoneed = S3_2
 gen servnoaccess = S3_3
 gen servnoteligible = S3_4
 gen servothersneed = S3_5
 gen servnobelief = S3_6
 gen servstigma = S3_7
 gen servtransport = S3_8
 
 recode useprogramsdummy .=0
 recode servnoneed .=0
 recode servnoaccess .=0
 recode servnoteligible .=0
 recode servothersneed .=0
 recode servnobelief .=0
 recode servstigma .=0
 recode servtransport .=0
 
label var useprogramsdummy "If you don't use progams Why? - R. already uses the programs"
label var servnoneed "If you don't use progams Why? R. does not need assistance"
label var servnoaccess "If you don't use progams Why? R. does not know how to access the programs"
label var servnoteligible "If you don't use progams Why? R. is not eligible for the programs"
label var servothersneed "If you don't use progams Why? R. believes others need the service more"
label var servnobelief "If you don't use progams Why? R. does not believe in social services"
label var servstigma "If you don't use progams Why? R. is uncomfortable disclosing need for the program"
label var servtransport "If you don't use progams Why? R. does not have transportation to access program"

rename S5 creditcarddebt

tab CF2, generate (city)

rename city1 CFChico
label var CFChico "Lived in Chico during Camp Fire"
rename city2 CFFireaffectedarea
label var CFFireaffectedarea "Lived in a fire affected area during Camp Fire"
rename city3 CFotherinBC
label var CFotherinBC "Lived in another community inside Butte County during Camp Fire, e.g. Biggs, Gridley"
rename city4 CFotheroutBC
label var CFotheroutBC "Lived in a community outside of Butte County during the Camp Fire" 


gen campfireimpact=.
replace campfireimpact=1 if CF3_4==1
replace campfireimpact=2 if CF3_3==1
replace campfireimpact=3 if CF3_2==1
replace campfireimpact=4 if CF3_1==1
replace campfireimpact=5 if CF3_5==1

gen CFpermmove=CF3_1
gen CFtempmove=CF3_2
gen CFexpenseincrease=CF3_3
gen CFnoimpact=CF3_4

recode CFpermmove .=0
recode CFtempmove .=0
recode CFexpenseincrease .=0
recode CFnoimpact .=0 

gen CFimpact=.
replace CFimpact=1 if CF3_1==1
replace CFimpact=1 if CF3_2==1
replace CFimpact=1 if CF3_3==1
replace CFimpact=0 if CF3_4==1
replace CFimpact=1 if CF3_5==1

label var campfireimpact "Impact of Camp Fire on Housing"
label values campfireimpact campfireimpactlbl
label define campfireimpactlbl 1 "No impact" 2 "Housing expenses increased" 3 "Temporarily moved" 4 "Permanently moved" 5 "Other"  

rename CF4 reasonsformove
recode reasonsformove 6=.
 
gen findhousing= CF7 
gen findaffhsg=CF8
gen housingstress=CF9
gen leaveChicohsg=CF10

label var findhousing "I easily found housing for the current academic year"
label var findaffhsg "I easily found housing I can afford for the current academic year"
label var housingstress "Finding housing has been a major source of stress in attending Chico State" 
label var leaveChicohsg "I have considered leaving Chico State due to difficulties in finding affordable housing" 

recode findhousing 6=.
recode findaffhsg 6=.
recode housingstress 6=.
recode leaveChicohsg 6=.

label values findhousing findhousinglbl
label define findhousinglbl 1 "Strongly agree" 2 "Agree" 3 "Neither agree nor disagree" 4 "Disagree" 5 "Strongly disagree"
label values findaffhsg findaffhsglbl
label define findaffhsglbl 1 "Strongly agree" 2 "Agree" 3 "Neither agree nor disagree" 4 "Disagree" 5 "Strongly disagree"
label values housingstress housingstresslbl
label define housingstresslbl 1 "Strongly agree" 2 "Agree" 3 "Neither agree nor disagree" 4 "Disagree" 5 "Strongly disagree"
label values leaveChicohsg leaveChicohsglbl
label define leaveChicohsglbl 1 "Strongly agree" 2 "Agree" 3 "Neither agree nor disagree" 4 "Disagree" 5 "Strongly disagree"

gen findhousingdummy=findhousing
recode findhousingdummy 4 5 =1 1 2 3=0
gen findaffhsgdummy=findaffhsg
recode findaffhsgdummy 4 5 =1 1 2 3=0
gen housingstressdummy=housingstress
recode housingstressdummy 1 2=1 3 4 5=0
gen leaveChicohsgdummy=leaveChicohsg
recode leaveChicohsgdummy 1 2=1 3 4 5=0

egen negperchousing = rowtotal (findhousingdummy findaffhsgdummy housingstressdummy leaveChicohsgdummy), missing 
label var negperchousing "Negative perceptions of finding housing in Chico - higher values are more negative"
 
gen genhealth=W2
label var genhealth "Describe your overall health" 
recode genhealth 1=5 2=4 3=3 4=2 5=1 
label values genhealth genhealthlbl
label define genhealthlbl 1 "Poor" 2 "Fair" 3 "Good" 4 "Very good" 5 "Excellent"

gen physicalhealthdays=W3
label var physicalhealthdays "For how many days (in the past 30) has your physical health been not good?"
label values physicalhealthdays physicalhealthdayslbl
recode physicalhealthdays 1=0 2=1 3=2 4=3
label define physicalhealthdayslbl 0 "0 days" 1 "1-2 days" 2 "3-4 days" 3 "5 or more days"

gen mentalhealthdays=W4
label var mentalhealthdays "For how many days (in the past 30) has your mental health been not good?"
label values mentalhealthdays mentalhealthdayslbl
recode mentalhealthdays 1=0 2=1 3=2 4=3
label define mentalhealthdayslbl 0 "0 days" 1 "1-2 days" 2 "3-4 days" 3 "5 or more days"

gen poorhealthdays=W5
label var poorhealthdays "For how many days (in the past 30) has your physical and or mental health kept you from doing your usual activities" 
label values poorhealthdays poorhealthdayslbl
recode poorhealthdays 1=0 2=1 3=2 4=3
label define poorhealthdayslbl 0 "0 days" 1 "1-2 days" 2 "3-4 days" 3 "5 or more days"

gen famincome=.
replace famincome=1 if DependentIncome == "24,000 or less" 
replace famincome=2 if DependentIncome == "24,000 - 35,999" 
replace famincome=3 if DependentIncome == "36,000 - 47,999" 
replace famincome=4 if DependentIncome == "48,000 - 59,999" 
replace famincome=5 if DependentIncome == "60,000 - 71,999" 
replace famincome=6 if DependentIncome == "72,000 or more"
replace famincome=. if DependentIncome == "No response" 
replace famincome=. if DependentIncome == "Other" 

label var famincome "income of family if student is a dependent"
label values famincome famincomelbl
label define famincomelbl 1 "$24,000 or less" 2 "24,000-35,999" 3 "36,000-47,999" 4 "$48,000-59,999" 5 "$60,000-71,999" 6 "$72,000 or more"

gen incomecateg=famincome
replace income=1 if IndepIncome=="6,000 or less"
replace income=1 if IndepIncome=="6,000 - 11,999"
replace income =1 if IndepIncome=="12,000 - 23,999"
replace income=2 if IndepIncome=="24,000 - 35,999"
replace income=3 if IndepIncome == "36,000 - 47,999" 
replace income=4 if IndepIncome == "48,000 - 59,999" 
replace income=5 if IndepIncome == "60,000 or more"


tab AcademicLvl, generate (classstanding)
rename classstanding1 Freshmen

rename classstanding5 Sophomore
rename classstanding2 Junior
rename classstanding4 Senior
rename classstanding3 PostBaccUG

tab AdmitType, generate (AdmitType)
rename AdmitType1 Firsttimestudents
rename AdmitType4 Transferstudents
replace Transferstudents =1 if AdmitType3==1

gen independent=.
replace independent =1 if IndepIncome != "No response"
recode independent .=0

gen incomeinterval=. 
replace incomeinterval = 3000 if IndepIncome == "6,000 or less"
replace incomeinterval = 6000 if IndepIncome=="6,000 - 11,999"
replace incomeinterval = 18000 if IndepIncome=="12,000 - 23,999"
replace incomeinterval = 18000 if DependentIncome == "24,000 or less"
replace incomeinterval = 30000 if IndepIncome=="24,000 - 35,999" | DependentIncome=="24,000 - 35,999"
replace incomeinterval = 42000 if  IndepIncome == "36,000 - 47,999" | DependentIncome== "36,000 - 47,999"
replace incomeinterval = 54000 if  IndepIncome == "48,000 - 59,999" | DependentIncome== "48,000 - 59,999" 
replace incomeinterval = 66000 if IndepIncome== "60,000 or more"
replace incomeinterval = 66000 if DependentIncome == "60,000 - 71,999"
replace incomeinterval = 78000 if DependentIncome== "72,000 or more"

gen homepostalcode = substr(ResPostalCode, 1, 5)
gen buttecountyres=0
replace buttecountyres =1 if homepostalcode == "95914"
replace buttecountyres = 1 if homepostalcode == "95916"
replace buttecountyres = 1 if homepostalcode == "95917"
replace buttecountyres = 1 if homepostalcode == "95926"
replace buttecountyres = 1  if homepostalcode == "95927"
replace buttecountyres = 1 if homepostalcode == "95928"
replace buttecountyres = 1 if homepostalcode == "95930"
replace buttecountyres = 1 if homepostalcode == "95938"
replace buttecountyres = 1 if homepostalcode == "95940"
replace buttecountyres = 1 if homepostalcode == "95941"
replace buttecountyres = 1 if homepostalcode == "95942"
replace buttecountyres = 1 if homepostalcode == "95948"
replace buttecountyres = 1 if homepostalcode == "95954"
replace buttecountyres = 1 if homepostalcode == "95958"
replace buttecountyres = 1 if homepostalcode == "95965"
replace buttecountyres = 1 if homepostalcode == "95966"
replace buttecountyres = 1 if homepostalcode == "95967"
replace buttecountyres = 1 if homepostalcode == "95968"
replace buttecountyres = 1 if homepostalcode == "95969"
replace buttecountyres = 1 if homepostalcode == "95973"
replace buttecountyres = 1 if homepostalcode == "95974"
replace buttecountyres = 1 if homepostalcode == "95978"
replace buttecountyres = . if homepostalcode == "     "

gen clusterID=.
replace clusterID =	1	if ExternalReference == 	"ABUS41502"
replace clusterID =	2	if ExternalReference == 	"ABUS46401"
replace clusterID =	3	if ExternalReference == 	"ACCT20104"
replace clusterID =	4	if ExternalReference == 	"ACCT32001"
replace clusterID =	5	if ExternalReference == 	"ACCT32702"
replace clusterID =	6	if ExternalReference == 	"ACCT36501"
replace clusterID =	7	if ExternalReference == 	"ANTH11301"
replace clusterID =	8	if ExternalReference == 	"ANTH48701"
replace clusterID =	9	if ExternalReference == 	"ARTS12202"
replace clusterID =	10	if ExternalReference == 	"ARTS12504"
replace clusterID =	11	if ExternalReference == 	"ARTS22701"
replace clusterID =	12	if ExternalReference == 	"ARTS495W01"
replace clusterID =	13	if ExternalReference == 	"BADM300W03"
replace clusterID =	14	if ExternalReference == 	"BIOL10501"
replace clusterID =	15	if ExternalReference == 	"BLAW20302"
replace clusterID =	16	if ExternalReference == 	"CIVL31101"
replace clusterID =	17	if ExternalReference == 	"CIVL595W01"
replace clusterID =	18	if ExternalReference == 	"CMST13205"
replace clusterID =	19	if ExternalReference == 	"CMST13207"
replace clusterID =	20	if ExternalReference == 	"CMST23302"
replace clusterID =	21	if ExternalReference == 	"CMST339A01"
replace clusterID =	22	if ExternalReference == 	"CSCI301W01"
replace clusterID =	23	if ExternalReference == 	"ECON35202"
replace clusterID =	24	if ExternalReference == 	"EDTE52003"
replace clusterID =	25	if ExternalReference == 	"EDTE53002"
replace clusterID =	26	if ExternalReference == 	"ENGL130W18"
replace clusterID =	27	if ExternalReference == 	"ENGL130W25"
replace clusterID =	28	if ExternalReference == 	"ENGL47106"
replace clusterID =	29	if ExternalReference == 	"GEOS20301"
replace clusterID =	30	if ExternalReference == 	"GEOS300W01"
replace clusterID =	31	if ExternalReference == 	"GEOS39901"
replace clusterID =	32	if ExternalReference == 	"GEOS60601"
replace clusterID =	33	if ExternalReference == 	"HIST13501"
replace clusterID =	34	if ExternalReference == 	"HIST31301"
replace clusterID =	35	if ExternalReference == 	"HIST44601"
replace clusterID =	36	if ExternalReference == 	"JOUR10101"
replace clusterID =	37	if ExternalReference == 	"JOUR32001"
replace clusterID =	38	if ExternalReference == 	"KINE12305"
replace clusterID =	39	if ExternalReference == 	"KINE148A01"
replace clusterID =	40	if ExternalReference == 	"KINE20201"
replace clusterID =	41	if  ExternalReference == 	"KINE22201"
replace clusterID =	42	if ExternalReference == 	"KINE315S01"
replace clusterID =	43	if ExternalReference == 	"KINE335S01"
replace clusterID =	44	if ExternalReference == 	"KINE39003"
replace clusterID =	45	if ExternalReference == 	"KINE617S01"
replace clusterID =	46	if ExternalReference == 	"LAST38201"
replace clusterID =	47	if ExternalReference == 	"MADT26101"
replace clusterID =	48	if ExternalReference == 	"MADT28501"
replace clusterID =	49	if ExternalReference == 	"MATH10547"
replace clusterID =	50	if ExternalReference == 	"MATH19501"
replace clusterID =	51	if ExternalReference == 	"MCGS33001"
replace clusterID =	52	if ExternalReference == 	"MGMT30301"
replace clusterID =	53	if ExternalReference == 	"MGMT30408"
replace clusterID =	54	if ExternalReference == 	"MGMT44280"
replace clusterID =	55	if ExternalReference == 	"MGMT45002"
replace clusterID =	56	if ExternalReference == 	"MGMT47003"
replace clusterID =	57	if ExternalReference == 	"MUSC10502"
replace clusterID =	58	if ExternalReference == 	"MUSC110F01"
replace clusterID =	59	if ExternalReference == 	"OSCM30601"
replace clusterID =	60	if ExternalReference == 	"PHHA32102"
replace clusterID =	61	if ExternalReference == 	"PHYS204A05"
replace clusterID =	62	if ExternalReference == 	"PHYS204A09"
replace clusterID =	63	if ExternalReference == 	"PHYS204C01"
replace clusterID =	64	if ExternalReference == 	"POLS10201"
replace clusterID =	65	if ExternalReference == 	"POLS30101"
replace clusterID =	66	if ExternalReference == 	"POLS331W05"
replace clusterID =	67	if ExternalReference == 	"POLS331X02"
replace clusterID =	68	if ExternalReference == 	"POLS35101"
replace clusterID =	69	if ExternalReference == 	"POLS39902"
replace clusterID =	70	if ExternalReference == 	"POLS43701"
replace clusterID =	71	if ExternalReference == 	"POLS451B72"
replace clusterID =	72	if ExternalReference == 	"POLS45901"
replace clusterID =	73	if ExternalReference == 	"POLS680B01"
replace clusterID =	74	if ExternalReference == 	"PSYC34101"
replace clusterID =	75	if ExternalReference == 	"PSYC35504"
replace clusterID =	76	if ExternalReference == 	"PSYC673B01"
replace clusterID =	77	if ExternalReference == 	"PSYC68801"
replace clusterID =	78	if ExternalReference == 	"RELS40370"
replace clusterID =	79	if ExternalReference == 	"RHPM20001"
replace clusterID =	80	if ExternalReference == 	"RHPM20101"
replace clusterID =	81	if ExternalReference == 	"RHPM34201"
replace clusterID =	82	if ExternalReference == 	"RHPM42002"
replace clusterID =	83	if ExternalReference == 	"SOCI15701"
replace clusterID =	84	if ExternalReference == 	"SOCI33001"
replace clusterID =	85	if ExternalReference == 	"SOSC30272"
replace clusterID =	86	if ExternalReference == 	"SPAN10201"
replace clusterID =	87	if ExternalReference == 	"SPED34303"
replace clusterID =	88	if ExternalReference == 	"SWRK64201"
replace clusterID =	89	if ExternalReference == 	"THEA25101"

 gen ChicoGPA=ChicoCumltvGPA
 recode ChicoGPA 0=.
 
***
*replace ChicoGPA = ChicoCumltvGPA if AcademicLvl== "Sophomore"

*replace ChicoGPA = ChicoCumltvGPA if AcademicLvl== "Junior"

*replace ChicoGPA = ChicoCumltvGPA if AcademicLvl== "Senior"

*replace ChicoGPA = . if AdmitType== "Transfer"








