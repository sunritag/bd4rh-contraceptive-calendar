infix using "C:\COUNTRIES\LB7A\LBFW7AFL.dct"

label variable fw101    "Fieldworker code"
label variable fw000    "Country code and phase"
label variable fw102    "In what county do you live"
label variable fw103    "Type of place of residence"
label variable fw104    "How old are you"
label variable fw105    "Are you male or female"
label variable fw106    "Marital status"
label variable fw107    "How many living children do you have"
label variable fw108    "Ever had a child who died"
label variable fw109    "Highest level of school attended"
label variable fw110    "Highest grade completed at that level"
label variable fw111    "What is your religion"
label variable fw112    "Fieldworker ethnicity"
label variable fw113a   "Bassa"
label variable fw113b   "Gbandi"
label variable fw113c   "Belle"
label variable fw113d   "Dey"
label variable fw113e   "Gio"
label variable fw113f   "Gola"
label variable fw113g   "Grebo"
label variable fw113h   "Kissi"
label variable fw113i   "Kpelle"
label variable fw113j   "Krahn"
label variable fw113k   "Kru"
label variable fw113l   "Lorma"
label variable fw113m   "Mandingo"
label variable fw1q3n   "Mano"
label variable fw113o   "Mende"
label variable fw113p   "Sapro"
label variable fw113q   "Vai"
label variable fw113r   "None/only english"
label variable fw113x   "Other"
label variable fw114    "Fieldworker's mother tongue/native language"
label variable fw115a   "Ever worked on a LDHS prior to this survey"
label variable fw115b   "Ever worked on a LMIS prior to this survey"
label variable fw115c   "Ever worked on any other survey prior to this survey"
label variable fw116    "Were you already working for LISGIS or MOH when employed to work on this DHS"
label variable fw117    "Are you a permanent or temporary employee of LISGIS or MOH"

#delimit ;
label define FW102   
     1 "Bomi"
     2 "Bong"
     3 "Gbarpolu"
     4 "Grand bassa"
     5 "Grand cape mount"
     6 "Grand gedeh"
     7 "Grand kru"
     8 "Lofa"
     9 "Margibi"
    10 "Maryland"
    11 "Montserrado"
    12 "Nimba"
    13 "River cess"
    14 "River gee"
    15 "Sinoe"
;
label define FW103   
     1 "City"
     2 "Town"
     3 "Rural"
;
label define FW105   
     1 "Male"
     2 "Female"
;
label define FW106   
     1 "Currently married"
     2 "Living with a man/woman"
     3 "Widowed"
     4 "Divorced"
     5 "Separated"
     6 "Never married or lived with a man/woman"
;
label define FW108   
     1 "Yes"
     2 "No"
;
label define FW109   
     1 "Elementary (1-6)"
     2 "Junior high (7-9)"
     3 "Senior high (10-12)"
     4 "Higher"
;
label define FW110   
     0 "Less than one year at level"
;
label define FW111   
     1 "Christian"
     2 "Muslim"
     3 "Traditional religion"
     4 "No religion"
     6 "Other"
;
label define FW112   
     1 "Bassa"
     2 "Belle"
     3 "Dey"
     4 "Gbandi"
     5 "Gio"
     6 "Gola"
     7 "Grebo"
     8 "Kissi"
     9 "Kpelle"
    10 "Krahn"
    11 "Kru"
    12 "Loma"
    13 "Mandingo"
    14 "Mano"
    15 "Mende"
    16 "Sapro"
    17 "Vai"
;
label define FW114   
     1 "Bassa"
     2 "Gbandi"
     3 "Belle"
     4 "Dey"
     5 "Gio"
     6 "Gola"
     7 "Grebo"
     8 "Kissi"
     9 "Kpelle"
    10 "Krahn"
    11 "Kru"
    12 "Lorma"
    13 "Mandingo"
    14 "Mano"
    15 "Mende"
    16 "Sapro"
    17 "Vai"
    18 "English"
    96 "Other"
;
label define FW115A  
     1 "Yes"
     2 "No"
;
label define FW115B  
     1 "Yes"
     2 "No"
;
label define FW115C  
     1 "Yes"
     2 "No"
;
label define FW116   
     1 "Yes, LISGIS"
     2 "Yees, MOH"
     3 "NO"
;
label define FW117   
     1 "Permanent"
     2 "Temporary"
     3 "Not permanent nor temporary"
;

#delimit cr
label values fw102    FW102   
label values fw103    FW103   
label values fw105    FW105   
label values fw106    FW106   
label values fw108    FW108   
label values fw109    FW109   
label values fw110    FW110   
label values fw111    FW111   
label values fw112    FW112   
label values fw114    FW114   
label values fw115a   FW115A  
label values fw115b   FW115B  
label values fw115c   FW115C  
label values fw116    FW116   
label values fw117    FW117   
