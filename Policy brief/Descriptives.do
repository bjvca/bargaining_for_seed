import delimited "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG\baseline\data\public\baseline.csv", clear

cd "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG\bargaining_for_seed\Policy brief"

// keeping only farmers that only bought seed/participated in the bargaining
keep if paidstart_neg != "n/a" 
drop if farmer_id == ""

//sex of the seller
gen seller = enumerator_gender
tab seller
des seller
replace seller = "1" if seller == "Female"
replace seller = "2" if seller == "Male"
destring seller, replace
la define seller 1 "Female" 2 "Male"
la values seller seller
tab seller

// First offer price
des p1_pric
gen offer_price = p1_pric
destring offer_price, replace ignore ("NA")
ttest offer_price, by (seller)
asdoc ttest offer_price, by(seller) stat(obs mean p dif) label dec(3) tzok title(Descriptives) save(descriptives) replace

// buyer accepted the first initial offer price
tab paidstart_neg
tab paidstart_neg if paidstart_neg != "n/a"
gen accepts_offer = paidstart_neg
des accepts_offer
replace accepts_offer = "1" if accepts_offer == "Yes"
replace accepts_offer = "2" if accepts_offer == "No"
destring accepts_offer, replace
recode accepts_offer (1=100) (2=0)
asdoc ttest accepts_offer, by(seller) stat(obs mean p dif) label dec(3) tzok title(Descriptives) save(descriptives) rowappend

// price bids
gen bid1 = paidp2_pric
gen bid2 = paidp2_pric_2
gen bid3 = paidp2_pric_3
gen bid4 = paidp2_pric_4
gen bid5 = paidp2_pric_5
gen bid6 = paidp2_pric_6
gen bid7 = paidp2_pric_7
gen bid8 = paidp2_pric_8
gen bid9 = paidp2_pric_9
gen bid10 = paidp2_pric_10
gen bid11 = paidp2_pric_11

destring bid1 bid2 bid3 bid4 bid5 bid6 bid7 bid8 bid9 bid10 bid11, replace ignore ("n/a")

// first counter bid price
sum bid1
asdoc ttest bid1, by(seller) stat(obs mean p dif) label dec(3) tzok title(Descriptives) save(descriptives) rowappend

// 1st counter bid price is the lowest acceptable
count if bid1 == 3000
gen bottom_price = bid1
recode bottom_price (3000=100) (nonmissing=0)
asdoc ttest bottom_price, by(seller) stat(obs mean p dif) label dec(3) tzok title(Descriptives) save(descriptives) rowappend

// buyer sticks to the first bid price
gen stick2 = .
replace stick2 = 1 if bid2==bid1
replace stick2 = 0 if bid2!=bid1

gen stick3 = .
replace stick3 = 1 if bid3==bid1
replace stick3 = 0 if bid3!=bid1

gen stick4 = .
replace stick4 = 1 if bid4==bid1
replace stick4 = 0 if bid4!=bid1

gen stick5 = .
replace stick5 = 1 if bid5==bid1
replace stick5 = 0 if bid5!=bid1

gen stick6 = .
replace stick6 = 1 if bid6==bid1
replace stick6 = 0 if bid6!=bid1

gen stick7 = .
replace stick7 = 1 if bid7==bid1
replace stick7 = 0 if bid7!=bid1

gen stick8 = .
replace stick8 = 1 if bid8==bid1
replace stick8 = 0 if bid8!=bid1

gen stick9 = .
replace stick9 = 1 if bid9==bid1
replace stick9 = 0 if bid9!=bid1

gen stick10 = .
replace stick10 = 1 if bid10==bid1
replace stick10 = 0 if bid10!=bid1

gen stick11 = .
replace stick11 = 1 if bid11==bid1
replace stick11 = 0 if bid11!=bid1

egen sticks = rowtotal(stick2 stick3 stick4 stick5 stick6 stick6 stick8 stick9 stick10 stick11)
recode sticks (0=0) (nonmissing = 100)

asdoc ttest sticks, by(seller) stat(obs mean p dif) label dec(3) tzok title(Descriptives) save(descriptives) rowappend

// number of negotiation rounds
gen rounds = .
replace rounds = 1 if bid1 !=.
replace rounds = 2 if bid2 !=.
replace rounds = 3 if bid3 !=.
replace rounds = 4 if bid4 !=.
replace rounds = 5 if bid5 !=.
replace rounds = 6 if bid6 !=.
replace rounds = 7 if bid7 !=.
replace rounds = 8 if bid8 !=.
replace rounds = 9 if bid9 !=.
replace rounds = 10 if bid10 !=.
replace rounds = 11 if bid11 !=.
sum rounds

asdoc ttest rounds, by(seller) stat(obs mean p dif) label dec(3) tzok title(Descriptives) save(descriptives) rowappend

// final transaction price
gen price = .
replace price = offer_price if bid1 == .
replace  price = bid1 if bid2 == . & price == .
replace  price = bid2 if bid3 == . & price == .
replace  price = bid3 if bid4 == . & price == .
replace  price = bid4 if bid5 == . & price == .
replace  price = bid5 if bid6 == . & price == .
replace  price = bid6 if bid7 == . & price == .
replace  price = bid7 if bid8 == . & price == .
replace  price = bid8 if bid9 == . & price == .
replace  price = bid9 if bid10 == . & price == .
replace  price = bid10 if bid11 == . & price == .
tab price

asdoc ttest price, by(seller) stat(obs mean p dif) label dec(3) tzok title(Descriptives) save(descriptives) rowappend
