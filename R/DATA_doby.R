
#' @title Near infra red light (NIR) measurements in milk
#' 
#' @description Near infra red light (NIR) measurements are made at
#'     152 wavelengths on 17 milk samples.  While milk runs through a
#'     glass tube, infra red light is sent through the tube and the
#'     amount of light passing though the tube is measured at
#'     different wavelengths.  Each milk sample was additionally
#'     analysed for fat, lactose, protein and dry matter.
#' @concept dataset
#' @name nir_milk
#' @docType data
#' 
#' @format
#' Data comes in two formats:
#' 
#' nir_milk: A list with two components
#' * x Datafrane with infra red light
#'  amount at different wavelengths (column names are the wavelengths;
#'  just remove the leading X).
#' * y Datafrane with response variables
#'  fat, protein, lactose and dm (drymatter)
#'
#' NIRmilk: This data frame contains 17 rows and 158 columns.
#'
#' * The first column is the sample number.
#' * The columns named in the form `Xw` contains
#'     the transmittance (fraction of electromagnetic power)
#'     transmittance through the sample at wavelength `w`.
#' * The response variables are fat, protein, lactose and dm (dry
#'     matter).
#'
#' @keywords datasets
#' @examples
#' 	
#' data(nir_milk)
#' data(NIRmilk)
#' 
"nir_milk"
"NIRmilk"


#' @title Height of math teachers
#' @description Height of a sample of math teachers in Danish high
#'     schools collected at a continued education day at Mariager Fjord Gymnasium in 2019.
#' @concept dataset
#' Format:
#' height: Height in centimeters
#' sex: male or female
#'
#' @examples
#'
#' aggregate(height ~  sex, data=math_teachers, FUN=mean)
#' aggregate(height ~  sex, data=math_teachers, FUN=function(x) {c(mean=mean(x), sd=sd(x))})
#' 
"math_teachers"


##' @title Berkeley Growth Study data
##' @description dataframe with heights of 39 boys and 54 girls from
##'     age 1 to 18 and the ages at which they were collected.
##' @concept dataset
##' Format:
##'
##' gender: Gender of child
##' age: Age at time of data recordning
##' subject: Idenfication for each child
##' height: Height of child
##'
##'
##' @details Notice that the ages are not equally spaced. Data are
##'     taken from the fda package (growth) but put in long format
##'     here.
##'
##' @references
##'     Ramsay, James O., Hooker, Giles, and Graves, Spencer (2009),
##'     _Functional data analysis with R and Matlab_, Springer, New York.
##'
##'     Ramsay, James O., and Silverman, Bernard W. (2005), _Functional
##'     Data Analysis, 2nd ed._, Springer, New York.
##'
##'     Ramsay, James O., and Silverman, Bernard W. (2002), _Applied
##'     Functional Data Analysis_, Springer, New York.
##'
##'     Tuddenham, R. D., and Snyder, M. M. (1954) "Physical growth of
##'     California boys and girls from birth to age 18", _University of
##'     California Publications in Child Development_, 1, 183-364.
##'
##'
"child_growth"     



#' Chemical composition of wine
#' 
#' Using chemical analysis determine the origin of wines
#' 
#' Data comes from the UCI Machine Learning Repository. The grape variety
#' \code{Cult} is the class identifier.  
#' @concept dataset
#' @name data-wine
#' @docType data
#' @format A data frame with 178 observations on the following 14 variables.
#'   \describe{
#'   \item{\code{Cult}}{a factor with levels \code{v1} \code{v2}
#'     \code{v3}: 3 different graph varieties}
#'   \item{\code{Alch}}{Alcohol}
#'   \item{\code{Mlca}}{Malic acid}
#'   \item{\code{Ash}}{Ash}
#'   \item{\code{Aloa}}{Alcalinity of ash}
#'   \item{\code{Mgns}}{Magnesium}
#'   \item{\code{Ttlp}}{Total phenols}
#'   \item{\code{Flvn}}{Flavanoids}
#'   \item{\code{Nnfp}}{Nonflavanoid phenols}
#'   \item{\code{Prnt}}{Proanthocyanins}
#'   \item{\code{Clri}}{Color intensity}
#'   \item{\code{Hue}}{Hue}
#'   \item{\code{Oodw}}{OD280/OD315 of diluted wines}
#'   \item{\code{Prln}}{Proline}
#' }
#' 
#' @references See references at
#'   \url{https://archive.ics.uci.edu/ml/datasets/Wine/}
#'
#' @source Frank, A. & Asuncion, A. (2010). UCI Machine Learning
#'     Repository \url{https://archive.ics.uci.edu/ml/}. Irvine, CA:
#'     University of California, School of Information and Computer
#'     Science.
#'
#' @keywords datasets
#' @usage data(wine)
#' @examples
#' 
#' data(wine)
#' ## maybe str(wine) ; plot(wine) ...
#' 
"wine"


#' @title Income data, years of educations and ethnicity
#' @description Data on income, years of educations and ethnicity for a samle of adult Americans aged over 25. The year of sampling is not avalable in the source.
#' @name income
#' @concept dataset
#' @docType data
#' @format This data frame contains:
#' \describe{
#' \item{inc:}{Income: Yearly income (thousands of dollars).}
#' \item{educ:}{Education: Number of years of education (12=high school graduate, 16=college graduate).}
#' \item{race:}{Racial-Ethnic group: "b" (black), "h" (hispanic) and "w" (white).}
#' }
#' @keywords datasets
#'
#' @details Variable names are as in the reference.
#' 
#' @references Agresti, A. (2024) Statistical Methods for the Social
#'     Sciences, Global Edition (6th edition).  ISBN-13: 9781292449197. Table 13.1
#' 
"income"



#' @title Wear of shoes
#' @description Wear of soles of shoes of materials A and B for one foot each for of ten boys.
#' @concept dataset
#' @details The shoes data are measurements of the amount wear of the
#'     soles of shoes worn by 10 boys. The soles were made to two
#'     different synthetic materials, a standard material A and a
#'     cheaper material B.
#' 
#' @name shoes
#' @docType data
#' @format This data frame contains:
#' \describe{
#' \item{A:}{Wear, material A}
#' \item{B:}{Wear, material B}
#' \item{boy:}{Id of boy}
#' \item{footA:}{The foot with material A}
#' }
#' @keywords datasets
#'
#' @references Box, Hunter, Hunter (2005) Statistics for Experimenters, 2nd edition Wiley, p. 81.
#' 
"shoes"


#' @title crickets data
#' @description Mating songs of male tree crickets.
#' @name crickets
#' @concept dataset
#' @docType data
#' @format This data frame contains:
#' \describe{
#' \item{species:}{Species, (exis, nius), see details}
#' \item{temp:}{temperature}
#' \item{pps:}{pulse per second}
#' }
#' @keywords datasets
#'
#' @details
#'
#'  Walker (1962) studied the mating songs of male tree crickets. Each
#'  wingstroke by a cricket produces a pulse of song, and females may
#'  use the number of pulses per second to identify males of the
#'  correct species. Walker (1962) wanted to know whether the chirps
#'  of the crickets Oecanthus exclamationis (abbreviated exis) and
#'  Oecanthus niveus (abbreviated nius) had different pulse rates. See
#'  the biostathandbook for details. (The
#'  abbreviations are made from the the first two and last two letters
#'  of the species.) Walker measured the pulse rate of the crickets
#'  (variable `pps`) at a variety of temperatures (`temp`):
#'
#' @examples
#' 	
#' data(crickets)
#' coplot(pps ~ temp | species, data=crickets) 
"crickets"


##
## beets
##
#' @title beets data
#'
#' @description Yield and sugar percentage in sugar beets from a split plot
#'     experiment.  Data is obtained from a split plot experiment. There are 3
#'     blocks and in each of these the harvest time defines the "whole plot" and
#'     the sowing time defines the "split plot". Each plot was 25 square meters and
#'     the yield is recorded in kg. See 'details' for the experimental layout.
#'
#' @name beets
#' @concept dataset
#' @docType data
#' @format The format is: chr "beets"
#'
#' @details
#' \preformatted{  
#' Experimental plan
#' Sowing times            1        4. april
#'                         2       12. april
#'                         3       21. april
#'                         4       29. april
#'                         5       18. may
#' Harvest times           1        2. october
#'                         2       21. october
#' Plot allocation:
#'                Block 1     Block 2     Block 3
#'             +-----------|-----------|-----------+
#'       Plot  | 1 1 1 1 1 | 2 2 2 2 2 | 1 1 1 1 1 | Harvest time
#'        1-15 | 3 4 5 2 1 | 3 2 4 5 1 | 5 2 3 4 1 | Sowing time
#'             |-----------|-----------|-----------|
#'       Plot  | 2 2 2 2 2 | 1 1 1 1 1 | 2 2 2 2 2 | Harvest time
#'       16-30 | 2 1 5 4 3 | 4 1 3 2 5 | 1 4 3 2 5 | Sowing time
#'             +-----------|-----------|-----------+  
#' }
#'
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{https://www.jstatsoft.org/v59/i09/}
#'
#' @keywords datasets
#'
#' @examples
#' data(beets)
#' 
#' beets$bh <- with(beets, interaction(block, harvest))
#' summary(aov(yield ~ block + sow + harvest + Error(bh), beets))
#' summary(aov(sugpct ~ block + sow + harvest + Error(bh), beets))
#' 
"beets"


#' Gene expression signatures for p53 mutation status in 250 breast cancer
#' samples
#' 
#' Perturbations of the p53 pathway are associated with more aggressive and
#' therapeutically refractory tumours. We preprocessed the data using Robust
#' Multichip Analysis (RMA). Dataset has been truncated to the 1000 most
#' informative genes (as selected by Wilcoxon test statistics) to simplify
#' computation. The genes have been standardized to have zero mean and unit
#' variance (i.e. z-scored).
#' 
#' The factor \code{code} defines whether there was a mutation in the p53
#' sequence (code=case) or not (code=control).
#' 
#' @name data_breastcancer
#' @concept dataset
#' @docType data
#'
#' @format A data frame with 250 observations on 1001 variables. The
#'     first 1000 columns are numerical variables; the last column
#'     (named \code{code}) is a factor with levels \code{case} and
#'     \code{control}.
#'
#' @references Miller et al (2005, PubMed
#'     ID:16141321)
#'
#' @source Chris Holmes, \email{c.holmes@@stats.ox.ac.uk}
#'
#' @keywords datasets
#'
#' @examples
#' 
#' data(breastcancer)
#' bc <- breastcancer
#' pairs(bc[,1:5], col=bc$code)
#'
#' train <- sample(1:nrow(bc), 50)
#' table(bc$code[train])
#' \dontrun{
#' library(MASS)
#' z <- lda(code ~ ., data=bc, prior = c(1, 1) / 2, subset = train)
#' pc <- predict(z, bc[-train, ])$class
#' pc
#' bc[-train, "code"]
#' table(pc, bc[-train, "code"])
#' }
#' 
"breastcancer"


##
## carcass
##

#' Lean meat contents of 344 pig carcasses
#' 
#' Measurement of lean meat percentage of 344 pig carcasses together with
#' auxiliary information collected at three Danish slaughter houses
#' 
#' @name carcass
#' @concept dataset
#' @aliases carcass carcassall
#' @format carcassall: A data frame with 344 observations on the following 17
#' variables.
#'  \describe{
#'   \item{\code{weight}}{Weight of carcass}
#'   \item{\code{lengthc}}{Length of carcass from back toe to head (when
#'     the carcass hangs in the back legs)}
#'   \item{\code{lengthf}}{Length of carcass from back toe to front leg
#'     (that is, to the shoulder)}
#'   \item{\code{lengthp}}{Length of carcass from back toe to the pelvic bone}
#'   \item{\code{Fat02, Fat03, Fat11, Fat12, Fat13, Fat14, Fat16}}{Thickness of fat
#'     layer at different locations on the back of the carcass (FatXX
#'     refers to thickness at (or rather next to) rib no. XX. Notice that
#'     02 is closest to the head}
#'   \item{\code{Meat11, Meat12, Meat13}}{Thickness of meat layer at different
#'     locations on the back of the carcass, see description above}
#'   \item{\code{LeanMeat}}{Lean meat percentage determined by dissection}
#'   \item{\code{slhouse}}{Slaughter house; a factor with levels \code{slh1} and  \code{slh2}.}
#'   \item{\code{sex}}{Sex of the pig; a factor with levels \code{castrate} and \code{female}.}
#'   \item{\code{size}}{Size of the carcass; a factor with levels \code{normal} and \code{large}.
#'       Here, `normal` refers to carcass weight under 80 kg; `large` refers to carcass weights between 80 and 110 kg.}
#'   
#' }
#'
#' @details: Notice that there were slaughtered large pigs only at one slaughter house.
#' @note carcass: Contains only the variables Fat11, Fat12, Fat13,
#'     Meat11, Meat12, Meat13, LeanMeat
#' @source Busk, H., Olsen, E. V., Brøndum, J. (1999) Determination of
#'     lean meat in pig carcasses with the Autofom classification
#'     system, Meat Science, 52, 307-314
#' @keywords datasets
#' @examples
#' data(carcass)
#' head(carcass)
#' 
"carcass"
"carcassall"


#' Diet of Atlantic cod in the Gulf of St. Lawrence (Canada)
#' 
#' Stomach content data for Atlantic cod (\emph{Gadus morhua}) in the Gulf of
#' St.Lawrence, Eastern Canada.  Note: many prey items were of no interest for
#' this analysis and were regrouped into the "Other" category.
#' 
#' Cod are collected either by contracted commerical fishing vessels
#' (\code{ship.type} 90 or 99) or by research vessels.  Commercial vessels are
#' identified by a unique \code{ship.id}.
#' 
#' Either one research vessel or several commercial vessels conduct a survey
#' (\code{trip}), during which a trawl, gillnets or hooked lines are set
#' several times. Most trips are random stratified surveys (depth-based
#' stratification).
#' 
#' Each trip takes place within one of the \code{region}s.  The \code{trip}
#' label is only guaranteed to be unique within a region and the \code{set}
#' label is only guaranteed to be unique within a \code{trip}.
#' 
#' For each fish caught, the \code{fish.length} is recorded and the fish is
#' allocated a \code{fish.id}, but the \code{fish.id} is only guaranteed to be
#' unique within a \code{set}. A subset of the fish caught are selected for
#' stomach analysis (stratified random selection according to fish length; unit
#' of stratification is the set for research surveys, the combination ship.id
#' and stratum for surveys conducted by commercial vessels, although strata are
#' not shown in codstom).
#' 
#' The basic experimental unit in this data set is a cod stomach (one stomach
#' per fish).  Each stomach is uniquely identified by a combination of
#' \code{region}, \code{ship.type}, \code{ship.id}, \code{trip}, \code{set},
#' and \code{fish.id}.
#' 
#' For each prey item found in a stomach, the species and mass of the prey item
#' are recorded, so there can be multiple observations per stomach.  There may
#' also be several prey items with the same \code{prey.type} in the one stomach
#' (for example many \code{prey.types} have been recoded \code{Other}, which
#' produced many instances of \code{Other} in the same stomach).
#' 
#' If a stomach is empty, a single observation is recorded with
#' \code{prey.type} \code{Empty} and a \code{prey.mass} of zero.
#' 
#' @name codstom
#' @docType data
#'
#' @format A data frame with 10000 observations on the following 10 variables.
#' \describe{
#'   \item{\code{region}}{a factor with levels \code{SGSL} \code{NGSL} 
#'         representing the southern and northern Gulf of St. Lawrence, respectively}
#'   \item{\code{ship.type}}{a factor with levels \code{2} \code{3} \code{31} 
#'   \code{34} \code{90} \code{99}}
#'   \item{\code{ship.id}}{a factor with levels \code{11558} \code{11712}
#'     \code{136148} \code{136885}
#'     \code{136902} \code{137325} \code{151225} \code{151935} \code{99433}}
#'   \item{\code{trip}}{a factor with levels \code{10} \code{11}
#'     \code{12} \code{179} \code{1999}
#'     \code{2} \code{2001} \code{20020808} \code{3} \code{4} \code{5}
#'     \code{6} \code{7} \code{8}
#'     \code{88} \code{9} \code{95}}     
#'   \item{\code{set}}{a numeric vector}
#'   \item{\code{fish.id}}{a numeric vector}
#'   \item{\code{fish.length}}{a numeric vector, length in mm}
#'   \item{\code{prey.mass}}{a numeric vector, mass of item in stomach, in g}
#'   \item{\code{prey.type}}{a factor with levels \code{Ammodytes_sp}
#'     \code{Argis_dent}
#'     \code{Chion_opil} \code{Detritus} \code{Empty} \code{Eualus_fab}
#'     \code{Eualus_mac} \code{Gadus_mor} \code{Hyas_aran}
#'     \code{Hyas_coar}
#'     \code{Lebbeus_gro} \code{Lebbeus_pol} \code{Leptocl_mac}
#'     \code{Mallot_vil}
#'     \code{Megan_norv} \code{Ophiuroidea} \code{Other} \code{Paguridae}
#'     \code{Pandal_bor} \code{Pandal_mon} \code{Pasiph_mult}
#'     \code{Sabin_sept}
#'     \code{Sebastes_sp} \code{Them_abys} \code{Them_comp} \code{Them_lib}}    
#' }
#' @source Small subset from a larger dataset (more stomachs, more variables,
#'     more \code{prey.types}) collected by D. Chabot and M. Hanson, Fisheries &
#'     Oceans Canada \email{chabotd@@dfo-mpo.gc.ca}.
#' @concept dataset
#' @keywords datasets
#' @examples
#' 
#' data(codstom)
#' str(codstom)
#' # removes multiple occurences of same prey.type in stomachs
#' codstom1 <- summaryBy(prey.mass ~ 
#'                       region + ship.type + ship.id + trip + set + fish.id + prey.type,
#'                       data = codstom, 
#'                       FUN = sum) 
#' 
#' # keeps a single line per stomach with the total mass of stomach content
#' codstom2 <- summaryBy(prey.mass ~ region + ship.type + ship.id + trip + set + fish.id,
#'                       data = codstom, 
#'                       FUN = sum) 
#' 
#' # mean prey mass per stomach for each trip
#' codstom3 <- summaryBy(prey.mass.sum ~ region + ship.type + ship.id + trip,
#'                       data = codstom2, FUN = mean) 
#' 
#' \dontrun{          
#' # wide version, one line per stomach, one column per prey type
#' library(reshape)
#' codstom4 <- melt(codstom, id = c(1:7, 9))
#' codstom5 <- cast(codstom4, 
#'                  region + ship.type + ship.id + trip + set + fish.id + fish.length ~ 
#'                  prey.type, sum)
#' k <- length(names(codstom5))
#' prey_col <- 8:k
#' out <- codstom5[,prey_col]
#' out[is.na(out)] <- 0
#' codstom5[,prey_col] <- out
#' codstom5$total.content <- rowSums(codstom5[, prey_col])
#' }
#' 
"codstom"


#' crimeRate
#' 
#' Crime rates per 100,000 inhabitants in states of the USA for different crime
#' types in 1977.
#' 
#' @name crimeRate
#' @concept dataset
#' @docType data
#' @format This data frame contains:
#' \describe{
#' \item{state:}{State of the USA}
#' \item{murder:}{crime of murder}
#' \item{rape:}{}
#' \item{robbery:}{}
#' \item{assault:}{}
#' \item{burglary:}{residential theft}
#' \item{larceny:}{unlawful taking of personal property (pocket picking)}
#' \item{autotheft:}{}
#' }
#' @keywords datasets
#' @examples
#' 	
#' data(crimeRate)
#' 
"crimeRate"

#' crimeRate
#' 
#' Crime rates per 100,000 inhabitants in states of the USA for different crime
#' types in 1977.
#' 
#' @name crime_rate
#' @concept dataset
#' @docType data
#' @format This data frame contains:
#' \describe{
## #' \item{State:}{State of the USA}
#' \item{murder:}{crime of murder}
#' \item{rape:}{}
#' \item{robbery:}{}
#' \item{assault:}{}
#' \item{burglary:}{residential theft}
#' \item{larceny:}{unlawful taking of personal property (pocket picking)}
#' \item{autotheft:}{}
#' }
#' @keywords datasets
#' @examples
#' 	
#' data(crime_rate)
#' 
"crime_rate"






##
## dietox
##


#' Growth curves of pigs in a 3x3 factorial experiment
#' 
#' The \code{dietox} data frame has 861 rows and 7 columns.
#'
#' @details Data contains weight of slaughter pigs measured weekly for 12
#'     weeks. Data also contains the start weight (i.e. the weight at week
#'     1). The treatments are 3 different levels of Evit = vitamin E (dose: 0,
#'     100, 200 mg dl-alpha-tocopheryl acetat /kg feed) in combination with 3
#'     different levels of Cu=copper (dose: 0, 35, 175 mg/kg feed) in the feed.
#'     The cumulated feed intake is also recorded. The pigs are litter mates.
#' 
#' @concept dataset
#' @format This data frame contains the following columns:
#' 
#' \describe{
#' \item{Weight}{Weight in Kg}
#' \item{Feed}{Cumulated feed intake in Kg}
#' \item{Time}{Time (in weeks) in the experiment}
#' \item{Pig}{Factor; id of each pig}
#' \item{Evit}{Factor; vitamin E dose; see 'details'.}
#' \item{Cu}{Factor, copper dose; see 'details'}
#' \item{Start}{Start weight in experiment, i.e. weight at week 1.}
#' \item{Litter}{Factor, id of litter of each pig}
#' }
#' 
#' @source Lauridsen, C., Højsgaard, S.,Sørensen, M.T. C. (1999) Influence of
#'     Dietary Rapeseed Oli, Vitamin E, and Copper on Performance and
#'     Antioxidant and Oxidative Status of Pigs. J. Anim. Sci.77:906-916
#' @keywords datasets
#' @examples
#' 
#' data(dietox)
#' head(dietox)
#' coplot(Weight ~ Time | Evit * Cu, data=dietox)
#' 
"dietox"






##
## fatacid
##

#' @title Fish oil in pig food
#'
#' @description Fish oil in pig food
#'
#' @name fatacid
#' @docType data
#' @concept dataset
#' @format A dataframe.
#'
#' @details A fish oil fatty acid \code{X14} has been added in
#'     different concentrations to the food for pigs in a
#'     study. Interest is in studying how much of the fatty acid can
#'     be found in the tissue. The concentrations of \code{x14} in the
#'     food are \code{verb+dose+=\{0.0, 4.4, 6.2, 9.3\}}.
#'
#' The pigs are fed with this food until their weight is 60 kg.  From
#' thereof and until they are slaughtered at 100kg, their food does
#' not contain the fish oil.
#' At 60kg (sample=1) and 100kg (sample=2) muscle
#' biopsies are made and the concentration of x14 is
#' determined.
#' Measurements on the same pig are correlated, and pigs are additionally
#' related through litters.
#'
#' @references Data courtesy of Charlotte Lauridsen, Department of
#'     Animal Science, Aarhus University, Denmark.
"fatacid"


#' @title Forced expiratory volume in children
#'
#' @description Dataset to examine if respiratory function in children
#'   was influenced by smoking.
#'
#' @name fev
#' @docType data
#'
#' @format A data frame with 654 observations on the following 5 variables.
#' \describe{
#'   \item{\code{Age}}{Age in years.}
#'   \item{\code{FEV}}{Forced expiratory volume in liters per second.}
#'   \item{\code{Ht}}{Height in centimeters.}
#'   \item{\code{Gender}}{Gender.}
#'   \item{\code{Smoke}}{Smoking status.}
#' }
#'
#'
#' @references I. Tager and S. Weiss and B. Rosner and F. Speizer (1979). Effect
#' of Parental Cigarette Smoking on the Pulmonary Function of
#' Children.  American Journal of Epidemiology. 110:15-26
#'
#' @examples
#'
#' data(fev)
#' summary(fev)
#' 
"fev"


#' Heat development in cement under hardening.
#' 
#' Heat development in cement under hardening related to the chemical
#' composition.
#' 
#' @name haldCement
#' @docType data
#' @concept dataset
#' @format A data frame with 13 observations on the following 5 variables.
#'   \describe{
#'     \item{\code{x1}}{Percentage (weight) of `[3Ca0][Al2O3]`}
#'     \item{\code{x2}}{Percentage (weight) of `[3Cao][SiO2]`}
#'     \item{\code{x3}}{Percentage (weight) of `[4Ca0][Al2O3][Fe03]`}
#'     \item{\code{x4}}{Percentage (weight) of `[2Cao][SiO2]`}
#'     \item{\code{y}}{Heat development measured in calories per
#'       gram cement after 180 days}
#'   }
#'
#' @references Anders Hald (1949); Statistiske Metoder; Akademisk Forlag (in
#' Danish), page 509.
#' @keywords datasets
#' @examples
#' 
#' data(haldCement)
#' 
#' if( interactive() ){
#' pairs( haldCement )
#' }
#' m <- lm(y ~ x1 + x2 + x3 + x4, data=haldCement)
#' summary(m)
#' 
#' # Notice: The model explains practically all variation in data;
#' # yet none of the explanatory variables appear to be statistically
#' # significant.
#'
"haldCement"


#' Yield from Danish agricultural production of grain and root crop. 
#'
#' @name cropyield
#' @docType data
#' @format A dataframe with 97 rows and 7 columns. 
#'   \describe{
#'     \item{\code{year}}{From 1901 to 1997.}
#'     \item{\code{precip}}{Milimeter precipitation.}
#'     \item{\code{yield}}{Million feed units (see details).}
#'     \item{\code{area}}{Area in 1000 ha for grains and root crop.}
#'     \item{\code{fertil}}{1000 tons fertilizer.}
#'     \item{\code{avgtmp1}}{Average temperature April-June (3 months).}
#'     \item{\code{avgtmp2}}{Average temperature July-Octobre (4 months). }
#'   }
#'
#' @details A feed unit is the amount of energy in a kg of barley.
#' @references Danmarks statistik (Statistics Denmark).
#' @keywords datasets
"cropyield"



#' Milk yield data for manually milked cows.
#' 
#' Milk yield data for cows milked manually twice a day (morning and evening).
#' 
#' There are data for 222 cows. Some cows appear more than once in the dataset
#' (in different lactations) and there are 288 different lactations.
#'
#' @aliases milkman_rdm1
#' @concept dataset
#' @name milkman
#' @docType data
#'
#' @format
#' A data frame with 161836 observations on the following 12 variables.
#' \describe{
#'   \item{\code{cowno}}{a numeric vector; cow identification}
#'   \item{\code{lactno}}{a numeric vector; lactation number}
#'   \item{\code{ampm}}{a numeric vector; milking time: 1: morning; 2: evening}
#'   \item{\code{dfc}}{a numeric vector; days from calving}
#'   \item{\code{my}}{a numeric vector; milk yield (kg)}
#'   \item{\code{fatpct}}{a numeric vector; fat percentage}
#'   \item{\code{protpct}}{a numeric vector; protein percentage}
#'   \item{\code{lactpct}}{a numeric vector; lactose percentage}
#'   \item{\code{scc}}{a numeric vector; somatic cell counts}
#'   \item{\code{race}}{a factor with levels \code{RDM} \code{Holstein} \code{Jersey}}
#'   \item{\code{ecmy}}{a numeric vector; energy corrected milk}
#'   \item{\code{cowlact}}{Combination of cowno and lactno; necessary
#'     because the same cow may appear more than once in the dataset (in
#'     different lactations)}
#' }
#'
#' @keywords datasets
#'
#' @references Friggens, N. C.; Ridder, C. and Løvendahl, P. (2007). 
#' On the Use of Milk Composition Measures to Predict the Energy Balance of Dairy Cows.
#' J. Dairy Sci. 90:5453–5467 doi:10.3168/jds.2006-821.
#'
#' This study was part of the Biosens project used data from the
#' “Malkekoens energibalance og mobilisering” project; both were
#' funded by the Danish Ministry of Food, Agriculture and Fisheries
#' and the Danish Cattle Association.
#' 
#' @examples
#' 
#' data(milkman)
#' 
"milkman"
"milkman_rdm1"







#' Weight and size of 20 potatoes
#' 
#' Weight and size of 20 potatoes. Weight in grams; size in millimeter. There
#' are two sizes: \code{length} is the longest length and \code{width} is the
#' shortest length across a potato.  
#' 
#' @name potatoes
#' @docType data
#' @concept dataset
#' @format A data frame with 20 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{weight}}{a numeric vector}
#'     \item{\code{length}}{a numeric vector}
#'     \item{\code{width}}{a numeric vector}
#'   }
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @source My own garden; autumn 2015.
#' @keywords datasets
#' @examples
#' 
#' data(potatoes)
#' plot(potatoes) 
#' 
"potatoes"


#' Mathematics marks for students
#' 
#' The \code{mathmark} data frame has 88 rows and 5 columns.
#'
#' @name data_mathmark
#' @concept dataset
#' @aliases mathmark math
#' @format This data frame contains the following columns: mechanics, vectors,
#' algebra, analysis, statistics.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @references David Edwards, An Introduction to Graphical Modelling, Second
#' Edition, Springer Verlag, 2000
#' @keywords datasets
#' @usage data(mathmark)
#' 
#' @examples
#' 
#' data(mathmark)
#' 
"mathmark"
"math"


#' @title Budworm data
#' 
#' @description Experiment on the toxicity to the tobacco budworm
#'     Heliothis virescens of doses of the pyrethroid
#'     trans-cypermethrin to which the moths were beginning to show
#'     resistance. Batches of 20 moths of each sex were exposed for
#'     three days to the pyrethroid and the number in each batch that
#'     were dead or knocked down was recorded. Data is reported in
#'     Collett (1991, p. 75).
#'
#' @concept data
#' @name data_budworm
#' @docType data
#' @concept dataset
#' @format This data frame contains 12 rows and 4 columns:
#'
#' \describe{
#' \item{sex:}{sex of the budworm.}
#' \item{dose:}{dose of the insecticide trans-cypermethrin (in micro grams)}.
#' \item{ndead:}{budworms killed in a trial.}
#' \item{ntotal:}{total number of budworms exposed per trial.}
#' }
#'
#' @references Venables, W.N; Ripley, B.D.(1999) Modern Applied Statistics with
#' S-Plus, Heidelberg, Springer, 3rd edition, chapter 7.2
#'
#' @source Collett, D. (1991) Modelling Binary Data, Chapman & Hall, London,
#' Example 3.7
#'
#' 
#' @keywords datasets
#' @examples
#' 
#' data(budworm)
#' 
#' ## function to caclulate the empirical logits
#' empirical.logit<- function(nevent,ntotal) {
#'    y <- log((nevent + 0.5) / (ntotal - nevent + 0.5))
#'    y
#' }
#' 
#' 
#' # plot the empirical logits against log-dose
#' 
#' log.dose <- log(budworm$dose)
#' emp.logit <- empirical.logit(budworm$ndead, budworm$ntotal)
#' plot(log.dose, emp.logit, type='n', xlab='log-dose',ylab='emprirical logit')
#' title('budworm: emprirical logits of probability to die ')
#' male <- budworm$sex=='male'
#' female <- budworm$sex=='female'
#' lines(log.dose[male], emp.logit[male], type='b', lty=1, col=1)
#' lines(log.dose[female], emp.logit[female], type='b', lty=2, col=2)
#' legend(0.5, 2, legend=c('male', 'female'), lty=c(1,2), col=c(1,2))
#' 
#' \dontrun{
#' * SAS example;
#' data budworm;
#' infile 'budworm.txt' firstobs=2;
#' input sex dose ndead ntotal;
#' run;
#' }
#' 
#' 
"budworm"


#' Coronary artery disease data
#' 
#' A cross classified table with observational data from a Danish
#' heart clinic.  The response variable is CAD (coronary artery
#' disease, some times called heart attack).
#'
#' @details
#'
#' Notice that data are collected at a heart clinic, so data do not
#' represent the population, but are conditional on patients having
#' ended up at the clinic.
#' 
#' * cad1: Complete dataset, 236 cases.
#'
#' * cad2: Incomplete dataset, 67 cases. Information on (some of) the
#'     variables 'Hyperchol', 'Smoker' and 'Inherit' is missing.
#'
#' @concept dataset
#' @name data_cad
#' @aliases cad1 cad2
#' @docType data
#' @format A data frame with 236 observations on the following 14 variables.
#'
#' \describe{
#'   \item{\code{Sex}}{Sex; a factor with levels \code{Female} \code{Male}}
#'
#'   \item{\code{AngPec}}{Angina pectoris (chest pain attacks); a
#'   factor with levels \code{Atypical} \code{None} \code{Typical}}
#'
#'   \item{\code{AMI}}{Acute myocardic infarct; a factor with
#'   levels \code{Definite} \code{NotCertain}}
#'
#'   \item{\code{QWave}}{A reading from an electrocardiogram; a
#'   factor with levels \code{No} \code{Yes}; Yes means pathological and is a sign of previous myocardial infarction. }
#'
#'   \item{\code{QWavecode}}{a factor with levels \code{Nonusable}
#'   \code{Usable}. An assesment of whether QWave is reliable.}
#'
#'   \item{\code{STcode}}{a factor with levels
#'   \code{Nonusable} \code{Usable}. An assesment of whether STchange is reliable.}
#'
#'   \item{\code{STchange}}{A reading from an electrocardiogram; a factor
#'   with levels \code{No} \code{Yes}. An STchange indicates a blockage of the coronary artery.}
#'
#'   \item{\code{SuffHeartF}}{Sufficient heart frequency; a factor with levels \code{No}, \code{Yes}}
#' 
#'   \item{\code{Hypertrophi}}{a factor with levels \code{No}, \code{Yes}. Hypertrophy refers to an
#'   increased size of the heart muscle due to exercise. }
#'
#'   \item{\code{Hyperchol}}{a factor with levels \code{No} \code{Yes}. Hypercholesterolemia, also called high cholesterol,
#'    is the presence of high levels of cholesterol in the blood.}
#'
#'   \item{\code{Smoker}}{Is the patient a smoker; a factor with levels \code{No}, \code{Yes}.}
#'
#'   \item{\code{Inherit}}{Hereditary predispositions for CAD; a factor with levels  \code{No}, \code{Yes}.}
#'
#'   \item{\code{Heartfail}}{Previous heart failures; a factor with  levels \code{No} \code{Yes}}
#'
#'   \item{\code{CAD}}{Coronary Artery Disease; a factor with levels
#'    \code{No} \code{Yes}}.  CAD refers to a reduction of blood flow
#'    to the heart muscle (commonly known as a heart attack). The
#'    diagnosis made from biopsies.
#'
#' }
#'
#' 
#' @references Hansen, J. F. (1980). The clinical diagnosis of
#'     ischaemic heart disease due to coronary artery disease. Danish
#'     Medical Bulletin
#'
#' Højsgaard, Søren and Thiesson, Bo (1995). BIFROST - Block recursive
#' models Induced From Relevant knowledge, Observations and
#' Statistical Techniques. Computational Statistics and Data Analysis,
#' vol. 19, p. 155-175
#' #'
#' @keywords datasets
#' @usage data(cad1)
#' 
#' @examples
#' 
#' data(cad1)
#' ## maybe str(cad1) ; plot(cad1) ...
#' 
"cad1"
"cad2"


#' Personality traits
#'
#' The `peronality` dataframe has 240 rows and 32 columns
#'
#' @name data_personality
#' @concept dataset
#' @format This dataframe has recordings on the following 32
#'     variables: distant, talkatv, carelss, hardwrk, anxious,
#'     agreebl, tense, kind, opposng, relaxed, disorgn, outgoin,
#'     approvn, shy, discipl, harsh, persevr, friendl, worryin,
#'     respnsi, contrar, sociabl, lazy, coopera, quiet, organiz,
#'     criticl, lax, laidbck, withdrw, givinup, easygon
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @references Origin unclear
#' @keywords datasets
#' @usage data(personality)
#' @examples
#' data(personality)
#' str(personality)
"personality"
