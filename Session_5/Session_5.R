params <-
list(presentation = TRUE)

#' ---
#' title: Session 5
#' subtitle: How to interpret the latent class
#' date: "2022-06-09"
#' author:
#'   - Matt Denwood
#' theme: metropolis
#' aspectratio: 169
#' colortheme: seahorse
#' header-includes: 
#'   - \input{../rsc/preamble}
#' params:
#'   presentation: TRUE
#' output:
#'   beamer_presentation:
#'       pandoc_args: ["-t", "beamer"]
#'       slide_level: 2
#'   html_document: default
#' ---
#' 
## ----rendering, eval=FALSE, include=FALSE-------------------------------------
## # To render this as PDF (beamer) slides run:
## rmarkdown::render('Session_5.Rmd', 'beamer_presentation', params=list(presentation=TRUE))
## # And for html:
## rmarkdown::render('Session_5.Rmd', 'html_document', params=list(presentation=FALSE))

#' 
## ----setup, include=FALSE-----------------------------------------------------
source("../rsc/setup.R", local = environment())

#' 
#' ## Recap
#' 
#' - Adding more populations and more tests to a Hui-Walter model is technically easy
#'   - Particularly if using template_huiwalter
#'   
#' - Verifying that the assumptions you are making are correct is harder
#'   - The sensitivity and specificity must be consistent
#'   - Pairwise correlation between tests should be accounted for
#' 
#' # How to interpret the latent class
#' 
#' 
#' ## What exactly is our latent class?
#' 
#' Think about what exactly the latent class is in these situations:
#' 
#' 1. An antigen plus antibody test
#' 
#' . . .
#' 
#'   * The latent status is probably close to the true disease status
#'   
#' . . .
#' 
#' 2. Two antibody tests 
#' 
#' . . .
#' 
#'   * The latent status is actually 'producing antibodies'
#'     * And not 'diseased' !!!
#'   
#' . . .
#' 
#' - What do we mean by "conditionally independent"?
#' 
#'   * Independent of each other conditional on the latent state
#'   * But the latent state is NOT always *disease*
#' 
#' 
#' ## A hierarchy of latent states
#' 
#' 
## ----echo=FALSE, fig.height=4-------------------------------------------------
ab_dag <- dagify(
  infected ~ prevalence,
  antibodies ~ infected,
  abtarget ~ antibodies,
  abtarget2 ~ antibodies,
  test1 ~ abtarget,
  test2 ~ abtarget,
  test3 ~ abtarget2,
  latent = c("infected", "antibodies", "abtarget","abtarget2"),
  exposure = "prevalence",
  outcome = c("test1", "test2", "test3"),
  labels = c("infected"="Infected", "prevalence"="Prevalence",
             "antibodies"="Producing Antibodies", "abtarget"="Presence of Target 1","abtarget2"="Presence of Target 2",
             "test1"="ELISA A", "test2"="ELISA B", "test3"="ELISA C")
)
ggdag(ab_dag, text=FALSE, use_labels="label") + theme_dag_blank()

#' 
#' - - -
#' 
#' 
#' 
#' ## Branching of processes leading to test results
#' 
#' - Sometimes we have multiple tests detecting similar things
#'   
#'   - For example:  two antibody tests and one antigen test
#'     - The antibody tests will be correlated
#'   
#' . . .
#' 
#' - Sometimes we have multiple tests on the same site / sample:
#'   
#'   - For example:  two throat swab tests vs a nasal swab test
#'     - The throat swab tests will be correlated
#' 
#' . . .
#' 
#' - Or even three antibody tests where two are primed to detect the same thing, and one has a different target!
#'   
#'   - In this case all three tests are correlated
#'   - But two are more strongly correlated
#' 
#' - - -
#' 
## ----echo=FALSE, fig.height=4-------------------------------------------------
covid_dag <- dagify(
  infected ~ prevalence,
  virus_throat ~ infected,
  virus_nose ~ infected,
  throat_pcr ~ virus_throat,
  throat_antigen ~ virus_throat,
  nose_antigen ~ virus_nose,
#  throat_antigen ~ cross_reaction,
#  nose_antigen ~ cross_reaction,
  latent = c("infected", "virus_throat", "virus_nose"),
  exposure = "prevalence",
  outcome = c("throat_pcr", "throat_antigen", "nose_antigen"),
  labels = c("infected"="infected", "prevalence"="prevalence",
             "virus_throat"="virus_throat", "virus_nose"="virus_nose",
             "throat_pcr"="throat_pcr", "throat_antigen"="throat_antigen",
             "nose_antigen"="nose_antigen", "cross_reaction"="cross_reaction")
)
ggdag(covid_dag, text=FALSE, use_labels="label") + theme_dag_blank()

#' 
#' - - -
#' 
#' Parasites generally have more complex life cycles
#' 
## ----echo=FALSE, out.width="100%", out.height="100%", fig.cap="Life cycle of liver fluke"----
knitr::include_graphics("../rsc/fluke.pdf")

#' 
#' - - -
#' 
#' So diagnostic tests are more difficult to interpret!
#' 
## ----echo=FALSE, out.width="100%", out.height="100%", fig.cap="Diagnostic tests for liver fluke"----
knitr::include_graphics("../rsc/flukediagnostics.pdf")

#' 
#' - - -
#' 
#' What are the tests detecting?
#' 
#'   - Faecal egg counts
#'     * Detect eggs from adult parasites
#'     * These are produced 8-12 weeks after infection
#'     * Eggs may persist in the gall bladder for some weeks after infection has been cleared
#'     
#'   - Antigen ELISA
#'     * Detects presence of maturing/adult parasites in faeces
#'     * This occurs from 5-8 weeks after infection
#'     * Parasites only detectable during active infection
#' 
#'   - Antibody ELISA
#'     * Triggered by migrating juveniles and adults
#'     * Persists (although declining) for several months after infection has been cleared
#' 
#' 
#' ## What is sensitivity and specificity?
#' 
#' - The probability of test status conditional on true disease status?
#' 
#' - The probability of test status conditional on the latent state?
#' 
#' . . .
#' 
#' So is the latent state the same as the true disease state?
#' 
#' . . .
#' 
#' Important quote:
#' 
#' "Latent class models involve pulling **something** out of a hat, and deciding to call it a rabbit"
#' 
#'   - Nils Toft
#' 
#' 
#' ## Model complexity
#' 
#' How many parameters are in my latent class model?
#' 
#' - 2 x k + d x (d - 1) + p
#' - Where k=total tests, d=covarying tests, p=populations
#' 
#' . . .
#' 
#' BUT remember the effect of priors:
#' 
#' - Se ~ dbeta(1, 1) vs. Se ~ dbeta(2, 1) vs. Se ~ dbeta(2978, 234)
#' 
#' . . .
#' 
#' Effective number of parameters:
#' 
#' - pD (and therefore DIC) is ill-defined for LCM
#' - p_waic (and WAIC) is a better bet - will be much easier in JAGS 5.0!
#' 
#' 
#' 
#' # Discussion session 5 - when should we correct for correlation?
#' 
#' Aims:
#' 
#' - To get you to think about when and why we should try to correct for correlation
#' 
#' - To get you to think about what is NOT achieved by including correlation terms
#' 
#' . . .
#' 
#' Main discussion points for each example:
#' 
#' - What is the latent class
#' 
#' - Which correlation terms should we include and why
#' 
#' . . .
#' 
#' Please feel free to hijack the discussion at any point :)
#' 
#' - - -
#' 
## ----echo=FALSE, fig.height=4-------------------------------------------------
eg_dag <- dagify(
  infected ~ prevalence,
#  antibodies ~ infected,
#  abtarget ~ antibodies,
  test1 ~ infected,
  test2 ~ infected,
  test3 ~ infected,
  latent = c("infected"),
  exposure = "prevalence",
  outcome = c("test1", "test2", "test3"),
  labels = c("infected"="Infected", "prevalence"="Prevalence",
             "antigen"="Pathogen Detectable", "antibodies"="Producing Antibodies", "abtarget"="Presence of Target",
             "test1"="Test A", "test2"="Test B", "test3"="Test C")
)
ggdag(eg_dag, text=FALSE, use_labels="label") + theme_dag_blank()

#' 
#' . . .
#' 
#' - No correlation to model!
#' 
#' - - -
#' 
## ----echo=FALSE, fig.height=4-------------------------------------------------
eg_dag <- dagify(
  infected ~ prevalence,
  antibodies ~ infected,
#  abtarget ~ antibodies,
  test1 ~ antibodies,
  test2 ~ antibodies,
  test3 ~ antibodies,
  latent = c("infected","antibodies"),
  exposure = "prevalence",
  outcome = c("test1", "test2", "test3"),
  labels = c("infected"="Infected", "prevalence"="Prevalence",
             "antigen"="Pathogen Detectable", "antibodies"="Producing Antibodies", "abtarget"="Presence of Target",
             "test1"="Test A", "test2"="Test B", "test3"="Test C")
)
ggdag(eg_dag, text=FALSE, use_labels="label") + theme_dag_blank()

#' 
#' . . .
#' 
#' - No correlation to model ... but "infected" is not the latent class
#' 
#' - - -
#' 
## ----echo=FALSE, fig.height=4-------------------------------------------------
eg_dag <- dagify(
  infected ~ prevalence,
  antibodies ~ infected,
  abtarget ~ antibodies,
  test1 ~ abtarget,
  test2 ~ abtarget,
  test3 ~ abtarget,
  latent = c("infected","antibodies","abtarget"),
  exposure = "prevalence",
  outcome = c("test1", "test2", "test3"),
  labels = c("infected"="Infected", "prevalence"="Prevalence",
             "antigen"="Pathogen Detectable", "antibodies"="Producing Antibodies", "abtarget"="Presence of Target",
             "test1"="Test A", "test2"="Test B", "test3"="Test C")
)
ggdag(eg_dag, text=FALSE, use_labels="label") + theme_dag_blank()

#' 
#' . . .
#' 
#' - Same as above!
#' 
#' 
#' - - -
#' 
## ----echo=FALSE, fig.height=4-------------------------------------------------
eg_dag <- dagify(
  infected ~ prevalence,
  antigen ~ infected,
  antibodies ~ infected,
  abtarget ~ antibodies,
  test1 ~ antigen,
  test2 ~ abtarget,
  test3 ~ abtarget,
  latent = c("infected","antibodies","abtarget"),
  exposure = "prevalence",
  outcome = c("test1", "test2", "test3"),
  labels = c("infected"="Infected", "prevalence"="Prevalence",
             "antigen"="Pathogen Detectable", "antibodies"="Producing Antibodies", "abtarget"="Presence of Target",
             "test1"="Test A", "test2"="Test B", "test3"="Test C")
)
ggdag(eg_dag, text=FALSE, use_labels="label") + theme_dag_blank()

#' 
#' . . .
#' 
#' - Tests B and C are correlated
#' 
#' - - -
#' 
## ----echo=FALSE, fig.height=4-------------------------------------------------
eg_dag <- dagify(
  infected ~ prevalence,
  antigen ~ infected,
  antibodies ~ infected,
  abtarget1 ~ antibodies,
  abtarget2 ~ antibodies,
  test1 ~ abtarget1,
  test2 ~ abtarget2,
  test3 ~ abtarget2,
  latent = c("infected","antibodies","abtarget1","abtarget2"),
  exposure = "prevalence",
  outcome = c("test1", "test2", "test3"),
  labels = c("infected"="Infected", "prevalence"="Prevalence",
             "antigen"="Pathogen Detectable", "antibodies"="Producing Antibodies", "abtarget1"="Presence of Target 1", "abtarget2"="Presence of Target 2",
             "test1"="Test A", "test2"="Test B", "test3"="Test C")
)
ggdag(eg_dag, text=FALSE, use_labels="label") + theme_dag_blank()

#' 
#' . . .
#' 
#' - All tests are correlated with respect to infected BUT infected is not the latent class
#' 
#' - Tests B and C are correlated with respect to antibodies - but maybe not substantially?
#' 
#' - - -
#' 
## ----echo=FALSE, fig.height=4-------------------------------------------------
eg_dag <- dagify(
  infected ~ prevalence,
  antigen ~ infected,
  antibodies ~ infected,
  test1 ~ antigen,
  test2 ~ antibodies,
  latent = c("infected","antibodies"),
  exposure = "prevalence",
  outcome = c("test1", "test2"),
  labels = c("infected"="Infected", "prevalence"="Prevalence",
             "antigen"="Pathogen Detectable", "antibodies"="Producing Antibodies", "abtarget"="Presence of Target",
             "test1"="Test A", "test2"="Test B", "test3"="Test C")
)
ggdag(eg_dag, text=FALSE, use_labels="label") + theme_dag_blank()

#' 
#' . . .
#' 
#' - No correlation to model
#' 
#' - - -
#' 
## ----echo=FALSE, fig.height=4-------------------------------------------------
eg_dag <- dagify(
  infected ~ prevalence,
#  antigen ~ infected,
  antibodies ~ infected,
  test1 ~ antibodies,
  test2 ~ antibodies,
  latent = c("infected","antibodies"),
  exposure = "prevalence",
  outcome = c("test1", "test2"),
  labels = c("infected"="Infected", "prevalence"="Prevalence",
             "antigen"="Pathogen Detectable", "antibodies"="Producing Antibodies", "abtarget"="Presence of Target",
             "test1"="Test A", "test2"="Test B", "test3"="Test C")
)
ggdag(eg_dag, text=FALSE, use_labels="label") + theme_dag_blank()

#' 
#' . . .
#' 
#' - No correlation to model - but "infected" is not the latent class
#' 
#' ## Other examples?
#' 
#' [Insert discussion here...]
#' 
#' ## Summary:  between-test correlation structure
#' 
#' My approach:
#' 
#' - If you have <3 tests then forget correlation BUT you must (always) consider what the latent class really means
#' 
#' . . .
#' 
#' - Otherwise, start with the biology.  Which pairwise correlation terms are plausible?
#' 
#' . . .
#' 
#' - If you have a LOT of terms then consider eliminating some based on the posterior being close to zero ... but check that other estimates do not change substantially between "full" and "reduced" models
#' 
#' . . .
#' 
#' - I dislike DIC ... maybe WAIC is better?
#' 
#' 
#' 
#' ## Publication of your results
#' 
#' STARD-BLCM:  A helpful structure to ensure that papers contain all necessary information
#'   
#'   - You should follow this and refer to it in your articles!
#' 
#' . . .
#' 
#' If you use the software, please cite JAGS:
#' 
#'   - Plummer, M. (2003). JAGS : A Program for Analysis of Bayesian Graphical Models Using Gibbs Sampling JAGS : Just Another Gibbs Sampler. Proceedings of the 3rd International Workshop on Distributed Statistical Computing (DSC 2003), March 20–22,Vienna, Austria. ISSN 1609-395X. https://doi.org/10.1.1.13.3406
#' 
#' ---
#' 
#' And R:
#' 
## -----------------------------------------------------------------------------
citation()

#' 
#' ---
#' 
#' And runjags:
#' 
## -----------------------------------------------------------------------------
citation("runjags")

#' 
#' 
## ----include=FALSE------------------------------------------------------------
unlink(cleanup)

