params <-
list(presentation = TRUE)

#' ---
#' title: Session 8
#' subtitle: Practical session
#' date: "2022-06-10"
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
## rmarkdown::render('Session_8.Rmd', 'beamer_presentation', params=list(presentation=TRUE))
## # And for html:
## rmarkdown::render('Session_8.Rmd', 'html_document', params=list(presentation=FALSE))

#' 
## ----setup, include=FALSE-----------------------------------------------------
source("../rsc/setup.R", local = environment())

#' 
#' ## Practical session
#' 
#' NOTE: THIS MATERIAL IS NOT YET FINALISED, PLEASE CHECK BACK SOON!
#' 
#' This is the final session of the course!
#' 
#' We have three options to choose from:
#' 
#' 1. Finish up working on practical exercises from previous sessions
#' 
#' 2. Work on your own data and ask us for help/advice
#' 
#' 3. Look at how to implement custom distributions/functions via a JAGS module
#' 
#' 
#' ## JAGS modules
#' 
#' TODO
#' 
#' 
#' ## Adding a function
#' 
#' TODO
#' 
#' ## Adding a distribution
#' 
#' TODO
#' 
#' ## Advanced usage
#' 
#' TODO
#' 
#' ## Before you go...
#' 
#' - Feedback on the course would be extremely welcome!
#'   - Please fill in the stop/start/continue feedback forms provided
#' 
#' . . .
#' 
#' - Remember to keep an eye on the COST action website:
#'   - http://harmony-net.eu
#'   - Physical training schools are being run in September and accepting sign-ups now!
#' 
#' 
## ----include=FALSE------------------------------------------------------------
unlink(cleanup)

