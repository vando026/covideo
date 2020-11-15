#' @title bstate
#' 
#' @description  behavior intent statements
#' 
#' @param NULL
#' 
#' @return 
#'
#' @export 

bstate <- function() {
  return(list(
    SocialDist = "go out with my friends",
    Wash = "wash my hands frequently",
    StockPile = "stock up on household supplies for a month",
    CleanDishes = "clean my dishes after use",
    CleanSurfaces = "clean kitchen counters after use",
    UseMedia = "seek health information from animated videos made by health experts"))
}


#' @title cstate
#' 
#' @description  clinical knowledge statements
#' 
#' @param NULL
#' 
#' @return 
#'
#' @export 

cstate <- function() {
  return(list(
  SpreadHealthy = c("The current coronavirus can be spread by an infected person even if they look healthy", "True"),
  NoPersonSpread = c("The current coronavirus  cannot be spread from person to person ", "False"),
  SurfaceSurvive = c("The current coronavirus cannot survive on surfaces for more than a few minutes ", "False"),
  Cough = c("Some people with COVID-19 infection may experience a cough ", "True"),
  Fever = c("Some people with COVID-19 infection do not experience a fever ", "True"),
  DropletMouth = c("The current coronavirus spreads from person to person through small droplets from the mouth ", "True"),
  DropletNose = c("The current coronavirus spreads from person to person through small droplets from the nose", "True"),
  TouchFace = c("You can catch COVID-19 by touching a contaminated surface and then touching your face", "True"),
  Antibiotic = c("Antibiotics can be used to treat COVID-19 infection", "False"),
  CleanSoap = c("Cleaning surfaces with soap and water is an effective way to kill the current coronavirus  ", "True")))
}

#' @title state
#' 
#' @description  knowledge spread questions
#' 
#' @param NULL
#' 
#' @return 
#'
#' @export 


sstate <- function() {
  return(list(
  WashHandSoap = c("An effective way to prevent COVID-19 spread: wash your hands frequently with soap and water ", "True"),
  RinseSalt = c("An effective way to prevent COVID-19 spread: regularly rinse your nose with salt water", "False"),
  AvoidFaceTouch = c("An effective way to prevent COVID-19 spread: avoid touching your face ", "True"),
  NoShakeHands = c("An effective way to prevent COVID-19 spread: avoid shaking hands with other people", "True"),
  AvoidPlace = c("An effective way to prevent COVID-19 spread: avoid places that are crowded with people (like bars, restaurants or performances)", "True"),
  Garlic = c("An effective way to prevent COVID-19 spread: eat garlic with each meal ", "False"),
  ShareUtensil = c("An effective way to prevent COVID-19 spread: avoid sharing eating utensils with others", "True"),
  # GoodStockpile = c("It is a good idea to buy a large supply of essential goods such as toilet paper to keep in your home", "False"),
  FaceMask = c("An effective way to prevent COVID-19 spread: wear a face mask even if you don't have COVID-19 symptoms", "True")))
}



