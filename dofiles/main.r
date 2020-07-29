## Description: 
## Project: CoVideo
## Author: AV / Created: 25May2020 

######################################################################
######################### Names of behav statements ##################
######################################################################
bstate <- list(
  SocialDist = "go out with my friends",
  Wash = "wash my hands frequently",
  StockPile = "stock up on household supplies for a month",
  CleanDishes = "clean my dishes after use",
  CleanSurfaces = "clean kitchen counters after use",
  UseMedia = "seek online, animated, expert, health videos")


kstate <- list(
  SpreadHealthy = c("The current coronavirus can be spread by an infected person even if they look healthy", "True"),
  NoPersonSpread = c("The current coronavirus  cannot be spread from person to person ", "False"),
  SurfaceSurvive = c("The current coronavirus cannot survive on surfaces for more than a few minutes ", "False"),
  Cough = c("Some people with COVID-19 infection may experience a cough ", "True"),
  Fever = c("Some people with COVID-19 infection do not experience a fever ", "True"),
  DropletMouth = c("The current coronavirus spreads from person to person through small droplets from the mouth ", "True"),
  DropletNose = c("The current coronavirus spreads from person to person through small droplets from the nose", "True"),
  TouchFace = c("You can catch COVID-19 by touching a contaminated surface and then touching your face", "True"),
  Antibiotic = c("Antibiotics can be used to treat COVID-19 infection", "False"),
  CleanSoap = c("Cleaning surfaces with soap and water is an effective way to kill the current coronavirus  ", "True"),
  WashHandSoap = c("Wash your hands frequently with soap and water ", "True"),
  RinseSalt = c("Regularly rinse your nose with salt water", "False"),
  AvoidFaceTouch = c("Avoid touching your face ", "True"),
  NoShakeHands = c("Avoid shaking hands with other people", "True"),
  AvoidPlace = c("Avoid places that are crowded with people (like bars, restaurants or performances)", "True"),
  Garlic = c("Eat garlic with each meal ", "False"),
  ShareUtensil = c("Avoid sharing eating utensils with others", "True"),
  GoodStockpile = c("It is a good idea to buy a large supply of essential goods such as toilet paper to keep in your home", "False"),
  FaceMask = c("Wear a face mask even if you don't have COVID-19 symptoms", "True"))

