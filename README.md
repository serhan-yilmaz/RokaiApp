<img src="www/rokai_app_logo.png" width="800">

## Introduction
RoKAI is a computation tool for inferring kinase activities in a robust manner using functional networks. If you are interested in performing RoKAI through a user-friendly online interface, please visit [RoKAI Web Application](https://rokai.io) ([Mirror link](https://syilmaz.shinyapps.io/rokai/)).
## Running Locally
To run RoKAI App locally on your R installation, simply run:
```
library(shiny)
runGitHub("rokaiapp", "serhan-yilmaz")
```
### Running an earlier version of the application
If desired, you can also run an earlier version of the application. For this purpose, specify the ref parameter to be one of the [release versions](https://github.com/serhan-yilmaz/RokaiApp/releases), e.g.:
```
library(shiny)
runGitHub("rokaiapp", "serhan-yilmaz", ref = "v2.1.4")
```
