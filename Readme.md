# PROGRAMMING TOOLS IN DATA SCIENCE semester project

Luca Bron, David Germano, Patrik Grandadam, Vincent Lomazzi, Edgar Raisin

## Introduction

This github repository has been created as part of the course *PROGRAMMING TOOLS IN DATA SCIENCE* given by S.Orso and I.Rudnytskyi. More precisely, it consists of our semester project has as main goal to present some tools that we have learned in this class.  
This repository is complementary to the package [*swissimmo*](https://github.com/Upsylon/swissimmo) that we created and that contains several functions.  
Based on the data available on the internet, one of the main goal of our project was to create an interactive map of the Swiss real estate market and to allow users to webscrap updated real estate data on Immoscout24. 

### Video Presentation

To begin with, it is possible to understand how our project works by looking [the following video](https://www.youtube.com/watch?v=hwB-TyIpzo0&t=1s) in which we make a presentation of our project.



## Methodology

Given one or multiple cities of interest, we provide a function that allows the user to **webscrap** all housings currently available on immoscout24.ch. For given cities, on can retrieve the housings available for renting on this website, their characteristics and addresses.  
Based on the prices observed on the market according to the characteristics of the housings, a **Machine Learning prediction model** is built to estimate the market prices of the different housings.  
Finally, one can use an interactive *ShinyApp* to visualize all the housings on a map, filter the data according to some criterion, and see  which housings are over-estimated or under-estimated. 

## Main references

We used a lot of available information coming from the internet and our previous knowledge. However, we can underline some sources that have been particularily usefull when doing this project:  
    - M. Beckman, S. Guerrier, J. Lee, R. Molinari & S. Orso : An Introduction to Statistical Programming Methods with R <https://smac-group.github.io/ds/index.html>  
    - Immoscout24: https://www.immoscout24.ch/en  
    - Wickham, Hadley. 2015. *R Packages*. O’Reilly. <http://r-pkgs.had.co.nz/>.
    
## License

The content of this project itself is licensed under the [Creative Commons Attribution 3.0 Unported license](https://creativecommons.org/licenses/by/3.0/)

