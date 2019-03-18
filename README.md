# Quantifying urban population density and scaling beyond two-dimensional space

Lei Dong, Zhou Huang, and Yu Liu

## Abstract

Population density is one of the most important indicators for measuring urban properties and is widely used in quantitative analysis in the social and natural sciences. Most previous studies have treated cities as a two-dimensional plane, on the basis of which population density and a series of statistical indicators (e.g., scaling exponents) have been discussed. However, an important distinction between city and rural areas is the presence of tall buildings that expand the city into the third dimension -- the sky and underground. Here, we analyze the spatial organization of cities by including three-dimensional information: the building footprints, number of floors, and granular population distribution data estimated by a mobile phone dataset. We classify the population into daytime and nighttime distributions and construct an `active' population distribution by weighting the distributions together. Our results show that the active population density gradient is almost conserved after taking the total floor area as the denominator. This result reflects the idea that an individual's need for space is stable -- it does not change much with the population concentration. Furthermore, we calculate the within-city scaling exponents among the active population, the building footprint, and the total floor area, and show the possibility of unifying the three scaling exponents -- 2/3, 5/6, and 1 -- between urban area and population under the framework of classifying urban spaces into shareable and nonshareable categories.

### Replicate data and code

- data
    * beijing_building_v1.csv, building dataset
    * beijing_daynight_density.csv, population denisty dataset
    * beijing_popu_building.csv, population and building by grid cell
    
- densityscaling_replicate.R
    * R code for all results presented in the paper
    
    
Contact: arch.dongl@gmail.com
