# Date: December 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: Visual representation of DE-9IM
# Description du problème:
# 
#
# Libraries utilisées: sf, tmap

library(sf)
#library(tmap)

# col_A_full = "#30360f"
# col_A_border = "#99d8c9"
# col_B_full =  "#cb401b"
# col_A_border = "#fdbb84"
# col_R_full =  "#e7ddd2"
# col_R_border = "#cc9764"

# rotation function from geocompr chapter5 ed1 and "sf3" vignettes
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

square1 = sf::st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))

diamond = (square1 - sf::st_centroid(square1)) * rot(pi/4) + sf::st_centroid(square1)
diamond = diamond  + c(0.75, 0)

pol_bbox = sf::st_as_sfc(sf::st_bbox(sf::st_sfc(square1, diamond)))

plot(sf::st_sfc(square1, diamond))

# Int_Int 
Int_Int = sf::st_intersection(square1, diamond)
plot(Int_Int)

# Int_Bound
Int_Bound = sf::st_intersection(square1,
                                sf::st_boundary(diamond))
plot(Int_Bound)

# Bound_Bound
Bound_Bound = sf::st_intersection(sf::st_boundary(square1),
                                  sf::st_boundary(diamond))
plot(Bound_Bound)

# Int_Ext
Int_Ext = sf::st_difference(square1, diamond)

plot(Int_Ext)
# I guess I need to move my diamond a bit !

# Bound_Ext
# lot of option to do this one 
Bound_Ext = sf::st_difference(sf::st_boundary(square1),
                              diamond)
plot(Bound_Ext)

# Ext_Ext
Ext_Ext = sf::st_difference(pol_bbox,
                            sf::st_union(sf::st_sfc(square1, diamond)))
plot(Ext_Ext)