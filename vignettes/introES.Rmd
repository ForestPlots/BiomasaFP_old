<!--

        %\VignetteEngine{knitr::rmarkdown}
        %\VignetteIndexEntry{Introduccion al paquete BiomasaFP}
        \usepackage[utf8]{inputenc} 
        \usepackage[spanish]{babel}
        
       
-->
        title: "Introduccion al paquete BiomasaFP"
        author: "Gabriela Lopez, Martin Sullivan, Tim Baker"
        date: "Sunday, February 28, 2015"
        output: html_document
---

**Introduccion**

El paquete BiomasaFP R esta disenado para permitir a los usuarios de ForestPlots.net calcular la biomasa aerea (AGB), densidad de tallos, densidad de madera y la frecuencia y el numero de especies de las familia a partir de datos descargados de [Forest Plots database] [http://www.forestplots.net/]. Tambien proporciona funciones para limpiar sus datos y proporcionar resumenes a nivel de parcela.

---

**Entrada de Datos**

Tres archivos, descargados de ForestPlots.net, son necesarios para la entrada de datos:
a) archivo csv con informacion de cada individuo, este archivo se descarga desde la búsqueda avanzada
b) archivo de densidad de la madera descargado desde la biblioteca de consulta
c) archivo de metadatos descargado de la biblioteca de consulta.

Los 3 archivos deben corresponder a la misma parcela vistas .

Estos archivos se leen en R utilizando ` mergefp`, que luego los combina y realiza una limpieza básica:

```
library(BiomasaFP)
# lee, limpia y fusiona el archivo de datos del Censo , el archivo de metadatos y densidad de la madera (WD).
mergedCensus <- mergefp('TestDataset.csv','Metadata10nov.csv','wd10nov.csv' )
```

`mergefp` tambien se puede utilizar para combinar y limpiar estos tres conjuntos de datos cuando ya se han leido en R. ` mergefp` chequea automaticamente si cada argumento que se le pasa es un objecto tipo `data.frame`  por lo que ** no ** necesita especificar si sus argumentos son tramas de datos o las rutas de archivos . El conjunto de datos devuelto por `mergefp` se utiliza como entrada  de datos en muchas de las otras funciones en *BiomasaFP*.


-----

**Salida de Resumen Estadistico **

Una vez que haya obtenido el conjunto de datos resultante de la fusión usando `mergefp`, se puede calcular una serie de resumenes estadisticos a nivel de vista de la parcela (PlotView):
- Coordenadas geograficas , fechas del primer y ultimo censo, y el numero de censos ( `SummaryPlotviews` ) .
- El numero de especies e individuos en cada familia (`SummaryFamilies`).
- Biomasa Total en cada censo (`SummaryAGB`).
- Cambio en AGB entre el primer y el ultimo censo (`AGBch`) .


Estas funciones devuelven tramas de datos indexados por vista de la parcela (y el numero de censo, cuando se calculan los resumenes estadisticos para cada censo).

Las tres funciones que calculan los resumenes estadisticos de biomasa (`SummaryAGB`, `AGBch` y `LastAGB`) requieren  de una ecuacion para estimar el AGB. Siete ecuaciones alometricas se incluyen en *BiomasaFP*; seis ecuaciones de Chave et al. 2005 (`AGBChv05DH`, `AGBChv05M` , `AGBChv05MH`, `AGBChv05W`  `AGBChv05WH`) y  el modelo pantropical reportado en Chave et al. (2014; modelo 4, 'AGBChv14') . Cuando se utiliza la altura del arbol en una ecuacion, esta se estima utilizando los parametros regionales para un modelo de Weibull (Feldpaush et al., 2011).


```
#Estimar AGB total por parcela(vista de la parcela) en cada censo
#Utilizando la ecuacion de Chave 2005 (sin altura) para bosque humedo
AGB.sum<-SummaryAGB(mergedCensus,AGBChv05M)
head(AGB.sum)
#Repetir utilizando la ecuacion de Chave 2005 (con altura) para bosque humedo
AGB.sum.height<-SummaryAGB(mergedCensus,AGBChv05MH)
head(AGB.sum.height)
```

Por defecto, estas tres funciones utilizan DBH4 para el diametro de los arboles, pero esto se puede configurar por el usuario para utilizar cualquiera de los siguientes diametros:  DBH1, DBH2, DBH3 o DBH4.

```
#Por defecto , utiliza DBH4
AGB.Last<-LastAGB(mergedCensus,AGBChv05M)
head(AGB.Last)
#Utilizando DBH3
AGB.Last.dbh3<LastAGB(mergedCensus,AGBChv05M,"DBH3")
head(AGB.Last.dbh3)
```
----
**Estimacion de biomasa a nivel de arbol**

Las ecuaciones alometricas llamados por `SummaryAGB`, `AGBch` y `MeanWtAGB` tambien se pueden acceder de forma individual, y devolver estimaciones de biomasa y (cuando se utiliza la altura en la ecuación) la altura para cada árbol en cada censo. Una vez más, estos utilizan DBH4 por defecto, pero esto se puede cambiar mediante la adicion del usario.

```
#Usando DBH4 (default)
tree.AGB<-AGBChv05M(mergedCensus)
head(tree.AGB)
#Usando  DBH3
tree.AGB.dbh3<-AGBChv05M(mergedCensus,"DBH3")
head(tree.AGB.dbh3)
```

**Citation**

Si utilizas esta paquete, por favor citalo como:

Lopez-Gonzalez G, Sullivan M and Baker TR (2015). BiomasaFP: Estimate biomass for data downloaded from ForestPlots.net. R package version 1.0.

-----
**Referencias**

**Base de Datos**

Lopez-Gonzalez G, Lewis SL, Burkitt M and Phillips OL (2011) ForestPlots.net: a web application and research tool to manage and analyse tropical forest plot data. Journal of Vegetation Science 22: 610-613. doi: 10.1111/j.1654-1103.2011.01312.x

**Biomasa y Altura**

Chave C, Andalo J, Brown S, et al. (2005) Tree allometry and improved estimation of carbon stocks and balance in tropical forests. Oecologia 145 (1):87-99. doi:10.1007/s00442-005-0100-x.

Chave J, Rejou-Mechain M, Burquez A et al. (2014) Improved allometric models to estimate the aboveground biomass of tropical trees. Global Change Biology 20: 3177-3190. doi: 10.1111/gcb.12629

Feldpausch TR, Banin L, Phillips OL, Baker TR, Lewis SL et al. (2011) Height-diameter allometry of tropical forest trees. Biogeosciences 8 (5):1081-1106. doi:10.5194/bg-8-1081-2011.

**Densidad de la Madera**

Chave J, Coomes DA, Jansen S, Lewis SL, Swenson NG, Zanne AE (2009) Towards a worldwide wood economics spectrum. Ecology Letters 12(4): 351-366. http://dx.doi.org/10.1111/j.1461-0248.2009.01285.x

Zanne AE, Lopez-Gonzalez G, Coomes DA, Ilic J, Jansen S, Lewis SL, Miller RB, Swenson NG, Wiemann MC, Chave J. 2009. Data from: Towards a worldwide wood economics spectrum. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.234
