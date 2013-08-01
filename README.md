ESEM Invariance Script Writter
=====================================

Overview
-----------------------------------
The esemInvaTarget function aims to take minimal user input and and return 13 fully functioning MPlus scripts for invariance across multiple groups. i.e., across gender or experimental conditions. Each script will fit one model of Marsh et al. typology of invariance models starting with a free or configural model with model 13 fitting a complete invariance model. From the configural model, subsequent models progressively holding elements of the model invariant between groups. The function should support invariance for two or more groups. The models are as follows:

- M1 – Configural
- M2 – FL
- M3 – FL, Uniq
- M4 – FL, FVCV
- M5 – FL, Inter
- M6 – FL, Uniq, FVCV
- M7 – FL, Uniq, Inter
- M8 – FL, FVCV, Inter
- M9 – FL, Uniq, FVCV, Inter
- M10 – FL, Inter, Latent Means
- M11 – FL, Uniq, Inter, Latent Means
- M12 – FL, FVCV, Inter, Latent Means
- M13 – FL, Uniq, FVCV, Inter, Latent Means

Key:
- FL = Item factor loadings
- Uniq = Item uniquenesses/residuals
- Inter = Item intercepts
- FVCV = Latent variable factor variance covariance matrix

What is NOT supported.
--------------------------
The scripts produced are for crosssectional models only. Longtudinal models would require a consistent naming convention from the user.
My experience is that this is rarely the case for Mplus scripts. 

I have currently only coded the scripts to use ML. I will add a switch for MLR or MLM soon. It is not clear that the 13 models of invariance
works as expected for WLS so this will not be an option.

What it does.
----------------------------
The function first checks input. Mplus has a number of restriction on input files and the extensive checks aim to ensure they are met. The only exception to this is for variable names. Mplus requires variable names to be 8 characters or less but will run input scripts with more characters by truncating the names. As such the input script should run, however, too many variables with over 8 characters may make a script run over line character limits and result in odd output. For this reason it is best to ensure all variables are 8 characters or less. The functions here will return a warning for variables over 8 characters but will not produce an error.
If you are only concerned with invariacne I would recomment using the geomin function as it is easier to code (see example below)
and runs slightly faster. Fit will be the same regardless of rotation statergy.

Example
--------------------------
Using Data from simulated model fond on this site for treatment group with target rotation:
```
esemInvaTarget(2, data, GroupVar = "TbyG", c("treatM", "contM", "treatF", "contF"),
                1:6, FileOut="ESEM", FileIn="ESEM/ESEM.dat", 
                Pattern=list(c(1,6), c(7,12)),LatentNames=c("Latent1", "Latent2"))
```

Using geomin rotation:
```
esemInvaGeomin(2, data, GroupVar = "TbyG", c("treatM", "contM", "treatF", "contF"),
               1:6, FileOut=tempDir, FileIn=MplusData)
```
The entire work flow can be run passively by imputing the following code into R:

```
  trial<-"http://raw.github.com/pdparker/ESEM/master/exampleScript_ESEMS.R"
	source(url(trial))
```
