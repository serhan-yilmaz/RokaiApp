## Changes on Rokai App v2.2.0 (November 2022) - Phosphatases Update

### Phosphatase Activity Inference

Starting on Rokai App v2.2.0, there is now an option to analyze the phosphatases and infer their activities in addition to the kinases! The analysis is done the same way it is done for kinases, only the direction is inverted. That is, a phosphatase is inferred to be active if its known substrates (or other phosphosites in its functional neighborhood) are phosphorylated less than expected (i.e., having a negative log-fold change). 

For this analysis, the phosphatase-substrate interactions in the [DEPOD database](http://depod.bioss.uni-freiburg.de/) are used. Overall, for ``human`` (``mouse``) species, ``1531`` (``740``) new interactions are added to the Rokai network data, which are between ``77``(``75``) phosphatases and ``478`` (``283``) phosphosites.

### Updated Datasets - Latest Available
All datasets from [PhosphoSitePlus](https://www.phosphosite.org/), [Signor](https://signor.uniroma2.it/), [STRING](https://string-db.org/) are updated with their latest release versions. For ``human`` network, the changes are as follows:

```
Kinase-Substrate Interactions (PSP + Signor): 
7749 -> 7780 between 222 -> 234 Kinases and 106136 -> 106350
```

### Added Rat Reference Proteome

### Additional

#### Rokai-NetworkData available to download
You can access the code used to preprocess the data from [Rokai-NetworkData github page](https://github.com/serhan-yilmaz/Rokai-NetworkData). The processed ``NetworkData`` is available to download in Matlab (``.mat``) and R (``.rds``) file formats for ``human``, ``mouse`` and ``rat`` species. 

#### Interface updates
There are a number of minor interface updates to make the results compatible with the addition of the phosphatase analysis. For example, in the plots, y-axis renamed as `Activity` instead of `Kinase Activity` and phosphatases are shown alongside the kinases, marked with ``Phospha-`` tag for easy visibility. In the tables, the ``Type`` column indicates whether a row corresponds to a kinase or a phosphatase. The columns in the table are also renamed to reflect that there are not only kinases in the results (e.g., ``KinID`` renamed as ``UniprotID``). The default and maximum values for ``Min. absolute z-score`` slider (for the filtering in the plots) is also changed to 1.5 and 3 (from 1.25 and 2 respectively). 

### Backward Compatibility

#### Disabling phosphatase analysis
Since the phosphatases are also included in the functional networks with this option, there can be slight changes in the kinase results as well. For backwards compatibility, you can turn this option off to remove phosphatases from the analysis by deselecting the ``Include_phosphatases_in_the_analysis`` tickbox.

#### Selecting NetworkData version
For compability with results of earlier versions, you can now select which NetworkData version to use in the analysis. For this purpose, visit the ``About -> Versions`` tab within the app and set the desired NetworkData version from the provided list. 

#### Running a local image of an older version
Alternatively, if you need compatibility with the output formats of the previous releases (e.g., in the Kinases table), you can run an earlier version of the application locally, by specifying the ref parameter of ``runGithub`` command to be one of the [release versions of RokaiApp](https://github.com/serhan-yilmaz/RokaiApp/releases), e.g.:
```
library(shiny)
runGitHub("rokaiapp", "serhan-yilmaz", ref = "v2.1.4")
```

### Upcoming

#### Performing Bulk Computations

#### Any Other Feedback is Welcome!
