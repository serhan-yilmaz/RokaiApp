## Changes on Rokai App v2.2.0 (November 2022) - Phosphatases Update

### Phosphatase Activity Inference

### Updated Datasets - Latest Available

### Added Rat Reference Proteome

### Additional

#### Rokai-NetworkData available to download
You can access the code used to preprocess the data from [Rokai-NetworkData github page](https://github.com/serhan-yilmaz/Rokai-NetworkData). The processed NetworkData is available to download in Matlab (``.mat``) and R (``.rds``) file formats. 

#### Interface updates

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
