### How are phosphatases analyzed?
***
<p style='text-align: justify; margin:0px; margin-bottom:6px;'> Starting on v2.2.0 (November 2022), there is now an option to analyze phosphatases and infer their activities in addition to the kinases. The analysis is done the same way it is done for kinases, only the direction is inverted. That is, a phosphatase is inferred to be active if its known substrates (or other phosphosites in its functional neighborhood) are phosphorylated less than expected. </p>

<p style='text-align: justify; margin:0px; margin-bottom:6px;'> Phosphatases are shown alongside the kinases and are marked with <em>"Phospha-"</em> tag in the plots for easy visibility. In the tables, the Type column indicates whether a row corresponds to a kinase or a phosphatase. The columns in the table are also renamed to reflect that there are not only kinases in the results (e.g., KinID renamed as UniprotID). </p>

<p style='text-align: justify; margin:0px; margin-bottom:6px;'> Since the phosphatases are also included in the functional networks with this option, there can be slight changes in the kinase results as well. For backwards compatibility, you can turn this option off to remove phosphatases from the analysis. For more information, check out <a href = "https://github.com/serhan-yilmaz/RokaiApp/tree/master/docs/v2.2.0">the release notes for v2.2.0</a>. </p>
