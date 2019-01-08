CeTrAn
======

CeTrAn is a R script to analyse trajectory data, at the moment only data obtained from Buritrack a tracker designed for fruit flies running in the Buridan assay. see also www.buridan.sourceforge.net.

The progam used to work with rgg (then it needs java 6 to run correctly), but was recently moved to shiny (not tested yet). You need to install Rstudio and open the server.r file and click the run app button.

To run the latest version, you can also open R and run:
```{r}
library(shiny)
shiny::runGitHub('CeTrAn', 'jcolomb')
```

But this will download the complete app, including the test data.


If you publish a scientific paper using CeTran, please refer to:
Colomb J, Reiter L, Blaszkiewicz J, Wessnitzer J, Brembs B (2012) Open Source Tracking and Analysis of Adult Drosophila Locomotion in Buridan's Paradigm with and without Visual Targets. PLoS ONE 7(8): e42247. doi:10.1371/journal.pone.0042247

<a rel="license" href="http://creativecommons.org/licenses/by/3.0/deed.en_US"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by/3.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/3.0/deed.en_US">Creative Commons Attribution 3.0 Unported License</a>.
