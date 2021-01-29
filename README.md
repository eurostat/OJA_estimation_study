OJA_estimation_study
=========

Experimental tools for estimation of Job Vacancies Statistics (JVS) from Online Job Advertisements (OJA).
---

The material provided herein will enable you to reproduce the experiments presented in _Eurostat_ statistical working paper on [**Inferring job vacancies from online job advertisements**](https://ec.europa.eu/eurostat/web/products-statistical-working-papers/-/KS-TC-20-008) (_cite this source code or the reference's doi: [10.2785/963837](http://dx.doi.org/10.2785/963837)_). Further details are also available in the working paper [below](#References).

**Description**

The source code is provided as *.Rmd* files supported with other functions and R codes in the [**_r-codes/_**](r-codes) folder to prepare the data. The .Rmd files allows you to reproduce the results of the study in case you have access to the original detailed data. 

The data was collected and provided by Cedefop for 28 four European countries on online job advertisements since 2018. The data stored in S3 buckets and could be accessed through Athena service. In the code the name of the Athena table is provided together with the query to get the base datasets which were used in the later steps in the paper.  

**Usage**

To recreate the graphs and tables, you can run *.Rmd* files. At the begining is listed the required libraries.


**<a name="About"></a>About**

<table align="center">
    <tr>     <td  rowspan="4" align="center" width="140px"> <a href="https://ec.europa.eu/eurostat/documents/3888793/10879237/KS-TC-20-008"><img title="Inferring job vacancies from online job advertisements — 2021 edition" alt="Inferring job vacancies from online job advertisements — 2021 edition" src="https://ec.europa.eu/eurostat/documents/3888793/12287170/KS-TC-20-008-EN-ICON.jpg"></a></td>
<td align="left"><i>authors</i></td> <td align="left"> <a href="mailto:m.beresewicz@stat.gov.pl ">Beręsewicz M.</a>, 
	<a href="mailto:rpater@wsiz.rzeszow.pl">Pater R.</a></td> </tr> 
    <tr> <td align="left"><i>version</i></td> <td align="left">1.0</td> </tr> 
    <tr> <td align="left"><i>status</i></td> <td align="left">2021 &ndash; <b>closed</b></td> </tr> 
    <tr> <td align="left"><i>license</i></td> <td align="left"><a href="https://joinup.ec.europa.eu/sites/default/files/custom-page/attachment/eupl_v1.2_en.pdf">EUPL</a> <i>(cite the source code or the reference above!)</i></td> </tr> 
</table>

**<a name="References"></a>References** 

* Beręsewicz M. and Pater R., (2021): [**Inferring job vacancies from online job advertisements**](https://ec.europa.eu/eurostat/web/products-statistical-working-papers/-/KS-TC-20-008), _Eurostat_ Statistical Working Paper KS-TC-20-008, doi: [10.2785/963837](http://dx.doi.org/10.2785/963837).
