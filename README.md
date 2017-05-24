## Reverse chronological order (not strictly)

Most of the code is hosted on github account [ranjit58](https://github.com/ranjit58)

 - **Predicting survivors in Titanic (Kaggle Competition)** - A initial dig at Titanic survivor prediction from Kaggle competition (accuracy achieved 79%). 
   <br>Using **Python, Scikit-learn, pandas, Seaborn, Jupyter**
   <br>[github.com/ranjit58/ML-titanic](https://github.com/ranjit58/ML-titanic/blob/master/jup-titanic2.ipynb)
   
 - **Metagenomics Forensic application** - Here we built a classification model to predict whether two microbiome samples belong to same individual. The microbiome (metagenomics) profile of two samples were compared to generate several new features ( representing similarity/dissimilarity). Plot the similarity distribution charts between classes. Later used machine learning model (Logistic regression) to build classifier. Labelled data for learning patterns is created from [Human Microbiome Project](http://hmpdacc.org/).
<br>Using **R, ggplot2, CARET, Amazon EC2**
> - Feature generation - [mgSNP pipeline](https://github.com/ranjit58/mgSNP)
> - R code for feature processing and visualization - [snpdis2.R](https://github.com/ranjit58/Summary/blob/master/forensic/snpdis2.R)
> - R package CARET used for machine learning. After trying various ML models we found logistic regression works best for our set of features. [Final Model](https://github.com/ranjit58/Summary/blob/master/forensic/Classifier/transplant_model_final.R)
> - [Training](https://github.com/ranjit58/Summary/blob/master/forensic/xHMP93_21.png) | [Test](https://github.com/ranjit58/Summary/blob/master/forensic/xTRANS93_21.png) | [Performance](https://github.com/ranjit58/Summary/blob/master/forensic/Classifier/accuracy.png)
</P>

- **Compare Kits** : A visual comparison and of two methods (RNAseq) measuring same biological samples
<br>Using **R, Salmon and ggplot2**
> - Data preparation - https://rawgit.com/ranjit58/Summary/master/brad/data_processing.html
> - Data analysis and visualization - https://rawgit.com/ranjit58/Summary/master/brad/kit_analysis.html


-  SNP variation analysis and visualisation - From RYGB paper
 https://rawgit.com/ranjit58/Summary/master/RYGB/RYGB-MGSNP2.html
<br>Using **R and ggplot2**

- **Microbiome analysis pipeline** :  Pipeline for large scale microbiome data analysis on HPC cluster. [QWRAPv3](https://github.com/ranjit58/QWRAPv3)
<br>Using **Bash, R**

- **Workflow management** : On HPC Clusetr with DAG + resources management [Pipeline_test](https://github.com/ranjit58/PIPELINE_test)
<br>Using Snakemake (cousin of make)

- **Article ranking system** - This project is still in early stages of understanding features and mapping different data sets. Large scale XML data processing workflow. [STDOC](https://github.com/ranjit58/STDOC)
 <br>Using **Python, XML libraries, Solr**

- Tools developed by students under my supervision
> - [Mkraken](https://github.com/ranjit58/Mkraken) : Parsing and merging kraken output
> - [MetaVar](https://github.com/ranjit58/MetaVar) : Identify hotspot of metagenomic variation in large cohorts
> - [Meta-PCR](https://github.com/ranjit58/META-PCR) : Identify variable metagenomic region surrounded by conserved region for PCR primer development

- **Microbiome explorer** : A web app for storing and retreiving microbiome metadata
<br>Using **Python, Bootstrap and MySQL**
<br>https://github.com/ranjit58/microbiome_explorer

- HPIDB : Host pathogen interaction database [HPIDB](http://www.agbase.msstate.edu/hpi/main.html)
<br>Using **Perl, HTML and MySQL**

... and may more interesting stuff described in these [publications](http://goo.gl/zUHc9k)



