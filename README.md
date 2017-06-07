# Machine Learning models in Spanish using R.

Here we can see some exercises of Machine Learning using R (decision trees, deep learning, boosting,...), and all of it explained in Spanish.
I recomend reading the .md documents in the web, and download the .Rmd to run it using R.

## Cluster_PC_Tree

We can see an example of clustering analysis of Crimen.dat. Various techniques of dimensionality reduction are shown in the second part such as PCA, using data prvided by a package of R.
In the last exercise we use the data Default.txt to construct diferents decision trees.

## Gam

 Using the data Auto from the library ISLR, we have to make a predictive model. We should use a generalized linear model (such as poisson model).
In the second part of the document, we use the data college of the library ISLR, we have to make a gam model in order to predict the Grad.Rate variable.

## Knn

Using "datawork.csv" we have to do two predictive models, one of regression, and the other of clasification (we use
two diferent columns to do that).

## NaiveBayes_LDA

 The BreastCancer data from the mlbench package provide tumour instances, the objective is to identify each of a number of benign or malignant classes. In this example we use 
Naive Bayes and linear discriminant analysis.

## RF_nnet_h2o

We use the LetterRecognition data from the library mlbench in order to make a model who predict two characters. The models used are random forest and neural networks(nnet and deep learning using h2o).

## SVM.

We load the Glass data from the mlbench package, which shows 7 types of glass. Our goal is use Suport Vector Machines models wich allow us to predict the type of glass.

## letter_h2o

We use the LetterRecognition data from the library mlbench ir order to make a model who allow us to predict characters. Here we use deep learning models, some of the steps are to slow to 
compute, so some models are saved: modelo_2-1000,modelo_grid_8, modelo_acp1, modelo_acp1_train2, modelo_h2o_grid_8.RData and modelo_ho_grid_2.RData.

## Unbalanced

 We have to make a predictive model, clasification, but one class is much larger than the other. We use diferent techniques in order to deal with unbalanced data (Insolvencia.RData).

