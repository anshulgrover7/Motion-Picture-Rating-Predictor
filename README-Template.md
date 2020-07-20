# Project Title

This project aims at predicting the MPAA rating of a film based on the CRISP-DM methodology. In this project, various classification based machine learning algorithms are implemented and hence compared among them. The legacy data is acquired from https://www.kaggle.com/danielgrijalvas/movies and is expanded by adding data about the movies from Twitter and Youtube. Artifical Neural Network (ANN) achieved the highest classification accuracy of 71% for classifying MPAA rating and Random Forest achieved the highest accuracy of 77% in classifying movie as a family movie or not.


## Getting Started

I have added comment to each line of code in order to understand what each step does. First few lines of the code does the pre-processing of the master data file and the following code runs various classification based machine learning algorithms.

### Prerequisites

R Studio

```
If you are using Anaconda, RStudio comes along with it, alternatively, you can download it from https://rstudio.com.
```

## Deployment

Just install RStudio and run mpaa.R

## Few Machine Learning Algorithms used

* [RandomForest](https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest)- used with default and tuned parameters for comparison
* [NaiveBayes](https://www.rdocumentation.org/packages/naivebayes/versions/0.9.7) - used to convert two attributes to bins
* [h2o](https://cran.r-project.org/web/packages/h2o/index.html) - ANN used to achieve higher accuracy 

## Authors

* **Abhishek Jain** 
* **Rahul Chomal** 
* **Anuj Bhambri** 

See the full paper on (https://github.com/anshulgrover7/Motion-Picture-Rating-Predictor/blob/master/Paper/final_paper.pdf)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Mr. Ade Fajemisin, National College of Ireland
* Mr. Michael Bradford, National College of Ireland