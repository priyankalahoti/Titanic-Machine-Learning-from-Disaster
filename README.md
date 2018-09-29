# Titanic-Machine-Learning-from-Disaster
The data has been split into two groups:

training set (train.csv)
test set (test.csv)
The training set is used to build machine learning models.Feature engineering is used to create new features. 

The test set is used to see how well model performs on unseen data. For the test set, the ground truth for each passenger is not provided. The job to predict these outcomes. For each passenger in the test set, trained model is used to predict whether or not they survived the sinking of the Titanic.

I have used feature engineering to create some new features like Title, FamilyName etc. And RandomForest model is used to predict survival on test dataset

###Variable Notes
pclass: A proxy for socio-economic status (SES)
1st = Upper
2nd = Middle
3rd = Lower

age: Age is fractional if less than 1. If the age is estimated, is it in the form of xx.5

sibsp: The dataset defines family relations in this way...
Sibling = brother, sister, stepbrother, stepsister
Spouse = husband, wife (mistresses and fiancés were ignored)

parch: The dataset defines family relations in this way...
Parent = mother, father
Child = daughter, son, stepdaughter, stepson
Some children travelled only with a nanny, therefore parch=0 for them.
