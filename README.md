# Overview
The code prepares and models a loan dataset to predict loan approvals. It first loads and cleans the data, calculates the debt-to-income ratio, and creates a binary target for loan status. Relevant columns are selected, and categorical variables are numerically encoded. The data is split (70:30) into training and test sets, scaled, and reduced using Principal Component Analysis (PCA) to simplify modeling. Five models (Naive Bayes, SVM, KNN, Random Forest, and Logistic Regression) are trained and optimized for accuracy. Finally, an ensemble and cross-validated Random Forest model are applied for accurate loan prediction.
## About
The code processes a loan dataset to predict loan decisions by following these steps:
Data Loading and Cleaning: Reads and checks the dataset for missing values, calculates the debt-to-income ratio, and creates a binary target variable for loan decision status.
Data Transformation: Selects key columns and encodes categorical variables (e.g., gender, marital status) as numeric for modeling.
Data Splitting and Scaling: Splits the dataset into training and test sets (70:30 ratio) and scales the features.
Dimensionality Reduction: Applies Principal Component Analysis (PCA) to reduce feature dimensions for simplified modeling.
Model Training: Trains multiple models (Naive Bayes, SVM, KNN, Random Forest, and Logistic Regression), optimizes hyperparameters, and evaluates each model’s accuracy.
Ensemble and Final Model: Combines predictions, applies cross-validation on Random Forest, and selects an optimized model for final prediction accuracy.
## Conclusion
The code successfully processes loan data to predict approvals by combining data preparation, model training, and ensemble techniques. The final model ensures reliable and accurate loan decision predictions with an accuracy of 88.75 percent with Random Forest Modelling.
