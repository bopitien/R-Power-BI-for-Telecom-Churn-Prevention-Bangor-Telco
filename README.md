# R-Power-BI-for-Telecom-Churn-Prevention-Bangor-Telco

## INTRODUCTION

This project focuses on understanding customer behaviour in the telecommunications industry using data from a fictional 
company named Bangor Telco. The main challenge for Bangor Telco and many other companies in this industry is 
keeping their customers, which is also known as reducing customer churn. 
To tackle this problem, I will first employ three popular data analysis methods: Decision Trees, Logistic Regression, and 
k-nearest Neighbors (kNN). Each of these methods will help predict which customers might leave the company (churn) 
and understand different groups of customers based on their behaviour and characteristics. 
Furthermore, an essential part of this project is creating a Data Science Dashboard. This tool will use the model to show 
how well the predictions work and allow users to input information and get predictions. This dashboard is not just for 
showing the results; it is a practical tool for the company to make data-driven decisions. By tackling this project, I aim to 
provide valuable insights to Bangor Telco.  
These insights can help them improve their services, make their customers happier, and reduce the number of customers 
leaving the company. This project shows how data science can be used in real-world situations to help businesses 
understand their customers better and make smarter decisions. 

## DATA COLLECTION 

In this section, I connected to Bangor Telco’s MySQL database to retrieve customer data crucial for the analysis by using 
the RMySQL package in R to establish a connection using essential credentials: username, password, host, database 
name, and port number. 
To retrieve the data, the SQL query “SELECT * FROM customer_churn.customers” is executed to retrieve all records 
from the customer table.  After data retrieval, the database connection is closed to maintain security and resource 
efficiency. 

## CONCLUSION 

This analysis begins from refining data to creating predictive models that turn customer information into actionable 
insights. I focused on preparing the data meticulously, ensuring data quality, integrity and consistency. Further 
uncovering hidden trends through Exploratory analysis, and then choosing the right models to meet the specific 
business needs. From my models built it appears that the decision tree model is more efficient based on the metrics 
and outcome. However, when choosing the right model fits, it is iterative depending on the goal of the business.  
The development of these models is more than just an exercise in data science, it is about gaining a deeper 
connection with customer behaviours and needs. The resulting dashboard serves as a practical and straightforward 
interface for complex data-driven predictions. The steps taken in this analysis have practical applications in a real
world business setting and will help make informed decisions, improve customer relations, and evolve the business.
