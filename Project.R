#reading dataset
data <- read.csv("/Users/vaibhavipandit/Documents/R working Directory/diabetic_data.csv")

summary(data) #missing values are denoted by "?"
str(data) #the data contains mostly integers and categorical variables

## Data Cleaning

# changing column type from numerical to categorical/nominal
data$admission_type_id <- as.factor(data$admission_type_id)
data$discharge_disposition_id <- as.factor(data$discharge_disposition_id)
data$admission_source_id <- as.factor(data$admission_source_id)

# removing duplicate patient numbers
duplicates <- data[duplicated(data$patient_nbr), ]
print(duplicates)
data <- data[!duplicated(data$patient_nbr),]

# view the rows with duplicate values
print(duplicates)

#replacing all "?" with "NA"
data[data == "?"] <- NA
head(data)

# grouping all missing values 
missing_values <- data %>%
  gather(key = "variable", value = "value") %>%
  filter(is.na(value)) %>%
  count(variable) %>%
  arrange(desc(n))

# plotting missing values
ggplot(missing_values, aes(x = reorder(variable, -n), y = n)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Variable', y = 'Number of Missing Values', title = 'Number of Missing Values by Variable') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# there are more than 40% missing values in variables weight, medical_specialty and payer_code.
# this can cause issues in further analysis. These need to be dropped.

# removing variables from the dataset
data$weight = NULL
data$payer_code = NULL
data$medical_specialty = NULL

# removing variables which will not serve the analysis
data$encounter_id = NULL # id which will not have any effect on analysis
data$diag_2 = NULL # will use diag_1 for analysis which is primary
data$diag_3 = NULL # will use diag_1 for analysis which is primary
data$examide = NULL # have only one value "No"
data$citoglipton = NULL # have only one value "No"

# removing all other rows with missing values
data <- na.omit(data)
print(data)

## Explanatory Data Analysis

# Bar Plots

# plotting bar graphs for variables race, gender and age
ggplot(data, aes(x = race, fill = race)) +
  geom_bar() +
  labs(title = "Distribution of Race",
       x = "Race",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal()

ggplot(data, aes(x = age, fill = age)) +
  geom_bar() +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar Plot Observatios:
# Most of our patients are Caucasian. 
# Females are slightly more prone to diabetes and age group.
# 70-80 is mostly admitted.
# Elder population is more prone to diabetes and young.

# Box plots and removing outliers for numerical variables

# listing numerical variables
numerical_vars <- c("time_in_hospital", "num_lab_procedures", "num_procedures",
                    "num_medications", "number_outpatient", "number_emergency",
                    "number_inpatient", "number_diagnoses")

# melting the data for ggplot2
data_long <- data %>%
  select(all_of(numerical_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# plotting box plots
ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Box Plots of Numerical Variables", x = "Variable", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# function to remove outliers
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)] <- NA
  return(x)
}

# apply the function to the numerical variables and remove rows with NA
data_no_outliers <- data %>%
  mutate(across(all_of(numerical_vars), remove_outliers)) %>%
  drop_na()

# melting the data for ggplot2
data_no_outliers_long <- data_no_outliers %>%
  select(all_of(numerical_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# plotting box plots without outliers
ggplot(data_no_outliers_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Box Plots of Numerical Variables Without Outliers", x = "Variable", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot Observatios:
# Variables num_outpatient, num_emergency, and num_inpatient is not adding value to analysis. These need to be dropped.

# dropping variables
data$number_outpatient = NULL
data$number_emergency = NULL
data$number_inpatient = NULL

# Heat Map

# selecting the numerical variables
selected_vars <- data %>%
  select(time_in_hospital, num_lab_procedures, num_procedures, num_medications, number_diagnoses)

# calculating the correlation matrix
cor_matrix <- round(cor(selected_vars, use = "complete.obs"), 2)

# melting the correlation matrix into long format for ggplot2
cor_data <- as.data.frame(as.table(cor_matrix))
names(cor_data) <- c("Variable1", "Variable2", "Correlation")

ggplot(cor_data, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Heatmap of Correlation Matrix",
       x = "Variables",
       y = "Variables")

# Heat Map Observatios:
#Variables num_medications and time_in_hospital have positive correlation.
#Variables num_lab_procedures and time_in_hospital have weak correlation.
#Variables num_medications and num_lab_procedures have positive correlation.


## Modeling




