# import all datasets
library(tidyverse)
files <- c("libraries.csv", "checkouts.csv", "customers.csv", "books.csv")

data_list <- lapply(files, function(file) {
  read_csv(file)
})

names <- c("libraries", "checkouts", "customers", "books")
for (i in 1:length(data_list)) {
  assign(names[i], data_list[[i]])
}

  

# merge all data sets properly and clean data
# Step 1
data_model <- checkouts %>% 
  inner_join(libraries, by = c("library_id" = "id")) %>%
  mutate(name = tolower(gsub("\\s+", " ", name)), # remove wherever thes is more than one white space
                city = tools::toTitleCase(tolower(gsub("\\s+", " ", city))), # same
                region = toupper(gsub("\\s+", " ", region)), # same
                postal_code = as.numeric(gsub("[^0-9]", "", postal_code)), # extract numbers only
                date_checkout = as.Date(date_checkout),
                date_returned = as.Date(date_returned)) %>%
  rename(checkouts_id = id,
                library_name = name,
                library_street_address = street_address,
                library_city = city,
                library_region = region,
                library_postal_code = postal_code) 



# Step 2
data_model <- books %>% 
  inner_join(data_model, by = c("id" = "checkouts_id")) %>%
  mutate(title = tolower(gsub("\\s+", " ", title)), # remove wherever thes is more than one white space
         authors = gsub("\\[|\\]", "", authors), # get rid of square brackets
         categories = gsub("\\[|\\]", "", categories), # same
         publishedDate = as.Date(publishedDate),
         price = as.numeric(price),
         pages = as.double(pages)) %>%
  rename(book_id = id, 
         book_title = title,  
         book_authors = authors, 
         book_publisher = publisher, 
         published_date = publishedDate,
         book_categories = categories,
         book_price = price,
         book_pages = pages)

data_model <- customers %>% 
  inner_join(data_model, by = c("id" = "patron_id")) %>%
  mutate(city = tools::toTitleCase(tolower(gsub("\\s+", " ", city))), # remove wherever thes is more than one white space
                zipcode = as.numeric(gsub("[^0-9]", "", zipcode)), # extract numbers only
                zipcode = as.numeric(substr(zipcode, 1, nchar(zipcode) - 1)), # get rid of last disigt
                education = tolower(gsub("\\s+", " ", education)), # remove wherever thes is more than one white space
                occupation = tolower(gsub("\\s+", " ", occupation)), # same
                gender = tolower(gender),
                birth_date = as.double(substr(birth_date, 1, 4)),
                date_diff = date_returned - date_checkout,
                late_return = ifelse(date_returned - date_checkout > 28, TRUE, FALSE)) %>%
  rename(customer_id = id, 
                customer_name = name,  
                customer_street_address = street_address, 
                customer_city = city, 
                customer_state = state,
                customer_postal_code = zipcode,
                customer_birth_date = birth_date,
                customer_gender = gender,
                customer_education = education,
                customer_occupation = occupation) %>%
  filter(date_diff >= 0, 
         date_returned <= Sys.Date(), # could not be greater than today's date
         date_checkout <= Sys.Date(), # same for this
         lubridate::year(date_checkout) == 2018) # all other years are from the beginning of 19th century



# Let's see how number of book pages is correlated with late return
data_model %>%
  filter(!is.na(book_pages)) %>% 
  ggplot(aes(x = as.factor(late_return), y = book_pages)) +
  geom_boxplot() +
  labs(x = "Late Return", y = "Book Pages") +
  ggtitle("Boxplots of Book Pages by Late Return Status")



# Let's create buckets based on number of book pages and some more structured analysis
summary(data_model$book_pages)

data_model <- data_model %>%
  mutate(book_pages_range = case_when(
    book_pages > 200 & book_pages <= 400 ~ "200-400",
    book_pages > 400 & book_pages <= 600 ~ "401-600",
    book_pages > 600 & book_pages <= 800 ~ "601-800",
    book_pages > 800 & book_pages <= 1000 ~ "801-1000",
    book_pages > 1000 ~ ">1000"))



# Let's figure out the proportion of late returns by book pages
data_model %>%
  group_by(book_pages_range) %>%
  summarise(number_of_checkouts = sum(!is.na(late_return)), proportion_late = mean(late_return == TRUE, na.rm = TRUE)) %>%
  arrange(desc(proportion_late)) %>%
  filter(!is.na(book_pages_range)) %>%
  ggplot(aes(x = reorder(book_pages_range, -proportion_late), y = proportion_late)) +
  geom_bar(stat = "identity") +
  labs(x = "Book Pages", y = "Proportion of Late Returns") +
  ggtitle("Bar Chart of Proportion of Late Returns by Book Pages") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Let's figure out how the proportion is related to number of checkots by creating a simple table
data_model %>%
  group_by(book_pages_range) %>%
  summarise(number_of_checkouts = sum(!is.na(late_return)), proportion_late = mean(late_return == TRUE, na.rm = TRUE)) %>%
  arrange(desc(proportion_late)) %>%
  filter(!is.na(book_pages_range))


# Let's figure out how the proportion is related to specific books
data_model %>%
  filter(!is.na(book_title)) %>% 
  group_by(book_title) %>%
  summarise(number_of_checkouts = sum(!is.na(late_return)), proportion_late = mean(late_return == TRUE, na.rm = TRUE)) %>%
  arrange(desc(proportion_late)) %>% 
  head(20)
  

# Let's figure out the proportion of late returns by book pages and occupation
data_model %>%
  filter(book_pages_range != ">1000") %>%
  group_by(customer_occupation, book_pages_range) %>%
  summarise(number_of_checkouts = sum(!is.na(late_return)), proportion_late = mean(late_return == TRUE, na.rm = TRUE)) %>%
  filter(!is.na(book_pages_range) & customer_occupation != "NA" & customer_occupation != "others") %>%
  ggplot(aes(x = customer_occupation, y = proportion_late, fill = book_pages_range)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Customer Occupation", y = "Proportion of Late Returns", fill = "Book Pages Range") +
  ggtitle("Grouped Bar Chart of Proportion of Late Returns by Customer Occupation and Book Pages Range") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Let's figure out the proportion of late returns by book pages and education
data_model %>%
  filter(book_pages_range != ">1000") %>%
  group_by(customer_education, book_pages_range) %>%
  summarise(number_of_checkouts = sum(!is.na(late_return)), proportion_late = mean(late_return == TRUE, na.rm = TRUE)) %>%
  filter(!is.na(book_pages_range) & customer_education != "NA" & customer_education != "others") %>%
  ggplot(aes(x = customer_education, y = proportion_late, fill = book_pages_range)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Customer Education", y = "Proportion of Late Returns", fill = "Book Pages Range") +
  ggtitle("Grouped Bar Chart of Proportion of Late Returns by Customer Education and Book Pages Range") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# create Boolean column called portland_postal_code
portland_postal_codes <- c(97229, 97206, 97080, 97219, 97230, 97202, 97030, 97236, 
                           97233, 97203, 97266, 97217, 97211, 97213, 97220, 97214,
                           97212, 97060, 97215, 97239, 97209, 97216, 97201, 97232,
                           97218, 97210, 97272, 97221, 97259, 97255, 97205, 97024, 
                           97227, 97271, 97231, 97019, 97014, 97049, 97204, 97208, 
                           97258, 97299, 97010, 97282, 97207, 97228, 97238, 97242, 
                           97240, 97253, 97251, 97254, 97256, 97280, 97286, 97283,
                           97291, 97290, 97293, 97292, 97296, 97294, 97250, 97252)

data_model <- data_model %>%
  mutate(portland_postal_code = customer_postal_code %in% portland_postal_codes)

summary(data_model$portland_postal_code)



# return the table grouped by education, portland postal code, and book pages to see proportion of late returns
data_model %>%
  filter(!is.na(customer_education) & !is.na(book_pages_range)) %>%
  group_by(customer_education, portland_postal_code, book_pages_range) %>%
  summarise(number_of_books = n(),
            proportion_late = mean(late_return == TRUE)) %>% 
  filter(number_of_books >= 10) %>% 
  arrange(desc(proportion_late)) 
  


# Let's see if book categorization is clean
data_model %>%
  group_by(book_categories) %>%
  summarise(number_of_checkouts = sum(!is.na(late_return)), proportion_late = mean(late_return == TRUE, na.rm = TRUE)) %>%
  filter(!is.na(book_categories) & proportion_late >= 0.1) %>%
  arrange(desc(proportion_late)) %>%
  ggplot(aes(x = reorder(book_categories, -proportion_late), y = proportion_late)) +
  geom_bar(stat = "identity") +
  labs(x = "Book Categories", y = "Proportion of Late Returns") +
  ggtitle("Bar Chart of Proportion of Late Returns by Book Categories") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Let's figure out the proportion of late returns by book pages and Portland postal codes
data_model %>%
  filter(book_pages_range != ">1000") %>%
  group_by(portland_postal_code, book_pages_range) %>%
  summarise(number_of_checkouts = sum(!is.na(late_return)), proportion_late = mean(late_return == TRUE, na.rm = TRUE)) %>%
  filter(!is.na(book_pages_range)) %>%
  ggplot(aes(x = portland_postal_code, y = proportion_late, fill = book_pages_range)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Portland Postal Code", y = "Proportion of Late Returns", fill = "Book Pages Range") +
  ggtitle("Grouped Bar Chart of Proportion of Late Returns by Portland Postal Code and Book Pages Range") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create a heatmap visualization of the proportion of late returns by occupation and education
data_model %>%
  filter(customer_education != "NA", customer_education != "others", customer_occupation != "NA", customer_occupation != "others") %>%
  group_by(customer_education, customer_occupation) %>%
  summarise(proportion_late = mean(late_return == TRUE, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = customer_education, y = customer_occupation, fill = proportion_late)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Customer Education", y = "Customer Occupation", fill = "Proportion of Late Returns") +
  ggtitle("Heatmap of Proportion of Late Returns by Customer Education and Occupation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bonus viz for fun!
# a heatmap visualization of the late return for same postal codes (library and customer postal codes)
data_model %>%
  filter(!is.na(library_postal_code)) %>%
  mutate(library_postal_code = as.factor(library_postal_code),
                customer_postal_code = as.factor(customer_postal_code)) %>%
  group_by(library_postal_code, customer_postal_code) %>%
  summarise(proportion_late = mean(late_return == TRUE, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = library_postal_code, y = customer_postal_code, fill = proportion_late)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Library Postal Code", y = "Customer Postal Code", fill = "Late Return") +
  ggtitle("Heatmap of Proportion of Late Returns by Library Postal Code and Customer Postal Code") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# install the caret package
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# load the caret package
library(caret)

set.seed(123) # ensure reproducibility

# data partition
test_index <- createDataPartition(data_model$late_return, times = 1, p = 0.8, list = FALSE)
test_set <- data_model[test_index, ]
train_set <- data_model[-test_index, ]



### Build 4 models and compare their strengths and weakness

# define a list of formulas for the models
formulas <- list(
  late_return ~ book_pages,
  late_return ~ customer_education,
  late_return ~ customer_occupation,
  late_return ~ portland_postal_code
)

# Initialize an empty list to store results
results <- list()

# Loop through each formula
for (i in seq_along(formulas)) {
  # Fit the model
  fit_glm <- glm(formulas[[i]], data=train_set, family = binomial())
  
  # Predict
  p_hat_glm <- predict(fit_glm, newdata=test_set, type="response")
  
  # Calculate ROC and best threshold
  library(pROC)
  roc_obj <- roc(response = test_set$late_return, predictor = p_hat_glm)
  coords_obj <- coords(roc_obj, "best", ret=c("threshold", "accuracy", "sensitivity"), best.method="closest.topleft")
  
  # Use the best threshold for prediction
  best_threshold <- coords_obj$threshold
  y_hat_glm <- ifelse(p_hat_glm > best_threshold, 1, 0)
  
  # Calculate metrics
  conf_matrix <- table(Predicted = y_hat_glm, Actual = test_set$late_return)
  accuracy_glm <- sum(diag(conf_matrix)) / sum(conf_matrix)
  sensitivity_glm <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  specificity_glm <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
  precision_glm <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  recall_glm <- sensitivity_glm
  f1_glm <- 2 * (precision_glm * recall_glm) / (precision_glm + recall_glm)
  
  # Store results
  results[[i]] <- list(
    accuracy = accuracy_glm, 
    sensitivity = sensitivity_glm, 
    specificity = specificity_glm, 
    precision = precision_glm, 
    recall = recall_glm, 
    f1 = f1_glm, 
    best_threshold = best_threshold
  )
}

# Convert the list of results to a data frame for easier viewing
results_df <- do.call(rbind, lapply(results, function(x) as.data.frame(t(unlist(x)))))
rownames(results_df) <- c("Model 1", "Model 2", "Model 3", "Model 4")
results_df


### Let's build additional for 4 models by going deeper into customer's education with book_pages as a predictor

# define unique customer education levels excluding NAs
education_levels <- unique(na.omit(train_set$customer_education))

# initialize an empty list to store results
results <- list()

# loop through each education level
for (i in seq_along(education_levels)) {
  # Subset the train and test set for the current education level
  train_subset <- subset(train_set, customer_education == education_levels[i])
  test_subset <- subset(test_set, customer_education == education_levels[i])
  
  # fit the model
  fit_glm <- glm(late_return ~ book_pages, data=train_subset, family = binomial())
  
  # predict
  p_hat_glm <- predict(fit_glm, newdata=test_subset, type="response")
  
  # calculate ROC and best threshold
  roc_obj <- roc(response = test_subset$late_return, predictor = p_hat_glm)
  coords_obj <- coords(roc_obj, "best", ret=c("threshold", "accuracy", "sensitivity"), best.method="closest.topleft")
  
  # use the best threshold for prediction
  best_threshold <- coords_obj$threshold
  y_hat_glm <- ifelse(p_hat_glm > best_threshold, 1, 0)
  
  # calculate metrics
  conf_matrix <- table(Predicted = y_hat_glm, Actual = test_subset$late_return)
  accuracy_glm <- sum(diag(conf_matrix)) / sum(conf_matrix)
  sensitivity_glm <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  specificity_glm <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
  precision_glm <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  recall_glm <- sensitivity_glm
  f1_glm <- 2 * (precision_glm * recall_glm) / (precision_glm + recall_glm)
  
  # store results
  results[[i]] <- list(
    accuracy = accuracy_glm, 
    sensitivity = sensitivity_glm, 
    specificity = specificity_glm, 
    precision = precision_glm, 
    recall = recall_glm, 
    f1 = f1_glm, 
    best_threshold = best_threshold
  )
}

# Convert the list of results to a data frame for easier viewing
results_df <- do.call(rbind, lapply(results, function(x) as.data.frame(t(unlist(x)))))
rownames(results_df) <- paste("Model", education_levels)
results_df


# Conclusions:

# Factors that influence late book returns are: number of book pages, customer's place of residence,
# education,  and occupation (although not as much as education).

# I would recommend the library:
# 1. start the project of arranging the database by organizing the categorization of books.
# 2. create such software that will disable logical errors during data entry such as:
# a.) taking a book before the time it actually happened or taking books at a time in the future that has not yet happened.
# b.) returning books before the time the book was taken or returning books to a time in the future that has not yet occurred. 
# 3. Be more flexible with regard to books that have a large number of pages and to dynamically determine the limit for late book returns.
# 4. Provide students with benefits in the form of more places for reading and more books that are most in demand among the student population,
# so that the books that are most often returned late are kept in the library. They can also provide books in digital formats for this purpose.
# 5. enable the return of books by mail or establish cooperation with other libraries located near the place of residence of customers
# and thus reduce the probability of late return of books. 
# 6. find ways to sanction late returns, especially those books where the likelihood of late returns has been extremely high in the past (books related to sales, business, and finance occupation).
