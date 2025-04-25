rm(list=ls())
library(tidyverse)
PERMID <- 3437712
PERMID <- as.numeric(gsub("//D", "", PERMID))
set.seed(PERMID)

library(stringr)
library(gridExtra)


schoolData_raw <- read_csv("schoolData.csv")

schoolData1 <- schoolData_raw %>% 
  clean_names() |>
  rename(
    total_students = total_students_all_grades_excludes_ae,
    reduced_lunch_eligible = reduced_price_lunch_eligible_students,
    free_reduced = free_and_reduced_lunch_students,           
    hispanic = hispanic_students,              
    black = black_or_african_american_students,                       
    white = white_students,                       
    asian = asian_or_asian_pacific_islander_students)
schoolData1 <- schoolData1 |> 
  filter(
    str_detect(school_level, regex("Primary|Middle|High|Elementary|Secondary", ignore_case = TRUE)),
    str_detect(school_type, regex("regular", ignore_case = TRUE))) %>%
  select(-school_type)

cleanValues <- function(data) {
  cleaned_data <- data %>%
    mutate(across(where(is.character), ~ na_if(.x, "†"))) %>%
    mutate(across(where(is.character), ~ na_if(.x, "–"))) %>%
    mutate(across(where(is.character), ~ na_if(.x, "‡")))
  cleaned_data <- type.convert(cleaned_data, as.is = TRUE)
  return(cleaned_data)
}

schoolData <- cleanValues(schoolData1)
schoolData <- schoolData %>%
  filter(!is.na(agency_name))

########################### PART 1 Q2

districtData_raw <- read_csv("districtData.csv")

districtData1 <- districtData_raw %>%
  clean_names() %>% # Convert column names to snake_case
  rename(
    iep_students = individualized_education_program_students ,
    total_teachers = full_time_equivalent_fte_teachers) %>%
  cleanValues()

############################## P1 Q3

teacherSalaryData <- read_csv("teacherSalaryData.csv")

teacherSalaryData1 <- teacherSalaryData %>%
  clean_names()
  
############################## P1 Q4

incomeData <- read_csv("incomeData.csv")

countyIncomeData <- incomeData %>%
  clean_names() %>%
  filter(str_detect(geo_name, ", CA$")) %>% 
  select(geo_fips, year, income, population) %>% 
  mutate(income = as.numeric(income))
# view(countyIncomeData)

##################### P1 Q5

cpiData_raw <- read_csv("CPIAUCSL.csv")

cpiData <- cpiData_raw %>%
  clean_names() |>
  mutate(date = as.Date(date)) |>
  filter(month(date) == 8) |>
  mutate(
    year = year(date),
    cpi = cpiaucsl / cpiaucsl[year == 2023] * 100
  ) %>%
  select(year, cpi)

############################################# PART 2

schoolData2 <- schoolData %>%
  mutate(
    asian_perc = asian / total_students,
    black_perc = black / total_students,
    hispanic_perc = hispanic / total_students,
    white_perc = white / total_students,
    free_reduced_perc = free_reduced / total_students,
    migrant_students_perc = migrant_students / total_students
  )

schoolData_summary1 <- schoolData2 %>%
  group_by(agency_id_nces_assigned, year) %>%
  summarize(
    num_schools = n(),
    num_students = sum(total_students, na.rm = TRUE),
    charter_school_perc = mean(charter_school == "1-Yes", na.rm = TRUE),
    title_i_perc = mean(school_wide_title_i == "1-Yes", na.rm = TRUE),  
    asian_perc = weighted.mean(asian_perc, total_students, na.rm = TRUE),
    black_perc = weighted.mean(black_perc, total_students, na.rm = TRUE),
    hispanic_perc = weighted.mean(hispanic_perc, total_students, na.rm = TRUE),
    white_perc = weighted.mean(white_perc, total_students, na.rm = TRUE),
    free_reduced_perc = weighted.mean(free_reduced_perc, total_students, na.rm = TRUE),
    migrant_students_perc = weighted.mean(migrant_students_perc, total_students, na.rm = TRUE),
    .groups = "drop"  # Ungroup after summarizing
  )

schoolData_summary1_na <- schoolData_summary1 %>%
  filter(!is.na(agency_id_nces_assigned))

districtData1_na <- districtData1 %>%
  filter(!is.na(agency_id_nces_assigned))

districtData2 <- schoolData_summary1_na %>%
  left_join(districtData1_na, by = c("year", "agency_id_nces_assigned"))

############################### PART 2 Q2

name_cleaner <- function(data, column, list_of_words) {
  data[[column]] <- str_replace_all(data[[column]], "[[:punct:]]", " ")
  words_pattern <- paste0("\\b(", paste(list_of_words, collapse = "|"), ")\\b")
  data[[column]] <- str_remove_all(data[[column]], regex(words_pattern, ignore_case = TRUE))
  data[[column]] <- str_squish(data[[column]])
  data[[column]] <- str_to_title(data[[column]])
  return(data)
}

districtData3 <- name_cleaner(districtData2, "agency_name", c("district"))
teacherSalaryData2 <- name_cleaner(teacherSalaryData1, "employer_name", c("district"))

districtData4 <- districtData3 %>%
  left_join(teacherSalaryData2, by = c("agency_name" = "employer_name", "year"))

districtData <- districtData4 %>%
  left_join(cpiData, by = "year")
# view(districtData)

######################## PART 2 Q3

countyData1 <- districtData %>%
  group_by(year, county_number) %>%
  summarize(
    charter_school_perc = weighted.mean(charter_school_perc, num_schools, na.rm = TRUE),
    title_i_perc = weighted.mean(title_i_perc, num_schools, na.rm = TRUE),
    teacher_salary = weighted.mean(salary, total_teachers, na.rm = TRUE),
    
    asian_perc = weighted.mean(asian_perc, num_students, na.rm = TRUE),
    white_perc = weighted.mean(white_perc, num_students, na.rm = TRUE),
    black_perc = weighted.mean(black_perc, num_students, na.rm = TRUE),
    hispanic_perc = weighted.mean(hispanic_perc, num_students, na.rm = TRUE),
    free_reduced_perc = weighted.mean(free_reduced_perc, num_students, na.rm = TRUE),
    migrant_students_perc = weighted.mean(migrant_students_perc, num_students, na.rm = TRUE),
    
    num_districts = n_distinct(agency_id_nces_assigned),
    num_schools = sum(num_schools, na.rm = TRUE),
    num_staff = sum(total_staff, na.rm = TRUE),
    num_teachers = sum(total_teachers, na.rm = TRUE),
    num_students = sum(num_students, na.rm = TRUE),
    
    county_name = first(county_name)
  ) %>%
  ungroup()

countyData2 <- countyData1 %>%
  mutate(county_number = as.numeric(county_number)) %>%
  left_join(countyIncomeData %>% mutate(geo_fips = as.numeric(geo_fips)), 
            by = c("county_number" = "geo_fips", "year"))

countyData <- countyData2 %>%
  left_join(cpiData, by = "year") %>%
  mutate(
    real_income = income / cpi * 100,  
    real_teacher_salary = teacher_salary / cpi * 100)
# view(countyData)
# view(schoolData)

################################# PART 3 Q1

nslp_locale_table <- schoolData %>%
  filter(!is.na(national_school_lunch_program) & !is.na(locale)) %>%
  mutate(locale = case_when(
    str_detect(locale, regex("city", ignore_case = TRUE)) ~ "City",
    str_detect(locale, regex("suburb", ignore_case = TRUE)) ~ "Suburb",
    str_detect(locale, regex("town", ignore_case = TRUE)) ~ "Town",
    str_detect(locale, regex("rural", ignore_case = TRUE)) ~ "Rural",
    TRUE ~ locale
  )) %>%
  mutate(national_school_lunch_program = case_when(
    national_school_lunch_program == "Yes under Community Eligibility Option (CEO)" ~ "Yes with CEO",
    national_school_lunch_program == "No" ~ "No",
    TRUE ~ "Yes without CEO"
  )) %>%
  tabyl(year, national_school_lunch_program, locale) %>%
  adorn_percentages("row") |>
  adorn_pct_formatting() 

########################### Part 3 Q2

salaryQuartilesData <- countyData %>%
  filter(!is.na(real_income)) %>%  
  group_by(year) %>%
  summarise(
    income_min = min(real_income, na.rm = TRUE),
    income_25 = quantile(real_income, 0.25, na.rm = TRUE),
    income_50 = quantile(real_income, 0.5, na.rm = TRUE),
    income_75 = quantile(real_income, 0.75, na.rm = TRUE),
    income_max = max(real_income, na.rm = TRUE),
    .groups = "drop"
  )

########################## Part 3 Q3

filtered_school_data <- schoolData %>%
  filter(year %in% c(1999, 2009, 2019)) %>%
  filter(!is.na(free_reduced) & !is.na(total_students)) %>% 
  mutate(free_reduced_perc = (free_reduced / total_students) * 100)  

free_reduced_histogram <- ggplot(filtered_school_data, aes(x = free_reduced_perc)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~year, ncol = 3) + 
  labs(
    title = "Distribution of Students Eligible for Free/Reduced Lunch",
    x = "Percentage of Students Eligible",
    y = "Count of Schools"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

############################3 P3 Q4

salary_quartiles_2019 <- salaryQuartilesData |>
  filter(year == 2019)
  

county_income_quartiles_data <- countyData %>%
  filter(year == 2019) %>%
  mutate(
    quartile = case_when(real_income < salary_quartiles_2019$income_25 ~ 1,
                         real_income > salary_quartiles_2019$income_25 &
                           real_income < salary_quartiles_2019$income_50 ~ 2,
                         real_income > salary_quartiles_2019$income_50 &
                           real_income < salary_quartiles_2019$income_75 ~ 3,
                         real_income > salary_quartiles_2019$income_75 ~ 4))

county_income_plot <- ggplot(county_income_quartiles_data, aes(x = reorder(county_name, real_income), y = real_income, fill = factor(quartile))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkblue", "blue", "lightblue", "skyblue"),
                    name = "Quartile",
                    labels = c("Bottom Quartile", "Second Quartile", "Third Quartile", "Top Quartile")) +
  labs(
    title = "2019 Average Real Income by County",
    x = "County",
    y = "Average Real Income") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top")


#view(countyData)

##################### P3 Q5

free_reduced_county_data <- countyData %>%
  filter(year == 2019) %>%
  mutate(quartile = case_when(real_income < salary_quartiles_2019$income_25 ~ 1,
                              real_income > salary_quartiles_2019$income_25 &
                                real_income < salary_quartiles_2019$income_50 ~ 2,
                              real_income > salary_quartiles_2019$income_50 &
                                real_income < salary_quartiles_2019$income_75 ~ 3,
                              real_income > salary_quartiles_2019$income_75 ~ 4))
# view(free_reduced_county_data)

free_reduced_county_plot <- ggplot(free_reduced_county_data, aes(x = reorder(county_name, free_reduced_perc), 
                                                                 y = free_reduced_perc, 
                                                                 fill = factor(quartile))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkblue", "blue", "lightblue", "skyblue"), 
                    name = "Income Quartile",
                    labels = c("Bottom Quartile", "Second Quartile", "Third Quartile", "Top Quartile")) +
  labs(
    title = "Percent of Students Eligible for Free/Reduced-Price Lunch by County (2019)",
    x = "County",
    y = "Percent of Students Eligible") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for clarity
    legend.position = "top")

##################### P3 Q6

filtered_school_data <- schoolData %>%
  filter(year == 2019) %>%
  filter(!is.na(free_reduced) & !is.na(asian) & !is.na(black) & !is.na(hispanic) & !is.na(white)) %>%
  mutate(
    free_reduced_perc = (free_reduced / total_students) * 100,
    asian_perc = (asian / total_students) * 100,
    white_perc = (white / total_students) * 100,
    black_perc = (black / total_students) * 100,
    hispanic_perc = (hispanic / total_students) * 100)
# view(filtered_school_data)

asian_plot <- ggplot(filtered_school_data, aes(x = asian_perc, y = free_reduced_perc)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add fitted line
  labs(title = "Asian Students vs Free/Reduced Lunch", 
       x = "Percentage of Asian Students", 
       y = "Percentage Eligible") +
  theme_minimal()

black_plot <- ggplot(filtered_school_data, aes(x = black_perc, y = free_reduced_perc)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add fitted line
  labs(title = "Black Students vs Free/Reduced Lunch", 
       x = "Percentage of Black Students", 
       y = "Percentage Eligible") +
  theme_minimal()+
  ylim(0, 100)

hispanic_plot <- ggplot(filtered_school_data, aes(x = hispanic_perc, y = free_reduced_perc)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add fitted line
  labs(title = "Hispanic Students vs Free/Reduced Lunch", 
       x = "Percentage of Hispanic Students", 
       y = "Percentage Eligible") +
  theme_minimal()

white_plot <- ggplot(filtered_school_data, aes(x = white_perc, y = free_reduced_perc)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add fitted line
  labs(title = "White Students vs Free/Reduced Lunch", 
       x = "Percentage of White Students", 
       y = "Percentage Eligible") +
  theme_minimal()

grid.arrange(asian_plot, black_plot, hispanic_plot, white_plot, ncol = 2)

