# Null value counts and percentages for each sheet in the FIZZ, MNST, and PEP datasets.
import pandas as pd
fizz = pd.ExcelFile(r'C:\Users\viraj\OneDrive\Desktop\CAPSTONE RU\FIZZ_annual_reports_last_10_years.xlsx')
mnst = pd.ExcelFile(r'C:\Users\viraj\OneDrive\Desktop\CAPSTONE RU\Mnst_annual_reports_last_10_years.xlsx')
pep = pd.ExcelFile(r'C:\Users\viraj\OneDrive\Desktop\CAPSTONE RU\PEP_annual_reports_last_10_years.xlsx')


def check_null_values(excel_file):
    for sheet_name in excel_file.sheet_names:
        df = excel_file.parse(sheet_name)  # Load the sheet as a DataFrame
        print(f"Null values in sheet '{sheet_name}':")
        print(df.isnull().sum())  # Count of null values per column
        print("\nPercentage of null values per column:")
        print((df.isnull().mean() * 100))  # Percentage of null values per column
        print("\n" + "-"*40 + "\n")


print("FIZZ Dataset:")
check_null_values(fizz)

print("Mnst Dataset:")
check_null_values(mnst)

print("PEP Dataset:")
check_null_values(pep)
