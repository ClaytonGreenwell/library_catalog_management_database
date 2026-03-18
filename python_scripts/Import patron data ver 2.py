import pandas as pd
import mysql.connector


# Load Excel file
excel_file = r"C:\Users\mressex\Desktop\School\D532\Community Library\patron.xlsx"  #<----This will need to be updated to where you have your Excel sheet stored
df = pd.read_excel(excel_file)

# Connect to the database <---- This section will need to be updated your connection criteria
conn = mysql.connector.connect( 
    host='127.0.0.1',
    user='root',
    password='root',
    database='community_library'
)

cursor = conn.cursor()


# Query to check for duplicates
check_query = """
    SELECT COUNT(*) 
    FROM patron 
    WHERE Patron_id = %s
"""

# Insert query
insert_query = """
    INSERT INTO patron (Patron_id, First_Name, Last_Name, Phone_Number, Email_Address)
    VALUES (%s, %s, %s, %s, %s)
"""

# Process each row
for _, row in df.iterrows():
    patron_id = int(row["Patron_id"])

    # Check if this Patron_id already exists
    cursor.execute(check_query, (patron_id,))
    exists = cursor.fetchone()[0]

    if exists:
        print(f"Skipping duplicate Patron_id {patron_id}")
        continue

    # Insert new patron
    cursor.execute(insert_query, (
        patron_id,
        row["First_Name"],
        row["Last_Name"],
        str(row["Phone_Number"]),
        row["Email_Address"]
    ))

conn.commit()
cursor.close()
conn.close()

print("Import complete — duplicates skipped.")
