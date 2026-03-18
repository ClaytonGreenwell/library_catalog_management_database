import pandas as pd
import mysql.connector


# Load Excel file
excel_file = r"C:\Users\mressex\Desktop\School\D532\Community Library\patron.xlsx" 
df = pd.read_excel(excel_file)

# Connect to the database
conn = mysql.connector.connect(
    host='127.0.0.1',
    user='root',
    password='root',
    database='community_library'
)

cursor = conn.cursor()

# Insert patrons into the table
insert_query = """
    INSERT INTO patron (Patron_id, First_Name, Last_Name, Phone_Number, Email_Address)
    VALUES (%s, %s, %s, %s, %s)
"""

for _, row in df.iterrows():
    cursor.execute(insert_query, (
        int(row["Patron_id"]),
        row["First_Name"],
        row["Last_Name"],
        str(row["Phone_Number"]),
        row["Email_Address"]
    ))

conn.commit()
cursor.close()
conn.close()

print("Patron data imported successfully.")