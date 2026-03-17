import mysql.connector


# Connect to the database
conn = mysql.connector.connect(
    host='127.0.0.1',
    user='root',
    password='root',
    database='community_library'
)

cursor = conn.cursor()

# Drop the LoCC column
alter_query = """
    ALTER TABLE books
    DROP COLUMN LoCC;
"""

cursor.execute(alter_query)
conn.commit()

cursor.close()
conn.close()

print("Column 'LoCC' has been dropped from the books table.")
