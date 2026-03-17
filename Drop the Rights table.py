import mysql.connector


# Connect to the database
conn = mysql.connector.connect(
    host='127.0.0.1',
    user='root',
    password='root',
    database='community_library'
)

cursor = conn.cursor()

# Drop the Rights column
alter_query = """
    ALTER TABLE books
    DROP COLUMN Rights;
"""

cursor.execute(alter_query)
conn.commit()

cursor.close()
conn.close()

print("Column 'Rights' has been dropped from the books table.")
