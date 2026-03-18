import mysql.connector

# Connect to the database
conn = mysql.connector.connect(
    host='127.0.0.1',
    user='root',
    password='root',
    database='community_library'
)

cursor = conn.cursor()


#Drop foreign key on patron.Checked_Out
try:
    cursor.execute("ALTER TABLE patron DROP FOREIGN KEY checked_out;")
    print("Dropped foreign key constraint on patron.Checked_Out.")
except:
    print("Foreign key on patron.Checked_Out not found or already removed.")

#Drop the Checked_Out column from patron
try:
    cursor.execute("ALTER TABLE patron DROP COLUMN checked_Out;")
    print("Dropped column Checked_Out from patron.")
except:
    print("Column Checked_Out not found or already removed.")

# Commit and close
conn.commit()
cursor.close()
conn.close()

print("Database normalization complete.")