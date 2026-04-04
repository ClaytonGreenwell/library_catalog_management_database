import mysql.connector
import sqlite3
from sqlite3 import Connection
import re

# CONFIGURATION
MYSQL_CONFIG = {
    "host": "127.0.0.1",
    "user": "root",
    "password": "root",
    "database": "community_library"
}

SQLITE_DB_PATH = "community_library.sqlite"



# MYSQL TO SQLITE DATATYPE MAPPING
def map_mysql_type(mysql_type: str) -> str:
    """
    Convert MySQL column types to SQLite-compatible types.
    SQLite is flexible, but we preserve intent.
    """
    t = mysql_type.lower()

    if "int" in t:
        return "INTEGER"
    if "varchar" in t or "text" in t:
        return "TEXT"
    if "date" in t:
        return "TEXT"  # SQLite stores dates as ISO strings
    if "char" in t:
        return "TEXT"

    # fallback
    return "TEXT"



# SCHEMA DEFINITIONS (from your uploaded images)
def create_sqlite_schema(conn: Connection):
    cursor = conn.cursor()

    # Enable FK enforcement
    cursor.execute("PRAGMA foreign_keys = ON;")

    # 1) patron  (parent table)
    cursor.execute("""
        CREATE TABLE patron (
            Patron_id      INTEGER PRIMARY KEY,
            First_Name     TEXT,
            Last_Name      TEXT,
            Phone_Number   TEXT,
            Email_Address  TEXT
        );
    """)

    
    # 2) books  (FK: patron)
    
    cursor.execute("""
        CREATE TABLE books (
            Book_id         INTEGER PRIMARY KEY,
            Author_Names    TEXT NOT NULL,
            Title           TEXT,
            Bookshelves     TEXT,
            Subjects        TEXT,
            Available       TEXT DEFAULT 'Yes',
            Checked_Out_By  INTEGER,
            FOREIGN KEY (Checked_Out_By) REFERENCES patron(Patron_id)
        );
    """)

    
    # 3) transaction_log  (FK: patron, books)
   
    cursor.execute("""
        CREATE TABLE transaction_log (
            Transaction_id  INTEGER PRIMARY KEY,
            Patron_id       INTEGER,
            Checkout_Date   TEXT,
            Return_Date     TEXT,
            Due_Date        TEXT,
            book_id         INTEGER,
            FOREIGN KEY (Patron_id) REFERENCES patron(Patron_id),
            FOREIGN KEY (book_id) REFERENCES books(Book_id)
        );
    """)

    conn.commit()



# COPY DATA FROM MYSQL TO SQLITE

def copy_table(mysql_cursor, sqlite_conn: Connection, table_name: str):
    sqlite_cursor = sqlite_conn.cursor()

    mysql_cursor.execute(f"SELECT * FROM {table_name}")
    rows = mysql_cursor.fetchall()
    col_names = [desc[0] for desc in mysql_cursor.description]

    placeholders = ", ".join(["?"] * len(col_names))
    insert_sql = f"INSERT INTO {table_name} ({', '.join(col_names)}) VALUES ({placeholders})"

    sqlite_cursor.executemany(insert_sql, rows)
    sqlite_conn.commit()

    print(f"Copied {len(rows)} rows into {table_name}")


# MAIN MIGRATION PROCESS
def migrate():
    print("Connecting to MySQL...")
    mysql_conn = mysql.connector.connect(**MYSQL_CONFIG)
    mysql_cursor = mysql_conn.cursor()

    print("Creating SQLite database...")
    sqlite_conn = sqlite3.connect(SQLITE_DB_PATH)

    print("Creating SQLite schema...")
    create_sqlite_schema(sqlite_conn)

    print("Copying data in FK-safe order...")
    copy_table(mysql_cursor, sqlite_conn, "patron")
    copy_table(mysql_cursor, sqlite_conn, "books")
    copy_table(mysql_cursor, sqlite_conn, "transaction_log")

    print("\nVerifying row counts...")
    for table in ["patron", "books", "transaction_log"]:
        mysql_cursor.execute(f"SELECT COUNT(*) FROM {table}")
        mysql_count = mysql_cursor.fetchone()[0]

        sqlite_cursor = sqlite_conn.cursor()
        sqlite_cursor.execute(f"SELECT COUNT(*) FROM {table}")
        sqlite_count = sqlite_cursor.fetchone()[0]

        print(f"{table}: MySQL={mysql_count}, SQLite={sqlite_count}")

    mysql_conn.close()
    sqlite_conn.close()
    print("\nMigration complete! SQLite DB created at:", SQLITE_DB_PATH)


if __name__ == "__main__":
    migrate()