import csv

input_file = r"C:\\Users\\mressex\\Desktop\\School\\D532\\Community Library\\gutenberg_metadata.csv"          # original CSV (you will need to update this to your own path)
output_file = r"C:\\Users\\mressex\\Desktop\\School\\D532\\Community Library\\Gutenberg_clean.csv"  # cleaned + renumbered (CSV you will need to update this to your own path)

# Types to remove
remove_types = {"Collection", "Data Set", "Image", "MovingImage", "Sound", "StillImage"}

with open(input_file, "r", newline="", encoding="utf-8") as infile, \
     open(output_file, "w", newline="", encoding="utf-8") as outfile:

    reader = csv.DictReader(infile)
    rows = []

    # Step 1: Filter out non-Text types
    for row in reader:
        if row["Type"] == "Text":
            rows.append(row)

    # Step 2: Sort by original Etext Number
    rows.sort(key=lambda r: int(r["Etext Number"]))

    # Step 3: Renumber sequentially starting at 1
    for i, row in enumerate(rows, start=1):
        row["Etext Number"] = str(i)

    # Step 4: Write final output
    writer = csv.DictWriter(outfile, fieldnames=reader.fieldnames)
    writer.writeheader()
    writer.writerows(rows)

print("Filtering complete and Etext numbers updated to sequential order.")