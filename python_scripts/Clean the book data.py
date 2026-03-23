import csv

input_file = "C:\\Users\\mressex\\Desktop\\School\\D532\\Community Library\\gutenberg_metadata.csv"      # original CSV with other Type values
output_file = "C:\\Users\\mressex\\Desktop\\School\\D532\\Community Library\\Gutenberg_clean.csv"  # the cleaned CSV

# Types you want to remove
remove_types = {"Collection", "Data Set", "Image", "MovingImage", "Sound", "StillImage"}

with open(input_file, "r", newline="", encoding="utf-8") as infile, \
     open(output_file, "w", newline="", encoding="utf-8") as outfile:

    reader = csv.DictReader(infile)
    writer = csv.DictWriter(outfile, fieldnames=reader.fieldnames)

    writer.writeheader()

    for row in reader:
        if row["Type"] == "Text":   # keep only Text
            writer.writerow(row)

print("Filtering complete. Only Text records remain.")

with open(input_file, "r", newline="", encoding="utf-8") as infile, \
     open(output_file, "w", newline="", encoding="utf-8") as outfile:

    reader = csv.DictReader(infile)
    rows = list(reader)

    # Sort by original Etext Number just to ensure correct order
    rows.sort(key=lambda r: int(r["Etext Number"]))

    # Reassign sequential numbers starting at 1
    for i, row in enumerate(rows, start=1):
        row["Etext Number"] = str(i)

    writer = csv.DictWriter(outfile, fieldnames=reader.fieldnames)
    writer.writeheader()
    writer.writerows(rows)

print("Etext numbers updated to sequential order.")
