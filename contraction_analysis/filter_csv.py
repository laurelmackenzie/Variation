import csv
import re

from treebankloader import treebank_sw

restrictions = {"WORD": ("is", "has", "will"), "NP": ("NP"), "CONVO":
                set(treebank_sw.fileids())}

in_file = open("contraction.csv", "Ur")
out_file = open("contraction_filtered.csv", "w")
reader = csv.DictReader(in_file)
writer = csv.DictWriter(out_file, reader.fieldnames)
writer.writeheader()

for fields in reader:
    # Clean out letters at end of conversation
    fields['CONVO'] = re.sub(r'[A-Z]', '', fields['CONVO']) + ".mrg"
    matches = [value in restrictions[col] for col, value in fields.items()
               if col in restrictions]
    
    if all(matches):
        writer.writerow(fields)    


