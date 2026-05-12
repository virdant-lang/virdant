import os
import re

def process_file(path):
    with open(path, 'r') as f:
        lines = f.readlines()

    new_lines = []
    in_keep_block = False
    for line in lines:
        stripped = line.strip()

        # Always strip semicolons at the end of statements
        # The grammar was updated to remove semicolons from typedefs

        # find semicolon that is at the end of the line, ignoring whitespace and // comments
        # a simple way is to use regex:
        match = re.search(r';(\s*(?://.*)?)$', stripped)
        if match:
            idx = stripped.rfind(';', 0, match.start() + 1)
            # wait, if match is found, match.start() is the index of ';'
            # we want to replace it in the original line (which might have leading spaces)
            line_match = re.search(r';(\s*(?://.*)?)$', line)
            if line_match:
                idx = line_match.start()
                line = line[:idx] + line_match.group(1) + '\n'

        new_lines.append(line)

    with open(path, 'w') as f:
        f.writelines(new_lines)

for root, _, files in os.walk('.'):
    if 'w' in root.split(os.sep): # don't touch backup dirs
        continue
    for f in files:
        if f.endswith('.vir'):
            process_file(os.path.join(root, f))
