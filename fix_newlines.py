import os
import re

def process_file(path):
    with open(path, 'r') as f:
        content = f.read()

    # We want to remove extra newlines inside blocks like:
    # union type Name { ... }
    # enum type Name width N { ... }
    # struct type Name { ... }
    # socket Name { ... }

    def replacer(match):
        block_content = match.group(2)
        # Replace \n\n with \n inside the block
        # We can just replace multiple newlines with a single newline
        # but let's be careful not to ruin intentional blank lines if there were any.
        # However, the user said "Fix that and any other places you did that kind of thing."
        # The extra newlines were added exactly where a semicolon was removed.
        # So we can replace `\n\n([ \t]*)` with `\n\1`
        # Actually, replacing all `\n\n` with `\n` inside these blocks is totally fine.
        fixed_content = re.sub(r'\n{2,}', '\n', block_content)
        # also if there's an extra newline before the closing brace
        fixed_content = re.sub(r'\n}$', '\n}', fixed_content)
        return match.group(1) + fixed_content + match.group(3)

    pattern = r'((?:union|enum|struct|socket)\b[^{]*\{)(.*?)(\n\})'
    new_content = re.sub(pattern, replacer, content, flags=re.DOTALL)
    
    # Also if the closing brace has multiple newlines before it
    new_content = re.sub(r'\n\n\}', '\n}', new_content)

    if new_content != content:
        with open(path, 'w') as f:
            f.write(new_content)
        print(f"Fixed {path}")

for root, _, files in os.walk('.'):
    if '.venv' in root or 'build' in root or 'target' in root:
        continue
    for f in files:
        if f.endswith('.vir'):
            process_file(os.path.join(root, f))
