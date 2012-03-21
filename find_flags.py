import os

def path_to_flags_for_file(source_file):
    cur_dir = os.path.dirname(source_file)
    while cur_dir != "" and cur_dir != "/":
        candidate = os.path.join(cur_dir, ".completion_flags")
        if os.path.exists(candidate):
            return candidate
    
        cur_dir = os.path.split(cur_dir)[0]

    return None

def flags_for_file(source_file):
    lines = []

    path = path_to_flags_for_file(source_file)
    
    if path == None:
        return (path, lines)
    
    for line in open(path, "r").readlines():
        line = line.strip()
        if line != "":
            lines.append(line)

    return (path, lines)
