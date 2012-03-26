from clang.cindex import *
import find_flags
import timeit
import os
import string
import json

index = Index.create()
translation_units = dict()

def format_results(res):
    ret = ""

    valid_completions = dict()

    i = 0
    for result in res.results:
        word = filter(lambda x: x.isKindTypedText(), result.string)

        if word != None and len(word) > 0:
            i = i + 1

            if i > 2000:
                break

            word = word[0].spelling
            returnValue = filter(lambda x: x.isKindResultType(), result.string)
            placeholders = filter(lambda x: x.isKindPlaceHolder(), result.string)
            placeholders = map(lambda x: x.spelling, placeholders)

            parens = filter(lambda x: x.isKindLeftParen() or
                            x.isKindRightParen(), 
                            result.string)

            if not word in valid_completions:
                completion = {"word" : word, "overloads" : []}

                if len(parens) == 0 and word.find(":") == -1:
                    completion["type"] = "variable"
                else:
                    if len(returnValue) > 0:
                        completion["type"] = "function"
                    else:
                        # A function template expansion. Use all symbols in
                        # one long string
                        completion["type"] = "word"
                        completion["word"] = "apa"
                        completion["word"] = ''.join(map(lambda x: x.spelling, result.string))

                        

                valid_completions[word] = completion

            if len(returnValue) > 0:
                overload = {"return_value" : returnValue[0].spelling,
                            "arguments" : placeholders}
                valid_completions[word]["overloads"].append(overload)

    valid_completions = sorted(valid_completions.values(), key=lambda c: c["word"])

    return json.dumps(valid_completions)

def handle_completion(c_file, c_line, c_col, c_content):
    global index
    global translation_units
    
    unsaved_files = [(c_file,c_content)]

    t = timeit.Timer()
    
    tu = None
    flags = TranslationUnit.PrecompiledPreamble | TranslationUnit.CXXPrecompiledPreamble # | TranslationUnit.CacheCompletionResults

    candidate,args = find_flags.flags_for_file(c_file)

    if candidate != None:
        os.chdir(os.path.dirname(candidate))
    
    if c_file in translation_units:
        tu = translation_units[c_file]
        tu.reparse(unsaved_files, flags)        
    else:
        tu = index.parse(c_file, args, unsaved_files, flags)
        translation_units[c_file] = tu
        
    for issue in tu.diagnostics:
        print issue
        
    res = tu.codeComplete(c_file, c_line, c_col, unsaved_files, flags)

    print t.timeit()
    
    return format_results(res)

def handle_completion_str(string):
    """Handles a completion from a raw string. Expected format:
    <filename> \n
    <line> \n
    <column> \n
    <file-contents>"""    

    res = string.find("\n")
    res = string.find("\n", res+1)
    res = string.find("\n", res+1)
    lines = string[:res+1].splitlines()
    return handle_completion(lines[0], int(lines[1]), int(lines[2]), string[res+1:])
    
if __name__ == "__main__":
    import server
