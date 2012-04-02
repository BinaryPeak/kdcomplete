from clang.cindex import *
import find_flags
import timeit
import os
import string
import json

index = Index.create()
translation_units = dict()
# [ {"delay", "(NSTimeInterval)"}, {animateDelay, "(NSTimeInterval)"}]
# "delay:(NSTimeInterval)"
# void doSomething(/* the width of something*/ int x )
# .cpp:  void do(int x) 
def format_result(result, typed_texts, valid_completions):
    word = typed_texts[0].spelling

    placeholders = []
    returnValue = None

    for item in result.string:
        if item.isKindResultType():
            returnValue = item.spelling
        elif item.isKindPlaceHolder():
            placeholders.append(item.spelling)
    
    parens = filter(lambda x: x.isKindLeftParen() or
                    x.isKindRightParen(), 
                    result.string)

    if word == "animateWithDuration:":
        print word, result.string


    if not word in valid_completions:
        completion = {"word" : word, "overloads" : []}

        if len(parens) == 0 and word.find(":") == -1:
            completion["type"] = "variable"
        else:
            if returnValue is not None:
                completion["type"] = "function-call"

                if word.find(":") != -1:
                    # Objc- should prefix all argument types with argument
                    # names after first one
                    
                    for i in xrange(1,len(placeholders)):
                        placeholders[i] = "%s%s" % (typed_texts[i].spelling, placeholders[i])

                    print word, "function-call"
            else:
                # A function template expansion. Use all symbols in
                # one long string
                print word, "function-definition"
                completion["type"] = "function-definition"
                completion["word"] = ''.join(map(lambda x: x.spelling, result.string))

        valid_completions[word] = completion

    if returnValue is not None:
        # print word, "is a function definition, overloads", placeholders
        # print result.string
        overload = {"return_value" : returnValue,
                    "arguments" : placeholders}
        valid_completions[word]["overloads"].append(overload)

def format_results(res):
    ret = ""

    valid_completions = dict()

    i = 0
    for result in res.results:
        typed_texts = filter(lambda x: x.isKindTypedText(), result.string)

        if typed_texts != None and len(typed_texts) > 0:
            i = i + 1

            if i > 2000:
                break

            format_result(result, typed_texts, valid_completions)

    valid_completions = sorted(valid_completions.values(), key=lambda c: c["word"])

    return json.dumps(valid_completions, indent = True)

def handle_completion(c_file, c_line, c_col, c_content):
    global index
    global translation_units
    
    unsaved_files = [(c_file,c_content)]

    t = timeit.Timer()
    
    tu = None
    flags = TranslationUnit.PrecompiledPreamble | TranslationUnit.CXXPrecompiledPreamble # | TranslationUnit.CacheCompletionResults

    candidate,args = find_flags.flags_for_file(c_file)

    if candidate != None:
        print args
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
