This assignment consisted of three parts, updates to the Lexer, updates to the Parser, and the Interpreter. 

The updates to the Lexer were relatively straightforward. The challenging part of this was handling operators that were 2 characters long.
To handle this special case, I implemented a function that peeked one character ahead, and determined if that character matched the character
we required; if it didn't, the character was replaced in the input file. With this, when a character like '>' was found, which could be both
'>' or '>=', I simply checked if there was a '=' ahead, and created the correct token based on that.

For the Parser, the approach was generally following the grammars defined. Depending on the specific non-terminal symbol being expanded, 
I checked tokens ahead with peek() to determine which grammar rule to follow. When the grammar consisted of further non-terminal symbols,
I made the corresponding parse call, and the results of that parse could be appended as a child to the current Node.

For the interpreter, the first step was the semantic analyzer. Since the only rule to follow was ensuring that all variables referenced
already existed, this was fairly simple. The analyze() function iterated through all the statements; if it was a variable definition, that
variable name was saved in an unordered_set; if not, then I used a helper function that recusrively searches for all variable references, and 
ensures those identifiers are in the unordered_set. If the program is semantically acceptable, then for execution, I wrote another recursive helper
function that executes a Node based on its type. This function used a switch statement to identify the AST node type, and make the appropriate
recursive calls or return base cases. The return values here were the Value class. Additionally, in execution, variable references had to be saved,
and that was done in the Environment class, which simply used an unordered_map to associate string variable names with integer values.