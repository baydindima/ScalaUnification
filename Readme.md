# Scala simple unification

A solution of a unification problem is a substitution, such that all expressions become equal.

### Grammar
* Expr ::= Var | Fun Args
* Args ::= '(' ')' | '(' Nargs ')'
* Nargs ::= Expr | Expr ',' Nargs
* Var ::= a | b | ... | z | A | B | ... | Z 
* Fun ::= a | b | ... | z | A | B | ... | Z 

### Example:

Input:      
2       
f(a, b, c)      
f(g(b), b, c)         

Output:     
1       
a       
g(b)        


Input:      
3       
f(a, b)     
f(g(a), c)      
f(a, b)

Output:     
None
