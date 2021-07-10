# titi-lang syntaxes

Variable declaration:

- `let` *id* `=` *type* `;`
- `let` *id* `=` *expr* `;`

Function definition:

- `fn` *id* `(` *params* `)` `{` *body* `}`
- `fn` *id* `(` *params* `)` `=` *type* `{` *body* `}`
- `extern` `fn` *id* `(` *params* `)` `{` *body* `}`
- `extern` `fn` *id* `(` *params* `)` `=` *type* `{` *body* `}`

C function declaration:

- `extern` `fn` *id* `(` *params* `)` `;`
- `extern` `fn` *id* `(` *params* `)` `=` *type* `;`

If statement:

- `if` *expr* `{` *body* `}`
- `if` *expr* `{` *body* `}` `else` *if*
- `if` *expr* `{` *body* `}` `else` `{` *body* `}`

For statement:

- `for` `{` *body* `}`
- `for` *expr* `{` *body* `}`
- `for` *id* `in` *expr* `{` *body* `}`

Switch statement:

- `switch` *expr* `{`

