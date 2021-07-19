By default, an unbroken series of opt modifiers is converted to
'opt','optchain','optchain', so you get `(a[,b[,c])`.

If `convert_opt` is specified, then no such conversion takes place; you then 
must explicitly use `optchain`.

The `@include` tag is only meaningful for project-level items like modules,
scripts, etc. The file is inserted into the document after being formatted.
In this case, you would usually specify `format="markdown"`.
