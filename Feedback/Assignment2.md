# Assignment 2 - General feedback

Many students missed the second parts of the exercises. Make sure to read the entire exercise and do all the parts.

Many students also missed the HelloLex exercises. Exercises outside the book don't happen that often, but do keep an eye out for them.

The second part of 2.4 requires implementing a function something like `compile : expr -> int list`. Many students probably just did this in the terminal, but please do include the function.

Lastly please try to avoid uploading build files (bin/, obj/ Debug/, \_\_MACOSX, etc.)

## 2.4

Many good and different solutions. After implementing sinstrToInt, assemble can be very concise:

```fs
let assemble = List.collect sinstrToInt
```

## 3.2

Many different regexes work here fx: a?(ba|b)\*
For the DFAs and NFAs, remember that:

- A DFA never has two equal transitions from the same state
- A DFA does not contain empty/epsilon transitions.

Because there is only one unique transition for the current input, it is much easier to check if a DFA accepts a string.

## HelloLex

Most people seemed to successfully get fslex and fsyacc up and running. Do reach out to us if you still have not.
.fsl files almost directly take regex, but you do have to add single quotes around symbols, so hello3.fsl could have the following:

```
['+''-']?(['0'-'9']*['.'])?['0'-'9']+
```
