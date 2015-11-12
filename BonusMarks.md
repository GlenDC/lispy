## Chapter 5: Languages

> Describe a language for decimals (e.g. `12.34`)

  Noun = 0 - 9
  Phrase = Noun+
  Decimal = Phrase or (Phrase and `.` and Phrase)

## Chapter 6 - Parsing

> Write a regular expression matching strings of all a or b such as aababa or bbaa

[ab]+

> Write a regular expression matching strings of consecutive a and b such as ababab or aba

(ab)+(ab?)?

> Write a regular expression matching pit, pot and respite but not peat, spit, or part.

/^(respite|pot|pit)$/

