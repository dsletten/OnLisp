# Tables in Common Lisp

Beyond simple sequential collections of data such as lists or arrays, many programming languages
provide a type of collection that allows pairing values with arbitrary keys. These collections
are known by various names such as dictionaries, as a dictionary associates definitions with
words; tables, as a table pairs the fields of a row with a primary key; or in general, maps,
which pair elements from one set with those of another. Common Lisp has three built-in table
data types, and one comes in two flavors. So there are four choices to consider.

1. Association lists (alists or a-lists for short)  
   An association list is a linked-list based table of arbitrary keys and values.
   Elements of this list are key/value pair sublists:  
   `'((john . guitar) (paul . bass) (george . guitar) (ringo . drums))`

2. General property lists (plists or p-lists)  
   A property list is another linked-list data structure, but the elements of such a list
   are the alternating keys and values themselves as though an association list has had its
   nested structure flattened:  
   `'(john guitar paul bass george guitar ringo drums)`

    Plists have slightly different semantics than alists. One distinction is that, while
    alists may employ any object as a key, only symbols should serve as keys for a plist.
    This is due to the mechanism to look up keys on the property list.

3. Symbol property lists  
   Every Common Lisp symbol is a substantial object in memory that has several values
   potentially associated with it. One of the values tied to each symbol is a property list
   as described in 2. above. All of the operations on property lists are available to symbol
   property lists. However, they should be used with some caution as they are globally visible
   (within their package -- there is no point in putting a property list on an uninterned
   symbol). Consequently they carry the risk of conflicts in modification as do all global
   mutable values.

4. Hashtables  
   Finally, Common Lisp provides built-in hashtable support. Hashtables furnish efficient
   retrieval and update for large tables with many entries. While it's true that Common Lisp
   does not define a convenient literal syntax for hashtables as some languages do (e.g., Ruby:
   `{:a => 1, :b => 2, :c => 3}`), this is a somewhat spurious complaint for three reasons.

    First, while a hashtable implementation of a dictionary is warranted for a large number of
    key/value pairs, an association list is actually a better choice for a smaller dictionary.
    And alists have an even simpler literal representation: `((:a 1) (:b 2) (:c 3))`. Moreover,
    a dictionary large enough to justify a hashtable would not be initialized by a literal value
    embedded in source code. The values would come from a file or database. This is the same in
    Common Lisp as with any other language.

    Second, if absolutely necessary, it is simple to define a reader macro to add new syntax to
    Common Lisp to allow hashtable literals. One could adopt the syntax of Clojure maps, for
    example: `{"pung" 1 "foo" 2 "bar" 3}`
    (This still does not address the _printable_ representation as opposed to this _readable_
    representation.)

    A third weakness in the complaint about literal syntax is that such syntax is less flexible.
    A Ruby hash literal cannot specify which equality test the table should use. Likewise, none
    of the other configuration details to an explicit call to [MAKE-HASH-TABLE](https://www.lispworks.com/documentation/HyperSpec/Body/f_mk_has.htm) (e.g.,
    **:REHASH-SIZE**, **:REHASH-THRESHOLD**) are available in Ruby.

Four main operations are possible with a dictionary/table:

1. Add a new entry to the table.
2. Retrieve the value associated with a key (or an entire key/value entry).
3. Update an entry, associating a new value with an existing key.
4. Remove an entry from the table. The given key no longer has a value associated with it.

It's not surprising that these operations correlate to the fundamental actions on a table in a
relational database  
[(CRUD)](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete): Create, Read, Update, Delete  
or  
(CRAP): Create, Replicate, Append, Process  
(DAVE): Delete, Add, View, Edit  

Another similar set of actions supports RESTful API's (HTTP verbs).
[Your mileage may vary](https://softwareengineering.stackexchange.com/questions/120716/difference-between-rest-and-crud]) on _how_ similar they are.

Here is a comparison of similar ideas from SQL/REST and the Lisp table datatypes discussed
below:
        
|        |SQL      |REST       |Alist           |Plist         |Hashtable     |
| ------- | --------- | ----------- | ---------------- | -------------- | -------------- |
|CREATE |insert   |POST (PUT) |ACONS           |(SETF GET(F)) |(SETF GETHASH)|
|READ   |select   |GET        |ASSOC           |GET(F)        |GETHASH       |
|UPDATE |update   |PUT        |SETF+ASSOC<br>(Shadow ACONS)      |(SETF GET(F)) |(SETF GETHASH)|
|DELETE |delete   |DELETE     |REMOVE          |REMF/REMPROP  |REMHASH       |



## Association Lists

CLHS §14.1.2.1 ["Lists as Association Lists"](https://www.lispworks.com/documentation/HyperSpec/Body/14_aba.htm) defines an alist:
> An association list is a list of conses representing an association  
> of keys with values, where the car of each cons is the key  
> and the cdr is the value associated with that key.  

An association list in common usage can be a bit broader than this orthodox description. It
is a list of entries of the form `(<KEY> . <VALUE>)` or perhaps `(<KEY> <VALUE>)`. In other words,
each top-level element of the alist is either a CONS whose CAR is the key and whose CDR is the
value (dotted pairs), or it is a two-element list whose FIRST is the key and whose SECOND is
the value:  
`((apple . red) (banana . yellow) (orange . orange))`  
vs.  
`((apple red) (banana yellow) (orange orange))`

These distinctions may be blurred in the case where the value of the entry is itself a list.
Consider an entry relating BOB with a list of his children, namely his sole daughter, MARY.
This would be represented as `(BOB . (MARY))`. However, this dotted pair would ordinarily be
printed as `(BOB MARY)`. In this case, the CDR of the entry would be considered the value: `(MARY)`,
rather than the SECOND: MARY.

Suppose we have the following table:

|  |  |
| --- | --- |
| a | 1 |
| b | 2 |
| c | 3 |

A literal list could encode this table as an alist:  
`((a . 1) (b . 2) (c . 3))`

```
[*|*]------->[*|*]------->[*|*]--->NIL
 |            |            |          
 v            v            v          
[*|*]--->1   [*|*]--->2   [*|*]--->3  
 |            |            |          
 v            v            v          
 A            B            C          
```

Alternatively:  
`((a 1) (b 2) (c 3))`

```
[*|*]------------------>[*|*]------------------>[*|*]--->NIL         
 |                       |                       |                   
 v                       v                       v                   
[*|*]--->[*|*]--->NIL   [*|*]--->[*|*]--->NIL   [*|*]--->[*|*]--->NIL
 |        |              |        |              |        |          
 v        v              v        v              v        v          
 A        1              B        2              C        3
```

The first version requires less memory (fewer CONS cells). The second is perhaps easier to read.

Of course, the order of the entries is irrelevant. It is the specific ordered key/value pairs
that define the table. This alist is equivalent to the first above: `((b . 2) (a . 1) (c . 3))`

(This does become an issue later when we consider shadowing.)

Rather than encoding the alist literally, we can use the function [PAIRLIS](https://www.lispworks.com/documentation/HyperSpec/Body/f_pairli.htm) to establish the
correspondence between a list of keys and a list of values:  
`(pairlis '(a b c) '(1 2 3)) => ((C . 3) (B . 2) (A . 1))`

Note that PAIRLIS builds alists of dotted pairs: `(<KEY> . <VALUE>)`. If you _really_ have to
have entries that are proper lists instead:  
`(pairlis '(a b c) (mapcar #'list '(1 2 3))) => ((C 3) (B 2) (A 1))`

On the other hand, the alist can be built incrementally by adding individual entries. The
function [ACONS](https://www.lispworks.com/documentation/HyperSpec/Body/f_acons.htm) takes an alist (possibly empty) and conses a new entry at the head. As always,
CONSing yields a new structure. The original is not modified.

From scratch:  
`(acons 'c 3 (acons 'b 2 (acons 'a 1 '()))) => ((C . 3) (B . 2) (A . 1))`

Add to an existing alist:  
`(defvar *table1* (pairlis '(a b c) '(1 2 3)))`  
`(acons 'd 4 *table1*) => ((D . 4) (C . 3) (B . 2) (A . 1))`

The value of a key is retrieved by the function [ASSOC](https://www.lispworks.com/documentation/HyperSpec/Body/f_assocc.htm), which actually returns the entire entry
for the key:  
`(assoc 'b *table1*) => (B . 2)`  
The function CDR/REST (or SECOND) would then be used to extract the value from the entry as
appropriate.

CLHS illustrates the relationship between ASSOC and the general-purpose sequence function
FIND:  
`(assoc <ITEM> <ALIST> :test <FN>) ≡ (find <ITEM> <ALIST> :test <FN> :key #'car)`

This is true except in the case where `<ITEM>` is NIL.

The default equality test for ASSOC to match a key to `<ITEM>` is EQL. Another function can be
specified by the :TEST keyword, e.g., STRING-EQUAL for case-insensitive string keys.

There are two different ways to think about updating an entry (associating a new value with a
given key). Because the lookup mechanism for ASSOC proceeds from the head of the alist and stops
as soon as it locates an entry with a matching key, we can shadow an existing entry simply by
adding a new entry with the same key to the head of the alist:  
`(assoc 'c *table1*) => (C . 3)`  
`(acons 'c 99 *table1*) => ((C . 99) (C . 3) (B . 2) (A . 1))`  
`(assoc 'c (acons 'c 99 *table1*)) => (C . 99)`

This way of updating the alist allows a temporary change without destructively modifying the
alist that can easily be restored in a different context. For example, a function may CONS new
entries onto an alist while its caller maintains a pointer to the original alist. It would be
restored to this original state when the function returns:
```
(defun foo (key alist)
  (format t "Enter FOO: ~A => ~S~%" key (assoc key alist))
  (bar key alist)
  (format t "Leave FOO: ~A => ~S~%" key (assoc key alist)))

(defun bar (key alist)
  (format t "In BAR: ~A => ~S~%" key (assoc key (acons key :bar alist))))

(foo 'a *table1*)
Enter FOO: A => (A . 1)
In BAR: A => (A . :BAR)
Leave FOO: A => (A . 1)
```

Shadowing does stretch the metaphor of a mapping considering that there are multiple ordered
pairs with the same key. But only one pair is visible at any time.

An alternative way to shadow would be to PUSH an entry directly onto the front of the alist:
```
(defun bar (key alist)
  ...
  (push (cons key new-value) alist)
  ...)
```
In fact, one way to circumvent shadowing would be to use [PUSHNEW](https://www.lispworks.com/documentation/HyperSpec/Body/m_pshnew.htm) to detect an existing key:  
`(pushnew (cons k v) alist :key #'first)` ; Possibly :TEST too

`(push (cons 'b 12) *table1*) => ((B . 12) (C . 3) (B . 2) (A . 1))`  
`(pushnew (cons 'b 12) *table1* :key #'first) => ((C . 3) (B . 2) (A . 1))`

In the second case, PUSHNEW declines to add a new entry since an entry for B was already
present.

In other cases, we may actually want a permanent update to the table. Using SETF along with
ASSOC will destructively modify the alist entry replacing the old value with the new. Since the
alist retains a reference to the entry, the alist is indirectly changed as well.

However, we cannot use (SETF ASSOC) directly:  
`(setf (assoc 'a *table1*) 5) =>
  The function (SETF ASSOC) is undefined.`

We must choose the correct value accessor (CDR/SECOND) depending on the type of the alist:  
`(setf (cdr (assoc 'a *table1*)) 5)`  
`*table1* => ((C . 3) (B . 2) (A . 5))`

CLHS also mentions using the old school [RPLACD](https://www.lispworks.com/documentation/HyperSpec/Body/f_rplaca.htm) function:  
`(rplacd (assoc 'b *table1*) 7)`  
`*table1* => ((C . 3) (B . 7) (A . 5))`

Alternatively, [NSUBSTITUTE](https://www.lispworks.com/documentation/HyperSpec/Body/f_sbs_s.htm) (the destructive version of SUBSTITUTE) could be used:  
`(nsubstitute (cons 'b 12) 'b *table1* :key #'car)`  
`*table1* => ((C . 3) (B . 12) (A . 5))`

For that matter, it is possible to nondestructively update an alist with [SUBSTITUTE](https://www.lispworks.com/documentation/HyperSpec/Body/f_sbs_s.htm) as an
alternative to shadowing:  
`(substitute (cons 'b 18) 'b *table1* :key #'car) => ((C . 3) (B . 18) (A . 5))`  
`*table1* => ((C . 3) (B . 12) (A . 5))`

However, these non-shadowing approaches only work when truly _updating_ an entry. The entry
must already exist for the key (i.e., the result of calling ASSOC is not NIL). There has to be
something there for SETF (or (N)SUBSTITUTE) to replace. This will fail if an attempt is made
to add a new key. (Association lists are unique among the table variants in that they have
different mechanisms for adding/updating entries.)

Shadowing works whether or not the table already contains an entry for the key. Obviously no
_shadowing_ actually takes place for a new key.

Finally, an entry can be removed (or destructively deleted):  
`(remove 'b *table1* :key #'car) => ((C . 3) (A . 5))`  
`*table1* => ((C . 3) (B . 12) (A . 5))`

Be aware that this will remove all matching entries, undoing any shadowing:  
`(acons 'b 19 *table1*) => ((B . 19) (C . 3) (B . 12) (A . 5))`  
`(remove 'b (acons 'b 19 *table1*) :key #'car) => ((C . 3) (A . 5))`

This can be controlled either by passing a :COUNT argument to REMOVE:  
`(remove 'b (acons 'b 19 *table1*) :key #'car :count 1) => ((C . 3) (B . 12) (A . 5))`  
(This effectively "unshadows" the key B)

or by removing more selectively:
```
(let ((table (acons 'b 19 *table1*)))
  (remove (assoc 'b table) table))
((C . 3) (B . 12) (A . 5))
```

Here, ASSOC explicitly locates the first entry for removal.

Destructive DELETE:  
`(delete 'b *table1* :key #'car) => ((C . 3) (A . 5))`  
`*table1* => ((C . 3) (A . 5))`

Common Lisp also provides the functions [ASSOC-IF/ASSOC-IF-NOT](https://www.lispworks.com/documentation/HyperSpec/Body/f_assocc.htm) to tailor how an entry is
retrieved or modified.

In addition to the basic table operations, an association list can also be treated as an
invertible function, mapping from the values to the keys. Entries can be looked up using the
corresponding [RASSOC/RASSOC-IF/RASSOC-IF-NOT](https://www.lispworks.com/documentation/HyperSpec/Body/f_rassoc.htm) functions:  
`(defvar *table2* '((apple . red) (banana . yellow) (orange . orange)))`  
`(rassoc 'red *table2*) => (APPLE . RED)`  
`(rassoc 'yellow *table2*) => (BANANA . YELLOW)`  
`(rassoc 'orange *table2*) => (ORANGE . ORANGE)`

However, despite the implication of a _reverse_ association, these functions still search from
the head of the alist--the ordered pairs are considered to be reversed. Thus, the first entry
whose _value_ matches is returned:  
`(rassoc 1 '((d . 1) (c . 1) (b . 2) (a . 1))) => (D . 1)`

This may produce unexpected results for a function such as this that is _not_ _actually_
_invertible_ (since it is not injective).

And, as expected, the call needs to be adapted for an alist with entries that are proper lists.
Since RASSOC is already examining the CDR of each entry, the :KEY needs to be #'CAR in order to
access the second element (CADR) of each entry:  
`(rassoc 1 '((d 1) (c 1) (b 2) (a 1)) :key #'car) => (D 1)`

As with ASSOC, CLHS highlights this relationship between RASSOC and FIND:  
`(rassoc <ITEM> <ALIST> :test <FN>) ≡ (find <ITEM> <ALIST> :test <FN> :key #'cdr)`

There is also a function [COPY-ALIST](https://www.lispworks.com/documentation/HyperSpec/Body/f_cp_ali.htm) to copy the deeper structure of an alist (vs. shallow
copy: COPY-LIST).

Example:
```
(let* ((a (pairlis '(a b c) '(1 2 3)))
       (b (copy-list a))
       (c (copy-alist a)))
...)
```

```
   Alist A: [*|*]------->[*|*]------->[*|*]--->NIL
              |            |            |          
              v            v            v          
           +>[*|*]--->3 +>[*|*]--->2 +>[*|*]--->1  
           |  |         |  |         |  |          
           |  v         |  v         |  v          
           |  C         |  B         |  A          
           |            |            |             
Alist B: [*|*]------->[*|*]------->[*|*]--->NIL   
```


```
Alist C: [*|*]------->[*|*]------->[*|*]--->NIL
          |            |            |          
          v            v            v          
         [*|*]--->3   [*|*]--->2  [*|*]--->1   
          |            |            |          
          v            v            v          
          C            B            A          
```

Finally, David Touretzky's book [Common LISP: A Gentle Introduction to Symbolic Computation](
)
(pg. 337) addresses renaming a key in an alist:
```
(defun rename-key (new old table)
  (setf (car (assoc old table)) new))
```
This can also be accomplished non-destructively:
```
(defun rename-key (new old table)
  (let ((entry (assoc old table)))
    (acons new (cdr entry) (remove entry table))))
```
And on pg. 338 he discusses adding to an existing value as opposed to replacing it:
```
(defun add (obj prop table)
  (nconc (assoc obj table) (list prop)))
```
or
```
(defun add (key prop table)
  (push prop (rest (assoc key table))))
```


## Property Lists

A property list (plist) is like an alist without any nested structure. The key/value pairs are
simply adjacent elements of the top-level plist structure:  
`(<K1> <V1> <K2> <V2> <K3> <V3>)`  
rather than  
`((<K1> <V1>) (<K2> <V2>) (<K3> <V3>))`  
Consequently, the plist should contain an even number of elements, and there is no dotted list
variant as with alists.

In typical Lisp usage, a _property_ _indicator_ and a _property_ _value_ jointly establish a
[property](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#property). (Sometimes the word "property" simply refers to the property value.)

There is no analogous PAIRLIS function to initialize a plist, but it is easy enough to write
one:
```
(defun make-plist (keys vals)
  (loop for key in keys
        for val in vals
        collect key
        collect val))

(make-plist '(a b c) '(1 2 3)) => (A 1 B 2 C 3)
```

However, plists are typically built incrementally rather than starting with a predefined set of
properties.

One important issue with property lists is that the built-in retrieval/update functions are
hard-wired to use EQ as the test to locate keys. Not only does this preclude the use of strings
as keys, it also makes the use of characters and numbers unreliable too (implementation-
dependent). Consequently, plists normally only use symbols (which includes keywords) as keys.

We can use the function [GETF](https://www.lispworks.com/documentation/HyperSpec/Body/f_getf.htm) to look up the value for a given key. But GETF differs from ASSOC
in three ways:

1. Parameter order. The plist comes first then the indicator
   `(getf plist 'a)` vs. `(assoc 'a alist)`
2. GETF returns merely the value (if found) rather than the entry for the key.
3. GETF can supply a default value to be used if no entry is found.

```
(defvar *p1* (list :a 1 :b 2 :c 3))

(getf *p1* :c) => 3
(getf *p1* :d) => NIL
(getf *p1* :d -1) => -1
```

However, just like ASSOC, GETF returns the value of the first matching key that it encounters.
So it is possible to shadow a key as with alists:
```
(defvar *p2* (list* :c 8 *p1*))
*p1* => (:A 1 :B 2 :C 3)
*p2* => (:C 8 :A 1 :B 2 :C 3)

(getf *p2* :c) => 8
(getf *p1* :c) => 3
```

([LIST*](https://www.lispworks.com/documentation/HyperSpec/Body/f_list_.htm) is not specifically a property list function, but it can serve as ACONS does for alists.)

GETF is, in fact, an accessor function. This means that it can also be used to (destructively)
update the value for a key via SETF:
```
(setf (getf *p1* :c) 4)

*p1* => (:A 1 :B 2 :C 4)
(getf *p1* :c) => 4
```

Furthermore, either of these techniques we've just examined will add a new entry to the plist:
```
(setf *p1* (list* :f 9 *p1*)) => (:F 9 :A 1 :B 2 :C 4) ; Shadow
(setf (getf *p1* :e) -6) ; Destructively modify
*p1* => (:E -6 :F 9 :A 1 :B 2 :C 4)
```

There is thus no discrepancy between adding and updating as with alists.

An entry can be removed using [REMF](https://www.lispworks.com/documentation/HyperSpec/Body/m_remf.htm), which destructively modifies the property list:
```
(remf *p1* :b)
*p1* => (:E -6 :F 9 :A 1 :C 4)
```

There is an additional function [GET-PROPERTIES](https://www.lispworks.com/documentation/HyperSpec/Body/f_get_pr.htm) that will return the value of the first key found of a group of specified indicators:
`(get-properties *p1* '(:a :f :c)) => :F; 9; (:F 9 :A 1 :C 4)`  

No analog to RASSOC exists for plists, but it could be defined:
```
(defun rgetf (plist val)
  (getf (reverse plist) val))
```

## Symbol Property Lists

The most common use of property lists (historically) was the plists that are automatically
associated with symbols. A [symbol](https://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm) object is a complex structure that has multiple attributes ("cells") associated with
it when it is instantiated. One of these is the symbol's property list, which is initially
empty. It can be retrieved by means of the function [SYMBOL-PLIST](https://www.lispworks.com/documentation/HyperSpec/Body/f_symb_4.htm):
```
* 'glurp ; Interned by reader
GLURP
* (symbol-plist 'glurp) ; Initially empty
NIL
```

Just like the general plists above, a symbol's property list is simply a list:  
`(listp (symbol-plist 'pung)) => T`
and it can be abused, for example, storing an odd number of elements:
```
(push 'foo (symbol-plist 'pung))
(symbol-plist 'pung) => (FOO)
```
However, the plist should normally hold an even number of elements (key/value pairs) as with
any property list.

A symbol's entire property list can be assigned by use of SETF with SYMBOL-PLIST:
`(setf (symbol-plist 'pung) (list :foo 1 :bar 9))`  
But CLHS discourages this:   
> The use of _setf_ should be avoided, since a symbol's property list is a global resource  
> that can contain information established and depended upon by unrelated programs in
> the same Lisp image.  

A symbol's plist could be manipulated by means of the property list functions already introduced (GETF, REMF): `(getf (symbol-plist 'pung) 'foo)`. But Common Lisp provides a second group of
functions specifically for use with symbol plists.

To retrieve a property use the function [GET](https://www.lispworks.com/documentation/HyperSpec/Body/f_get.htm):  
`(get <SYMBOL> <INDICATOR>)`

GET also takes an optional _default_ _value_ as GETF does.

CLHS says:  
`(get <SYMBOL> <INDICATOR>) ≡ (getf (symbol-plist <SYMBOL>) <INDICATOR>)`

To add or update a property:
`(setf (get <SYMBOL> <INDICATOR>) <NEW-VAL>)`

And a property can be removed with the function [REMPROP](https://www.lispworks.com/documentation/HyperSpec/Body/f_rempro.htm):  
`(remprop <SYMBOL> <INDICATOR>)`

If a property is being shadowed, REMPROP will only remove the first matching key/value from the
plist. The function returns _false_ if no matching property exists.
```
(setf (get 'baz :a) 1)
(setf (get 'baz :b) 2)
(setf (get 'baz :c) 3)

(symbol-plist 'baz) => (:C 3 :B 2 :A 1)

(remprop 'baz :b) => (:B 2 :A 1)
(symbol-plist 'baz) => (:C 3 :A 1)

(remprop 'baz :d) => NIL
```

CLHS says:  
`(remprop <SYMBOL> <INDICATOR>) ≡ (remf (symbol-plist <SYMBOL>) <INDICATOR>)`


;;;
;;;    GETF:REMF::GET:REMPROP
;;;    (GET:GETF::REMPROP:REMF) ???
;;;



