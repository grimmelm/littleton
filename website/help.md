<!-- title: Littleton Help --> 

<link href="css/bootstrap.min.css" rel="stylesheet">
<link href="css/theme.css" rel="stylesheet">
<link href="css/charter.css" rel="stylesheet">
<link href="css/merriweather-sans.css" rel="stylesheet">
<link href="css/fira_code.css" rel="stylesheet">
<link href="css/awesome.css" rel="stylesheet">
<link href="css/fontawesome.css" rel="stylesheet">

 <nav class="navbar navbar-static-top navbar-expand-lg">
      <div class="container">
        <div class="navbar-header">
          <h1 class="navbar-brand"><a href="/">Littleton Help</a></h1>
        </div>
        <button class="navbar-toggler" type="button"
                data-toggle="collapse" data-target="#nav-links"
                aria-controls="navbarNav" aria-expanded="false" aria-label="Toggle navigation">
          <span class="navbar-toggler-icon"></span>
        </button>
        <div class="collapse navbar-collapse" id="nav-links">
          <ul class="navbar-nav">
            <li class="nav-item">
              <a class="nav-link" href="index.html">Home <span class="sr-only">(current)</span></a>
            </li>
            <li class="nav-item">
              <a class="nav-link" href="interpreter.html">Interpreter</a>
            </li>
            <li class="nav-item active">
              <a class="nav-link" href="help.html">Help</a>
            </li>
          </ul>
        </div>
      </div>
    </nav>

<div id="content"  class="container-fluid" >



Littleton visualizes the results of _conveyances_ like `O conveys to A for life, then to B.` or `Willy Wonka conveys Blackacre to Slughorn, but if Slughorn eats chocolate to Charlie.`

To use, type a sequence of one or more conveyances and events in the input box and click "Interpret". The sequence must start by saying who owns the property initially, e.g. `O owns Blackacre.`

## Types of Conveyances ##

A _conveyance_ consists of the name of the grantor, the words `conveys` or `makes a will`, and one or more grants describing the interests created. _Inter vivos_ transfers take effect immediately; testamentary transfers take effect at the grantor's death, and can be replaced by a subsequent will. A _grant_ such as `to Aethelstan` must contain the word `to`, and a _description_ of the grantee or grantees consisting of one of the following:
* A _name_ consisting of one or more words, each of which must start with capital letter, e.g., `A`, `Agatha`, or `John Doe`. 
* Language identifying a person's spouse (e.g., `the husband of Charles`) or surviving spouse (e.g., `the widow of A`). Littleton will identify the spouse if and when it can; if there is no such person at the time the grant should become possessory, it will fail.
* A list of two or more names linked with `and`, e.g., `John and Paul and Ringo and George`.
* Language identifying a _class_ of a person's undetermined heirs (e.g. `the heirs of Martin Fortisquince`'), children (e.g., `the children of Mary Cavan Tyrone`), or grandchildren (e.g., `the grandchildren of Octavia`).

Grants can optionally contain:
* Language describing how a grant to be shared among multiple named individuals or an unnamed class should be divided. Littleton recognizes equal-share tenancies in common (`in common`), joint tenancies (`jointly`), and tenancies by the entireties (`as husband and husband`). In addition, shares can be divided `per stirpes` or `by representation`, in which case interests to deceased grantees are divided explicitly as instructed.
* Language describing the _quantum_ or natural duration of the interests created, e.g., `to A and their heirs`, `to A for life`, `to Junior for the life of Senior`, `to Tenant for 5 years`, or `to Matilda and the heirs of her body`. (Littleton allows any single-word pronoun, e.g. `his`, `eir`, and `zir`). This language must immediately follow the name of the grantee.
* Language adding a _special limitation_ to the interest, e.g., `to Compeyson until Estella marries` or `to Barney so long as Barney does not drink wine`. Littleton currently recognizes the following conjunctions in creating special limitations: `while`, `until`, `so long as`, `provided that`, and `on condition that`. This language must follow the grantee (and quantum, if any).
* Language adding a _condition precedent_: e.g., `if Akira is unmarried to Akira`. This language must precede the grantee.
A clause can contain none, some, or all of these, e.g. `if Biisuke is unmarried to Biita for life so long as Biigoro does not consume chocolate` contains all three. Conditions precedent can be introduced using different langauge, which controls how long they will wait for the condition to become true: `if` (no waiting), `if and when` (until the condiiton is true or impossible), or `after` (forever).

Grants can be linked with the words `, then` or `, but if`. (The comma is optional but recommended for clarity.) On the one hand, `then` expressees straightforward succession, as in `to A for life, then to B`. On the other, `but if` is used to create executory interests that can divest preceding interests, as in `to A, but if the property is used as a school to B`. The condition in a `but if` clause is mandatory.

Parentheses can be placed around any grant or sequence of grants and are useful to resolve ambiguity in the scope of an executory limitation, e.g., `(to A for life, then to B), but if C marries to C` is different from `to A for life, then (to B, but if C marries to C)`.

## Conditions ##

Littleton recognizes a limited but flexible set of conditions. Any condition on this list can be used as part of a special limitation (`while x` or `until x`), condition precedent (`if x`), or executory limitation (`but if x`):

* `A is dead`.
* `A is alive`
* `A survives B`
* `A and B marry` (present tense, true at the moment they marry), `A and B are married` (state of affairs, true as long as they are married), or `A and B have married` (perfect tense, true if they have ever been married). Any of these can also be written without the second spouse, e.g. `A marries`, in which case it will be true if A marries anyone.
* `A and B divorce` (present tense) or `A and B have divorced` (perfect tense)
* `the property is used as a school`. The use can be any single word, e.g. `school`, `factory`, or `farm`. 
* `A graduates law school`. Littleton also recognizes any single-word institution, e.g., `college` or `Brannigan`.
* `A drinks alcohol`. The subtance can be any single word, e.g., `alcohol`, `drugs`, or `chocolate`. In addition, `consumes` and `eats` can be used as synonyms for `consumes`. 
* `Mars becomes a state`. Any single other single word can be used as the name of the jurisdiction
* "Generic" conditions, enclosed in parentheses, such as `(the Moon explodes)` or `(wild badger moles overrun the property)`.  Generic conditions are treated as black boxes, and Littleton makes no attempt to understand their contents.

Most conditions can be negated with `not`, e.g., `A and B are not married` or `A does not graduate`, or put in the perfeect tense, e.g., `A has drunk alcohol`.

Littleton does its best to update the state of title based on its knowledge of conditions.  At the simplest level, this involves terminating posseessory interests when they expire: A's death will cause the life estate created by `to A for life, then to B` to terminate and make B's remainder possessory. Littleton also does its best to update the state of title to reflect possibilities that must eventually or cannot happen.  So, for example, B's death will eliminate B's remainder from `to A for life, then to B for life, then to C` because it can never become possessory. Littleton will make this simplification even if takes place before A's death.

## Events ##

Littleton can calculate the consequences of _events_ like `A dies.` Events and conveyances can be freely interwoven. Put each event on its own line.

```
A and B marry.
O conveys to A while A is married, then to C.
B and A divorce.
```
Events currently recognized by Littleton include:
```
Xavier dies.
Yolanda and Zelda marry.
Yolanda and Zelda divorce.
Winnifred reenters.
5 years pass.
the property is used as a school.
Ahmed graduates.
Bethanny has no living descendants.
```
The passage of time can be any positive integer, e.g. `2 years pass` or `19 years pass`. 

Littleton is capable of limited reasoning about the consequences of events, and will ignore events that are physically or legally impossible. For example it knows that dead people cannot consume alcohol, and that married people cannot marry someone else without first divorcing or becoming widow(er)ed. 

Littleton has a basic model of intestacy. If a person dies married, their heir is their spouse; otherwise Littleton will refer to "unknown" heirs to emphasize that they exist but have not been specified.

Littleton also recognizes _generic_ events and conditions enclosed in parentheses, which it treats as black boxes that match corresponding generic conditions. For example, the event `(the Moon explodes).` triggers the condition in `to A, but if (the Moon explodes) to B` because what is betwen the parentheses matches exactly.


## Multiple Conveyances ##

Conveyances can be combined, e.g.:
```
O conveys to A for life.
A conveys to C.
O conveys to D while D is married, then to E.
```
If the grantor is omitted from a conveyance, as in `To A for life, then to B.`, Litteton assumes that the grantor is `O`. Conveyances can optionally contain the name of a property, as in `O conveys Blackacre to P.` but the name is ignored.

## Naming, Vesting, and the Rule Against Perpetuities ##

Littleton does its best to name interests according to the system described in the Restatement (First) of Property. It computes a vesting status for every interest, but displays it only for remainders. It also applies the Rule Against Perpetuities to remainders and executory interests, doing its best with its limited knowledge of what is and is not possible in the real world.  For example, it knows that the condition precedent `if A consumes alcohol` must be satisfied or permanently fail within the lifetime of A, but that `until the property is used as a farm` need not be satisfied within any life in being plus 21 years.

## Miscellaneous Doctrines ##

By default, Littleton applies the Doctrine of Worthier Title, the Rule in Shelley's Case, and merger. It does not apply the destructibility of contingent remainders. When no quantum is specified, Littleton follows the modern default of assuming a fee simple. When no sharing is specified, Littleton follows the modern default of assuming a tenancy in common. The life tenant can bar an entail by conveying a fee simple. Testamentary grants to deceased takers are subject to lapse; _inter vivos_ grants to deceased takers are treated as grants to their generic unknown heirs.

## About Littleton ##

Paper coming soon!

</div>