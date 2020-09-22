---
title        : One λ-calculus, many times...
date         : 2016-03-20 12:00:00
tags         : [programming language theory, agda]
extra-script : [agda-extra-script.html]
---

<!--
```agda
module 2016-03-20-one-lambda-calculus-many-times where
```
-->

Previously, I mentioned that one of the most common posts on Agda blogs
is implementing the simply-typed λ-calculus. [Gergő Érdi][noshortcuts]
even goes as far as to call it the FizzBuzz of dependently-typed
programming, and rightfully so: If you do a quick search, you'll find
dozens of examples.

<!--more-->

In *[Dependently-Typed Programming with Agda][agdatutorial]*, Ulf Norell
implements a type checker the simply-typed λ-calculus;
[Francesco Mazzoli][byexample] more or less follows Ulf, but extends
his λ-calculus with a primitive operator for addition; and,
[Gergő Érdi][noshortcuts] extends Ulf's approach with a checker for
scope and binding.

I figured it would be more fun if, instead of rewriting the type
checker example, I would do something a little bit different. So for
my λ-calculus post, I'll have a look at kinds of different ways of
implementing the simply-typed λ-calculus. Today, natural deduction and
the sequent calculus.


### Natural Deduction and the λ-Calculus

We'll start our discussion with the syntax of types. Usually, types
are defined inductively over some set of atomic types. We don't really
care what these atomic types will be, so we might as well abstract
over them:

```
module Syntax (Atom : Set) where
```

But, if it makes you feel better, we can pretend that they'll be some
like this:

<pre class="Agda Spec">  <a name="511" class="Keyword">data</a><a name="515"> </a><a name="516" href="#289" class="Module">Atom</a><a name="521"> </a><a name="522" class="Symbol">:</a><a name="523"> </a><a name="524" class="PrimitiveType">Set</a><a name="527"> </a><a name="528" class="Keyword">where</a><a name="533"><br />    </a><a name="538" href="#538" class="InductiveConstructor">Int</a><a name="542">    </a><a name="543" class="Symbol">:</a><a name="544"> </a><a name="545" href="#516" class="Datatype">Atom</a><a name="550"><br />    </a><a name="555" href="#555" class="InductiveConstructor">String</a><a name="562"> </a><a name="563" class="Symbol">:</a><a name="564"> </a><a name="565" href="#516" class="Datatype">Atom</a></pre>

Next, we defined our types. Since we're talking about minimal
propositional logic, a type is either atomic (marked by <a class="Agda
InductiveConstructor">El</a>) or an implication:

```
  infixr 6 _⇒_

  data Type : Set where
    El  : Atom → Type
    _⇒_ : Type → Type → Type
```

Now we'll define sequents. Even though this is just a tiny piece of
syntax, we should put some thought behind it...

Traditionally, the antecedent of some sequent would be a *set* of
formulas. However, we're looking at this from the perspective of
λ-calculus, and there may well be a difference between two terms of
the same type. This is usually solved by changing the antecedent to a
set of *type assignments*, which means $$x : A$$ and $$y : A$$ are now
distinct. From the logical perspective, this is the same as using a
*bag* or *multiset* antecedent. If we were doing mathematics, we'd be
done, but implementation-wise a bag is actually a rather complex
beast. For this reason, we'll use a *list*:[^imports]

<div class="hidden">
```
  open import Data.Nat             using (ℕ; suc; zero)
  open import Data.Fin             using (Fin; suc; zero)
  open import Data.List            using (List; _∷_; []; _++_)
  open import Function.Equivalence using (_⇔_; id; map; equivalence)

  open import Data.List.Relation.Unary.Any using (Any; here; there)
  open import Data.List.Membership.Propositional using (_∈_)
  open import Data.List.Relation.Sublist.Propositional using (_⊆_; ⊆-refl; lookup)
  open import Data.List.Relation.Sublist.Propositional.Properties using (++⁺; ++⁺ˡ)
  open import Relation.Binary.PropositionalEquality using (_≡_; refl)
```
</div>
```
  infix 4 _⊢_

  data Sequent : Set where
    _⊢_ : List Type → Type → Sequent
```

So what does a *proof* of a sequent look like? The logical system that
is most familiar to a computer scientist is probably *natural
deduction*. The natural deduction system for minimal propositional
logic has *three* rules:

$$
    \frac{A \in \Gamma}{\Gamma \vdash A}{\small ax}
    \quad
    \frac{A , \Gamma \vdash B}{\Gamma \vdash A \Rightarrow B}{\small{\Rightarrow}\!i}
    \quad
    \frac{\Gamma \vdash A \Rightarrow B \quad \Gamma \vdash A}{\Gamma \vdash B}{\small{\Rightarrow}\!e}
$$

Recall that λ-terms are constructed in one of three ways: a λ-term is
either a *variable*, an *abstraction* or an *application*:

$$
    M, N ::= x \mid (\lambda x . M) \mid (M\;N)
$$

These correspond exactly to the rules of natural deduction. In fact,
in type systems they are usually presented together:

$$
    \frac{(x : A) \in \Gamma}{\Gamma \vdash x : A}
    \quad
    \frac{x : A , \Gamma \vdash M : B}{\Gamma \vdash (\lambda x. M) : A \Rightarrow B}
    \quad
    \frac{\Gamma \vdash M : A \Rightarrow B \quad \Gamma \vdash N : A}{\Gamma \vdash (M\;N) : B}
$$

However, I like the clean look of the logical notation, so in the interest of keeping things simple I will use that.

In what follows, we'll use the following metavariables for types, contexts, and sequents. That means that if you see one of these names, and you can't find a binding site, it's implicitly bound at the top-level:

```
  variable A B C : Type
  variable Γ Γ′  : List Type
  variable S     : Sequent
```

We encode the natural deduction system as a datatype, with each rule corresponding to a *constructor*, and each proof a *value*:

```
  infix 3 ND_

  data ND_ : Sequent → Set where
    ax : A ∈ Γ → ND Γ ⊢ A
    ⇒i : ND A ∷ Γ ⊢ B → ND Γ ⊢ A ⇒ B
    ⇒e : ND Γ ⊢ A ⇒ B → ND Γ ⊢ A → ND Γ ⊢ B
```

Note: for the sake of brevity, I'm using an Agda notation in which
implicit arguments are hidden. That means that any unbound
variable---such as the As, Bs and Γs above---is implicitly universally
quantified.

I prefer to think of things of the type <a class="Agda Datatype
Operator">ND</a> as proofs made up of rules, but if you prefer to
think of them as programs made up of the constructors of lambda terms,
just use the following syntax:

```
  pattern var   x = ax   x
  pattern lam   x = ⇒i   x
  pattern _∙_ f x = ⇒e f x
```

Earlier, we made the conscious choice to use *lists* to represent the
antecedent. However, this introduced a minor problem: while two
programs of the same type may not do the same thing, they *should* be
equivalent, as far as the type system is concerned, and so it *should*
be possible to rewrite a program which needs *two* values of type
$$A$$ to a program which needs only *one*.

Similarily, by using lists, we have introduced a fixed order in our
antecedent which isn't exactly desirable. While they may be different
programs, we *should* be able to rewrite the program $$f : A\to B\to C$$
to receive its arguments in the different order, i.e. to a program
$$f\prime : B\to A\to C$$.

Collectively, such properties are known as *structural* properties,
and for this particular logic we can summarise them neatly as follows:

> If $$\Gamma \subseteq \Gamma\prime$$ and $$\Gamma \vdash A$$, then
> $$\Gamma\prime \vdash A$$.

We can give a proof of this theorem by induction on the structure of
natural deduction proofs. Note that we represent the subset relation
as a *function*, that is to say $$\Gamma \subseteq \Gamma\prime$$ is
the *function* $$A\in\Gamma\to A\in\Gamma\prime$$:

```
  struct : Γ ⊆ Γ′ → ND Γ ⊢ A → ND Γ′ ⊢ A
  struct Γ⊆Γ′ (ax x)   = ax (lookup Γ⊆Γ′ x)
  struct Γ⊆Γ′ (⇒i f)   = ⇒i (struct (++⁺ ⊆-refl Γ⊆Γ′) f)
  struct Γ⊆Γ′ (⇒e f g) = ⇒e (struct Γ⊆Γ′ f) (struct Γ⊆Γ′ g)
```

Note that values of type $$A\in\Gamma$$ are constructed using <a
class="Agda InductiveConstructor" target="_blank"
href="https://agda.github.io/agda-stdlib/Data.List.Any.html#1174">here</a>
and <a class="Agda InductiveConstructor" target="_blank"
href="https://agda.github.io/agda-stdlib/Data.List.Any.html#1227">there</a>,
which makes them more or less just numbers, i.e. "first value",
"second value", etc...

I mentioned two uses of this structural rule: contracting two
different variables of the *same* type into one, and exchanging the
order of the types in the antecedent. There is one more canonical use:
*weakning*.
Weakening is so obvious to programmers that they don't really think of
it, but what it says is that if you can run a program in *some*
environment, then you should *certainly* be able to run that program
in that enviroment with some irrelevant stuff added to it. Formally,
we write it as:

```
  w′ : ND Γ ⊢ B → ND A ∷ Γ ⊢ B
  w′ f = struct (++⁺ˡ _ ⊆-refl) f
```

Passing <a class="Agda InductiveConstructor" target="_blank" href="https://agda.github.io/agda-stdlib/Data.List.Any.html#1227">there</a> to <a class="Agda Function">struct</a> simply moves every value by one: the first value becomes the second, the second becomes the third, etc... In the new antecedent, the first value will be our "irrelevant stuff".


### Sequent Calculus and Natural Deduction

We've got enough to start talking about the sequent calculus now. The
sequent calculus is a different way of writing down logical systems,
and it has some pros and cons when compared to natural deduction.
It's usual presentation is as follows:

$$
    \frac{A \in \Gamma}{\Gamma \vdash A}{\small ax}
    \quad
    \frac{\Gamma \vdash A \quad A , \Gamma \vdash B}{\Gamma \vdash B}{\small cut}
    \quad
    \frac{\Gamma \vdash A \quad B , \Gamma \vdash C}{A \Rightarrow  B , \Gamma \vdash C}{\small{\Rightarrow}\!l}
    \quad
    \frac{A , \Gamma \vdash B}{\Gamma \vdash A \Rightarrow B}{\small{\Rightarrow}\!r}
$$

We can encode these rules in Agda as follows:

```
  infix 3 SC_

  data SC_ : Sequent → Set where
    ax  : A ∈ Γ → SC Γ ⊢ A
    cut : SC Γ ⊢ A → SC A ∷ Γ ⊢ B → SC Γ ⊢ B
    ⇒l  : SC Γ ⊢ A → SC B ∷ Γ ⊢ C → SC A ⇒ B ∷ Γ ⊢ C
    ⇒r  : SC A ∷ Γ ⊢ B → SC Γ ⊢ A ⇒ B
```

We will define a few patterns that we'd otherwise have to write out,
over and over again. Namely, names for the first, second, and third
variable in a context:

```
  pattern ax₀ = ax (here refl)
  pattern ax₁ = ax (there (here refl))
  pattern ax₂ = ax (there (there (here refl)))
  --- etc.
```

It's a little bit of a puzzle, but given <a class="Agda Function">w′</a> it becomes quite easy to show that the two logics are in fact equivalent---that they derive the *same sequents*:

```
  module ND⇔SC where

    ⟹ : ND S → SC S
    ⟹ (ax x)   = ax x
    ⟹ (⇒i f)   = ⇒r  (⟹ f)
    ⟹ (⇒e f g) = cut (⟹ f) (⇒l (⟹ g) ax₀)

    ⟸ : SC S → ND S
    ⟸ (ax  p)   = ax p
    ⟸ (cut f g) = ⇒e (⇒i (⟸ g)) (⟸ f)
    ⟸ (⇒l  f g) = w′ (⇒i (⟸ g)) ∙ (ax₀ ∙ w′ (⟸ f))
    ⟸ (⇒r  f)   = ⇒i (⟸ f)
```

The rules for sequent calculus obviously no longer correspond *directly*
to the λ-calculus. However, we've just shown that there is in fact
*some* correspondence between them.
In the λ-calculus, computation is represented by β-reduction, which is
the iterative removal of redexes

$$(\lambda x.M)\; N\mapsto M[x := N]$$

Likewise, sequent calculus comes equipped with its own notion of
computation: cut-elimination. And the beautiful thing about cut
elimination is that it has a *very* concrete normal form. Instead of
faffing about, claiming the structure is free of β-redexes, cut
elimination---as its name implies---allows you to remove the entire
structural rule of $$cut$$. It would be interesting to show exactly
what kind of relation cut elimination has to β-reduction...

*Alas*! It may be too much effort for a single post to implement both of
these logics *and* a procedure for cut elimination. However, there
*is* a much simpler thing we can do. Agda itself has a pretty
servicable implementation of β-reduction for Agda terms, and we can
quite easily piggyback on that mechanism. In fact, most of the
articles I linked to at the beginning do exactly this.


### Interpretations in Agda

As a first step, we write down what an interpretation is---and since we want to use the intepretation brackets in as many places as possible, we create a type class for it, and give <a class="Agda Field Operator">⟦_⟧</a> the least restrictive type possible:

<div class="hidden">
```
open import Level using (_⊔_)
```
</div>
```
record Interpret {a} {b} (A : Set a) (B : Set b) : Set (a ⊔ b) where
  field
    ⟦_⟧ : A → B
open Interpret {{...}}
```

Now, in order to interpret natural deduction proofs in Agda, we'll
need an interpretation for the atomic types. Below we say as much:

```
module Semantics (Atom : Set) {{InterpretAtom : Interpret Atom Set}} where
```

<div class="hidden">
```
  open Syntax Atom
  open import Data.Empty           using (⊥-elim)
  open import Data.List            using (List; _∷_; []; map)
  open import Function             using (_∘_)
  open import Function.Equality    using (_⟨$⟩_)
  open import Function.Equivalence using (module Equivalence)
  open Equivalence                 using (to; from)

  open import Data.List.Relation.Unary.Any using (Any; here; there)
  open import Data.List.Membership.Propositional using (_∈_)
  open import Data.List.Relation.Sublist.Propositional using (_⊆_)
  open import Relation.Binary.PropositionalEquality using (_≡_; refl)
```
</div>

Unsurprisingly, we interpret the implication as Agda's function type:

```
  instance
    InterpretType : Interpret Type Set
    InterpretType = record { ⟦_⟧ = ⟦_⟧′ }
      where
        ⟦_⟧′  : Type → Set
        ⟦ El  A ⟧′ = ⟦ A ⟧
        ⟦ A ⇒ B ⟧′ = ⟦ A ⟧′ → ⟦ B ⟧′
```

In order to interpret sequents, we'll need an interpretation for the
antecedent. For this we'll create a type for *environments*, <a
class="Agda Datatype">Env</a>, which is indexed by a list of types, and
which stores values of the *interpretations* of those types:

```
  infixr 5 _∷_

  data Env : List Type → Set where
    []  : Env []
    _∷_ : ⟦ A ⟧ → Env Γ → Env (A ∷ Γ)
```

Using this, we can interpret sequents as functions from environments
to values:

```
  instance
    InterpretSequent : Interpret Sequent Set
    InterpretSequent = record { ⟦_⟧ = ⟦_⟧′ }
      where
        ⟦_⟧′ : Sequent → Set
        ⟦ Γ ⊢ A ⟧′ = Env Γ → ⟦ A ⟧
```

Let's get to interpreting terms! First off, variables. We can
interpret variables simply by looking them up in the environment:

```
  lookup : A ∈ Γ → Env Γ → ⟦ A ⟧
  lookup (here refl) (x ∷ _) = x
  lookup (there A∈Γ) (_ ∷ e) = lookup A∈Γ e
```


(If you're wondering what we're rewriting by: the <a class="Agda
InductiveConstructor" target="_blank"
href="https://agda.github.io/agda-stdlib/Data.List.Any.html#1174">here</a>
constructor carries a small proof that the element at the top of the
list is *really* the element you were looking for.)

The translation for natural deduction proofs is, of course, completely
routine---we translate variables withs lookups, introductions by
abstractions and eliminations by applications:

```
  instance
    InterpretND : Interpret (ND S) ⟦ S ⟧
    InterpretND = record { ⟦_⟧ = ⟦_⟧′ }
      where
        ⟦_⟧′ : ND S → ⟦ S ⟧
        ⟦ ax p   ⟧′ e = lookup p e
        ⟦ ⇒i f   ⟧′ e = λ x → ⟦ f ⟧′ (x ∷ e)
        ⟦ ⇒e f g ⟧′ e = (⟦ f ⟧′ e) (⟦ g ⟧′ e)
```

Hooray! And even better,  as a corollary, we immediately obtain a
translation from sequent calculus into Agda:

```
  instance
    InterpretSC : Interpret (SC S) ⟦ S ⟧
    InterpretSC = record { ⟦_⟧ = ⟦_⟧ ∘ ND⇔SC.⟸ }
```

Which means that we've now implemented the following functions:

$$
    \begin{array}{ccc}
    ND & \rightarrow & Agda \\
                            \\
    \updownarrow            \\
                            \\
    SC                      \\
    \end{array}
$$



If you are looking for more reading on this topic, I can recommend the
highly readible *[Lambda terms for natural deduction, sequent calculus
and cut elimination][barendregt]* by Henk Barendregt and Silvia Ghilezan.

Next time, I'll talk about Gentzen's LJ, which has explicit structural
rules, and variations which use other, non-list structures as the
antecedent.

---

[agdatutorial]: http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf
[noshortcuts]: https://gergo.erdi.hu/blog/2013-05-01-simply_typed_lambda_calculus_in_agda,_without_shortcuts/
[byexample]: http://mazzo.li/posts/Lambda.html
[barendregt]: http://journals.cambridge.org/action/displayAbstract?fromPage=online&aid=44279#

[^imports]: This is a good time to note that I'm not showing any of
    the import statements. If you wish to see them, they're there in
    the HTML source. However, it may be much easier to click the
    symbol that confuses you---that should take you directly to its
    definition in the standard library.
