---
title        : "Insertion sort in Agda"
date         : 2016-03-01 12:00:00
tags         : [agda]
extra-script : [agda-extra-script.html]
---

<!--
```agda
module 2016-03-01-insertion-sort-in-agda where
```
-->

I wrote this code a long time ago, and verifiying the correctness of
some sorting algorithm is pretty much *the* standard "Hello World! I
can Agda!" blog post---well, that and implementing the
λ-calculus---but I really wanted an excuse to test my Jekyll/Agda
integration.

<!--more-->

Now, the version of insertion sort that I will write in this blog post
will be *correct by construction*. By this I mean that I will
implemented insertion sort as a function from lists to ordered lists,
where the type of ordered lists only contains ordered lists.
There are some concerns about whether this style of programming is the
right way to go. If you read a lot of Coq code, you will notice that
Coq programmers often implement functions without *any*
guarantees---e.g. they would implement insertion sort as a function from
lists to lists---and then prove the function's properties separately.
Personally, I have found that this style can lead to very clumsy code,
but there are good arguments to be made for its naive efficiency, both
in terms of time and space---if you don't need some property, you
don't have to compute its proof!

I'm getting carried away... Well, one last announcement in the public
interest: This post is written in literate Agda, and I've gone through
the effort of using the Agda hilighter. This means that all functions
and module names have links to their definitions---be it within the
post, or in the Agda standard library!

Obligatory "this is literate code, here are my imports."

```
open import Level            using (_⊔_)
open import Data.Vec         using (Vec; []; _∷_)
open import Data.Nat         using (ℕ; zero; suc)
open import Data.Sum         using (_⊎_; inj₁; inj₂)
open import Data.Product     using (∃; _,_; proj₁; proj₂)
open import Data.Empty       using (⊥-elim)
open import Relation.Nullary using (¬_; Dec; yes; no)
open import Relation.Binary
open import Relation.Binary.PropositionalEquality
```

So the first question is "What do we want to sort?" The boring answer
would be "lists of integers", but let's be a little bit more
general. We can sort anything that forms a *decidable, total
order*. Basically, this means three things:

  - we have some type A;
  - for any x and y of type A, we have a *type* of orderings between
    them, which we write as `x ≤ y`;
  - we can actually get that ordering using `x ≤? y` or `total x y`.

Below, we define our module to work for any decidable total order, and
we unpack that order. If you have a look at `≤?` and `total`, you'll
notice that they do slightly different things. For some x and y, `x ≤?
y` will tell you whether or not `x ≤ y`, whereas `total` will tell you
whether it is `x ≤ y` or `y ≤ x`.

```
module InsertionSort {c ℓ₁ ℓ₂} {{Ord : DecTotalOrder c ℓ₁ ℓ₂}} where

  open DecTotalOrder {{...}}
    using (_≤_; _≤?_; total)
    renaming (trans to ≤-trans)
  A = DecTotalOrder.Carrier Ord
```

The type A is already ordered, but it would be incredibly convenient
if it were also *bounded*---meaning that it has a value which is
smaller than everything else, and a value which is bigger than
everything else. Below, we define a wrapper for A which is bounded at
the top by ⊤ and at the bottom by ⊥:

```
  data Â : Set c where
    ⊤ : Â
    ⊥ : Â
    ⟦_⟧ : A → Â
```

We still need to encode the fact that ⊥ and ⊤ are in fact smaller and
bigger than all other values. Below, we defined the order ≲ on bounded
Â... where we simply state these facts as ⊥≲ and ≲⊤:

```
  infix 4 _≲_

  data _≲_ : Rel Â (c ⊔ ℓ₂) where
    ⊥≲ : ∀ {x} → ⊥ ≲ x
    ≲⊤ : ∀ {x} → x ≲ ⊤
    ≤-lift : ∀ {x y} → x ≤ y → ⟦ x ⟧ ≲ ⟦ y ⟧
```

Note that with the last constructor, we can lift the order of any two
values in A into ≲. However, if we only have a proof of ≰, then the
lifting is slightly more involved. Therefore, we define a function
which does this for us:

```
  ≰-lift : ∀ {x y} → ¬ (y ≤ x) → ⟦ x ⟧ ≲ ⟦ y ⟧
  ≰-lift {x} {y} y≰x with total x y
  ≰-lift y≰x | inj₁ x≤y = ≤-lift x≤y
  ≰-lift y≰x | inj₂ y≤x = ⊥-elim (y≰x y≤x)
```

Another thing we can do with two values of type Â is compute their
*minimum*. This is one example where we deviate from *correctness by
construction*: we define minimum function ⊓, and only then prove its
correctness:

```
  infix 5 _⊓_

  _⊓_ : Â → Â → Â
  ⊤ ⊓ y = y
  ⊥ ⊓ _ = ⊥
  x ⊓ ⊤ = x
  _ ⊓ ⊥ = ⊥
  ⟦ x ⟧ ⊓ ⟦ y ⟧ with x ≤? y
  ⟦ x ⟧ ⊓ ⟦ y ⟧ | yes x≤y = ⟦ x ⟧
  ⟦ x ⟧ ⊓ ⟦ y ⟧ | no  x>y = ⟦ y ⟧

  ⊓-conserves-≲ : ∀ {x y z} → x ≲ y → x ≲ z → x ≲ y ⊓ z
  ⊓-conserves-≲ {x} {⊤} {_} _ q = q
  ⊓-conserves-≲ {x} {⊥} {_} p _ = p
  ⊓-conserves-≲ {x} {⟦ _ ⟧} {⊤} p _ = p
  ⊓-conserves-≲ {x} {⟦ _ ⟧} {⊥} _ q = q
  ⊓-conserves-≲ {x} {⟦ y ⟧} {⟦ z ⟧} p q with y ≤? z
  ⊓-conserves-≲ {x} {⟦ y ⟧} {⟦ z ⟧} p _ | yes y≤z = p
  ⊓-conserves-≲ {x} {⟦ y ⟧} {⟦ z ⟧} _ q | no  y≰z = q
```

Insertion sort has rather complicated invariants. If we were implementing
mergesort, that we could define ordered lists as lists in which every
element is larger than those before it... But alas! Such a crude
analysis won't work for insertion sort! In fact, the only guarantee that
insertion sort gives us is that after one "bubble"---one iteration over
the list---the *last* element is sorted... and that after *k* insertions,
the last *k* elements are sorted...

So to implement insertion sort, we're going to need some way to represent
lists of which the last *k* elements are sorted. In fact, because it's
easier to implement, we're going to go with an encoding which ensures
us that at most the *first* *k* elements are still *unsorted*.

The `OVec` datatype below has three parameters: *l*, *n* and *k*. The
first of these is the lower bound... that is to say, of the sorted
part, the smallest element is *l*.
The second is the length of the list. One question that we've avoided
so far is the question "What sort of things can go wrong in a sorting
algorithm?" Obviously, the first thing that comes to mind is "it
doens't sort", but some other problems that it could have is that it
can *delete* elements, or *copy* elements. While keeping track of the
length doesn't solve *all* of those problems---if we delete as much as
we copy, we don't change the length---but it's a good starting point.
The last, *k*, is the number of still unsorted elements at the
beginning of the list.

There are three ways to construct an `OVec`:

  - we have the empty list, which has zero length, and zero unsorted
    elements... and a sorted segment of length zero with lower bound
    ⊤---this was the main reason to introduce Â;
  - we can add an element to the sorted segment of the list---as long
    as there aren't any unsorted elements in there yet---but we will
    have to prove that the new element is actually smaller than the
    current lower bound;
  - and finally, we can forgo all sorting, and just add some unsorted
    elements to the front of the list.

```
  data OVec : (l : Â) (n k : ℕ) → Set (c ⊔ ℓ₂) where

    []     : OVec ⊤ 0 0

    _∷_by_ : ∀ {l n} (x : A) (xs : OVec l n 0)
           → ⟦ x ⟧ ≲ l → OVec ⟦ x ⟧ (suc n) 0

    _∷_    : ∀ {l n k} (x : A) (xs : OVec l n k)
           → OVec ⊥ (suc n) (suc k)
```

If we have a regular vector---a list which tracks its length---we can
turn it into a k-ordered vector together with some lower bound. (This
is the reason we're using vectors... if we were using lists, we'd have
another existential with the lists length in it.) Our naive process of
just inserting all elements in the vector as *unsorted* means that the
lower bound will be either ⊤ or ⊥. And we can show that!

```
  fromVec : ∀ {n} → Vec A n → ∃ (λ l → OVec l n n)
  fromVec [] = ⊤ , []
  fromVec (x ∷ xs) = ⊥ , x ∷ proj₂ (fromVec xs)

  fromVec-⊤or⊥ : ∀ {n} {xs : Vec A n}
    → let l = proj₁ (fromVec xs) in l ≡ ⊤ ⊎ l ≡ ⊥
  fromVec-⊤or⊥ {.0}       {[]}     = inj₁ refl
  fromVec-⊤or⊥ {.(suc _)} {x ∷ xs} = inj₂ refl
```

And obviously, we can also turn any k-ordered vector into a regular
vector simply by forgetting about all the order evidence:

```
  toVec : ∀ {l n k} → OVec l n k → Vec A n
  toVec [] = []
  toVec (x ∷ xs) = x ∷ toVec xs
  toVec (x ∷ xs by _) = x ∷ toVec xs
```

Finally! We've developed enough vocabulary to write down what it
really means to perform an insertion:

```
  insert : ∀ {l n k} (x : A) → OVec l n k → OVec (⟦ x ⟧ ⊓ l) (suc n) k
  insert x []       = x ∷ [] by ≲⊤
  insert x (y ∷ xs) = y ∷ insert x xs
  insert x (y ∷ xs by p) with x ≤? y
  ... | yes x≤y = x ∷ (y ∷ xs by p)
                  by (≤-lift x≤y)
  ... | no  x≰y = y ∷ (insert x xs)
                  by (⊓-conserves-≲ (≰-lift x≰y) p)
```

Note that insert takes a vector with *k* unsorted elements, and
returns a vector which has one more element, but still only *k*
unsorted elements! It does this (obviously) by inserting the element
at the right position within the sorted portion of the vector.

It follows fairly easily from the fact that 'insert' inserts an
element in the sorted portion of the vector, that if we take elements
from the unsorted portion, insert it, and repeat this *k* times, we'll
have sorted *k* elements... and therefore the list.

```
  insertsort : ∀ {l n k} → OVec l n k → ∃ (λ l → OVec l n 0)
  insertsort []            = ⊤ , []
  insertsort (x ∷ xs)      = insertsort (insert x xs)
  insertsort (x ∷ xs by p) = ⟦ x ⟧ , x ∷ xs by p
```

There is one thing we haven't verified so far---and I've hinted at this
possibility above. It is fairly simple to implement an insertion sort
algorithm with the *same* type which simply takes the first element
and repeats it *n* times.
So our types aren't perfect. However, such constraints are a little
harder to encode in data types. One approach would be to construct a
sorting permutation instead of working with an input and output list.
What we could do to make this code work is to give a separate
proof---though this would go against my correctness by construction
sensibilities---stating that if an element is in the input list it is
in the output list. However, as I mostly wrote this blog post as a
test case for my Jekyll/Agda integration... I'm not going to put in
the effort to do either.

One amusing anecdote about this code is that while I was writing it, I
thought I was implementing bubble sort---so much for safety. However,
if you have a look at the invariants that both algorithms maintain,
they are really quite similar. In fact, we can easily implement bubble
sort using our `OVec` data type. The underlying algorithm is
incredibly similar to insert. However, as opposed to inserting the
first element in the correct position, "bubble" has trouble making up
its mind and drops whatever it's holding when it sees a bigger element!

```
  bubble : ∀ {l n k} (x : A) → OVec l n k → OVec (⟦ x ⟧ ⊓ l) (suc n) k
  bubble x []            = x ∷ [] by ≲⊤
  bubble x (y ∷ xs)      with x ≤? y
  ... | no  x≰y = y ∷ bubble x xs
  ... | yes x≤y = x ∷ bubble y xs
  bubble x (y ∷ xs by p) with x ≤? y
  ... | no  x≰y = y ∷ bubble x xs
                  by ⊓-conserves-≲ (≰-lift x≰y) p
  ... | yes x≤y = x ∷ bubble y xs
                  by ⊓-conserves-≲ x≲y (≲-trans x≲y p)
    where
      x≲y = ≤-lift x≤y
```

All that we need is to show that our home-brewed ≲-relation is
transitive. This follows immediately from the underlying
order. This kind of stuff---the adding of bounds to total
order---should really be provided by the standard library. And perhaps
it is, and I've simply failed to find it...

```
      ≲-trans : ∀ {x y z} → x ≲ y → y ≲ z → x ≲ z
      ≲-trans  ⊥≲         _         = ⊥≲
      ≲-trans  _          ≲⊤        = ≲⊤
      ≲-trans (≤-lift p) (≤-lift q) = ≤-lift (≤-trans p q)
```

At any rate, once we have our "bubble" function, the implementation of
the sorting algorithm is trivial---and exactly identical to the
definition of insertion sort!

```
  bubblesort : ∀ {l n k} → OVec l n k → ∃ (λ l → OVec l n 0)
  bubblesort []            = ⊤ , []
  bubblesort (x ∷ xs)      = bubblesort (bubble x xs)
  bubblesort (x ∷ xs by p) = ⟦ x ⟧ , x ∷ xs by p
```

This does lead to an interesting point: how do you know that what
you've implemented is actually what you *wanted* to implement?
Of course, a similar discussion applies much more strongly to
programming languages with weaker or non-existent type systems.
However, the point seems to be brought up more often once you stray
into the realm of verification.

Obviously, if you write your program in a language such as JavaScript,
there is nothing that tells you you've implemented the right algorithm.
And it would be rather hard to come up with a test which could tell
the difference between insertion sort and bubble sort---though a
stress-test may reveal the fact. However, in JavaScript, one cannot
even tell the difference between two completely different algorithms,
e.g. "insertion sort" and "Lehvenstein distance", without using
tests. And even then, tests generally only cover a small, finite
number of cases. You may have implemented algorithm *A* for the first
100 inputs, and algorithm *B* afterwards, and you'll never know.

Once you enter the realm of Agda, the argument can be made a little
neater: using a language with a *strong* type system, you limit the
set of all possible algorithms with your types, and you can be sure
that you've implemented *one* of the algorithms in that set.
The trick is to narrow down the set to exactly those algorithms that
you need.

In the above exercise, I failed to do so. The set of algorithms that I
selected for was the set of algorithms that turn lists into sorted
lists of equal length, without inspecting the values (other than by
comparison) and maintaining the "*k*-unsorted elements" invariant.
As we've seen, some of the algorithms in this set are insertion sort,
bubble sort, and "copy the first element *n* times". And because I
paid little attention---I'm convinced my brain simply implemented what
was an obvious optimalisation---I picked the wrong one.

The second question that usually follows is "How do you know that you've
written down the right *property*?" For instance, one small mistake in
my definition of `OVec` would have it mean "a list where sometimes an
element is smaller than one of the elements after it".
Obviously, sorting algorithms would have this property... Now, the
simple answer is that you don't. And this holds for Agda, Coq,
JavaScript, set theory... There is no real way to ensure that what
you write down, in general, corresponds to what you wanted to write
down.
But there is one redeeming factor. Set theory is believed to not be a
hot mess because there are *tons* of people who've checked the
proofs, and who've used the proved properties to prove other, more
complex properties. When you prove a lemma, you intend to *use* it to
prove some different lemma. And in general, if you've proven the wrong
lemma, your next proof will *fail*. And obviously, the notion that the
*usage* of properties and and *repeated checking* of proofs strengtens
a theory applies even more strongly to theories which are also
*machine-checked*.
