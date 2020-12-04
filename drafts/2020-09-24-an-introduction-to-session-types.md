---
title: An Introduction to Session Types
katex: true
---

Session types. Ostensibly, I’ve studied them for the past few years, so I should know something about them, right? I am gonna try and explain the *foundations* of session types, and along the way, there will be programs which crash, Victorian ladies having milk puddings, and tin can telephones.

<!--more-->

Let’s start out with a *dramatis personæ*. 

## Dramatis Personæ

Session types are about *channels*, which are like *tin can telephones*, in that you can use your tin to whisper sweet little nothings to every friend who has a tin connected to yours. I know, my love for tin can telephones is betraying my age a little—I’m an old Victorian lady.

<figure>
    [![Two victorian ladies hold a tin can telephone pulled taut between them.](/public/images/tin-can-telephone.png)](https://books.google.com/books?id=IB8_AAAAYAAJ&dq=Ebenezer+Cobham+Brewer,+Fran%C3%A7ois+Napol%C3%A9on+Marie+Moigno,+Henri+de+Parville&pg=PA227&redir_esc=y#v=onepage&q&f=false)
    <figcaption>“You simply must try my milk puddings, Ada”, Briar whispers into the telephone.</figcaption>
</figure>

*Ada*
: A Victorian Lady.

*Briar*
: Ada’s Lady Friend.

*The Tin Labelled A*
: A Tin Held by Ada.

*The Tin Labelled B*
: A Tin Held by Briar.

*The Piece of Twine*
: A Piece of Twine Connecting Tin A and B.

In the vernacular of session types, the tin cans are referred to as *channel endpoints* or simply *endpoints*, and the collection of all tin cans held together by the twine is referred to as a *channel*. A series of messages whispered back and forth over a single channel is referred to as a *session*. 

Most of the literature on session types considers only the classic scenario, in which we connect exactly two tin cans to form a channel—this is referred to as *binary session types*. Yet if we wanted to, we could make a telephone with any number of tin cans—this is referred to as *multiparty session types*.

In this blog post, we I’ll focus on *binary session types*


## Session Types *at a Glance*

Let’s imagine for a moment that Ada were to take Briar up on her offer, and ask her to sample her famous milk puddings. Briar, a proper lady, only offers her milk puddings to those who make a *sufficiently polite* request—Ada must be polite and say “please”, but she must not overuse it, lest she comes off as begging! 

We encode the interaction between Ada and Briar using *session types* in Haskell:

- Ada’s requests are represented using the `Request` datatype, which allows us to prefix a request for pudding with any number of uses of `Please`.
- Briar’s response is represented using the `Response` datatype, in which she can either grant permission, in which case Briar sends an `Allow` with a sample of pudding attached, or refuse Ada’s request, in which case she sends a `Deny` with a reason.

The functions `ada` and `briar` represent Ada and Briar—these functions each receive an endpoint for the shared channel, and communicate along the lines of our story—Ada sends a request, Briar evaluates her politeness and responds with either pudding or a refusal, and finally Ada evaluates Briars response, and expresses her emotions accordingly:

```haskell
data Request
  = Please Request
  | MayIHaveSomePudding
    
data Response
  = Allow Pudding
  | Deny String
    
ada :: Send Request (Recv Response End) -> IO ()
ada chan = do
  chan' <- send (Please MayIHaveSomePudding) chan
  (resp, chan'') <- recv chan'
  close chan''
  case resp of
    Allow pudding -> putStrLn "I’m so happy!"
    Deny reason -> putStrLn "Woe is me!"

briar :: Recv Request (Send Response End) -> IO ()
briar chan = do
  (req, chan') <- recv chan
  let resp = case req of
    MayIHaveSomePudding -> Deny "Such rudeness!"
    Please MayIHaveSomePudding -> Allow myPudding
    Please (Please _) -> Deny "Such beggary!"
  chan'' <- send resp chan'
  close chan''
```

The example illustrates a crucial notions for session types:

Firstly, session types are *communication protocols.* If you glance at the types of the endpoints, you see that they represent the communication protocol from each participants perspective. For instance, Ada’s endpoint says she must send a request, receive a response, and then end the session.

Secondly, the types of the endpoints of a binary channel must be *dual*. When Ada’s endpoint says she must send, Briar’s endpoint says she must receive. For classical multiparty session types, the equivalent notion is called *coherence*, but the principle remains the same.

Finally, each endpoint must be used *exactly once* if we want to be sure to stick to the protocol. For instance, in the code above, each channel endpoint is only used once, and each send or receive returns a new channel on which to continue the communication. If we didn’t, we would be able to write a cheeky variant of Ada, who simply tries any number of pleases until she gets that sweet, sweet pudding:

```haskell
ada :: Send Request (Recv Response End) -> IO ()
ada chan = tryAll MayIHaveSomePudding chan
  where
    tryAll req chan = do 
      chan' <- send req chan
      (resp, chan'') <- recv chan'
      close chan''
      case resp of
        Allow pudding -> putStrLn "I’m so happy!"
        Deny reason -> tryAll (Please req) chan
```

But that’s not what the protocol says! Briar doesn’t have time for more than one request, so after the first one has run its course, Ada whispers her second request into the tin can, then waits forever, pining for a response from Briar which will never come!


## A Bit of a Roadmap

Only a few short years after Ada and Briar enjoyed sweet milk puddings, a man by the name of *Alonzo Church* was born in Washington, D.C., in the United States. Three decades later, in the 1930s, Alonzo developed [the λ-calculus][church1932], a foundational calculus which studies computation using *functions*. To this day, the λ-calculus underpins most theories of functional programming languages. Talk about influential! 

Only a few short years after Alonzo developed the λ-calculus, a man by the name of *Robin Milner* was born near Yealmpton, in England. Alonzo lived a long life, over nine decades! A few years before Alonzo’s death in the mid 1990s, Robin, together with Joachim Parrow and David Walker, developed the [π-calculus][milner1992], a foundational calculus which studies concurrent computation by processes using *message-passing communication*. It wasn’t the first process calculus—it itself was heavily influenced by ideas dating back to the early 1980s—but it’s certainly one of the most influential!

We’ll start out by discussing the untyped λ-calculus. It’s a wonderful little language, and it’s *really* powerful. Unfortunately, it has all sorts of programs that do all sorts of bad things, like loop forever, so with all that power, it’s *really scary* too! We’ll then discuss the idea of taming all that scary power using types, to try and get only well-behaved programs, and the challenges of taming it without taking all the *oomph* out.

Then, we’ll switch to discussing the π-calculus. It’s a wonderful little language, even if it’s twice as big as the λ-calculus—with *six* constructs instead of *three*! It’s even more powerful than the λ-calculus—it can express all sorts of concurrent behaviours that the λ-calculus has no hope of expressing. Unfortunately, it’s scarier as well—there’s way more things that can go wrong! Again, we’ll turn our attention to taming all that scary power using types, and the problems of *oomph*’lessness that comes with it.

Finally, we’ll talk about having the best of both worlds, in a concurrent λ-calculus, which is sorta what you get when you smash the λ-calculus and the π-calculus together at high speeds! The concurrent λ-calculus has the best of both worlds: higher-order functions and concurrency with message-passing communication!


## The λ-calculus! *(So powerful, so scary…)*

The untyped λ-calculus celebrated its 89th birthday last November, so to say that it’s been around for a while undersells it a bit. It’s a pretty small system—it has only three things—there’s variables, λ-abstractions to make functions, and function applications to get rid of ’em:

$$
\begin{array}{l}
\text{Term} \; L, M, N
\\
\quad
  \begin{array}{rl}
  ::= & x
  \;\mid\;    \lambda x.M
  \;\mid\;    M \; N
  \end{array}
\end{array}
$$

There’s only one computation rule—if a function $\lambda x.M$ meets its argument $N$ we replace all occurrences of $x$ in the function body $M$ with the argument $N$. The other two reduction rules are really just there to let us reduce under function applications:

::: mathpar
$\begin{array}{c}
(\lambda x.M)\;N
\longrightarrow
M\{N/x\}
\end{array}$

$\begin{array}{c}
M
\longrightarrow
M^\prime
\\ \hline
M \; N
\longrightarrow
M^\prime \; N
\end{array}$
$\begin{array}{c}
N
\longrightarrow
N^\prime
\\ \hline
M \; N
\longrightarrow
M \; N^\prime
\end{array}$
:::

The λ-calculus is *very* powerful—*some stuff about it being a “universal model of computation”*—but that power comes at the cost of also being able to express quite a lot of scary programs that do bad stuff.

For instance, the λ-calculus comes with general recursion out of the box, via the $Y$ combinator! We’ll see an example of using the $Y$ combinator below, but essentially, $Y\;f$ represents an infinite series of applications of $f$:

$$
\begin{array}{lrl}
Y
& =               & \lambda f. (\lambda x. x \; x) (\lambda x. f \; (x \; x)) \\
\\
Y \; f
& \longrightarrow & f \; (Y \; f)
\\
& \longrightarrow & f \; (f \; (Y \; f))
\\
& \longrightarrow & f \; (f \; (f \; (Y \; f)))
\\
& \longrightarrow & \dots
\end{array}
$$

That’s good—as programmers, we like recursion! Really simplifies your programs, not having to write out the case for every possible input!

However, if you pass $Y$ the identity function, you’ll get $Ω$—the program which runs forever, but never gets anything done! Watch it reduce to right back to itself in a single step:

$$
\begin{array}{lrl}
\Omega
& =               & (\lambda x. x \; x) (\lambda x. x \; x)
\\
& \longrightarrow & (x \; x)\{\lambda x. x \; x/x\}
\\
& =               & (\lambda x. x \; x) (\lambda x. x \; x)
\end{array}
$$

That’s scary, I’d prefer not to have that! Programs which run forever, but never do a single thing—or worse, programs which are doing things the whole time, but never produce any outputs!

Most functional languages don’t just implement the core λ-calculus, but rather extend the λ-calculus with various constructs—numbers, addition, multiplication, pairs, sums, *etc.* *Technically speaking*, these can all be encoded using just functions—see, *e.g.*, Church encodings—but it tends to be *a lot* more practical and faster to use, *e.g.*, machine numbers.

For example, we can extend the untyped λ-calculus with Peano numbers. First, we extend the term language with the number *zero*, written $\text{zero}$, the successor function, written $\text{suc}$, and a pattern matching construct for numbers, written $\text{case}\;L\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}$:

$$
\begin{array}{l}
\text{Term} \; L, M, N
\\
\quad
  \begin{array}{rl}
  ::= & \dots
  \\\mid &    \text{zero}
  \;\mid\;    \text{suc}
  \\\mid &    \text{case}\;L\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
  \end{array}
\end{array}
$$

Then, we extend the reduction rules with two reduction rules for pattern matches on numbers—depending on whether the number is zero or a successor—and a rule to let us reduce under pattern matches:

::: mathpar
$\text{case}\;\text{zero}\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
\longrightarrow
M$

$\text{case}\;\text{suc}\;{L}\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
\longrightarrow
N\{L/x\}$

$\begin{array}{c}
L
\longrightarrow
L^\prime
\\ \hline
\text{case}\;L\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
\\
\downarrow
\\
\text{case}\;L^\prime\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
\end{array}$
:::

We can now define addition on Peano numbers in our calculus! Ideally, we’d write something like the following, familiar definition for addition:

$$
\begin{array}{lllcl}
\text{plus} & \text{zero} & {N} & = & \text{zero}
\\
\text{plus} & (\text{suc}\;{M}) & {N} & = & \text{suc}\;(\text{plus}\;{M}\;{N})
\end{array}
$$

Our core language doesn’t support recursive or pattern matching definitions, so we’ll have to elaborate the above definition into something less familiar, which uses the $Y$ combinator and the pattern matching construct. It’s a bit more verbose, but it’s addition nonetheless:

$$
\begin{array}{l}
\text{plus} \triangleq \lambda{x}.\lambda{y}.(Y\;(\text{plus}^\prime\;{y}))\;{x}
\\
\quad\textbf{where}
\\
\quad\quad
\text{plus}^\prime \triangleq \lambda{y}.\lambda{\text{rec}}.\lambda{x}.
\\
\quad\quad\quad\text{case}\;{x}\;\text{of}\;\left\{
\begin{array}{lcl}
\text{zero} & \mapsto & \text{zero};
\\
\text{suc}\;{x^\prime} & \mapsto & \text{suc}\;(\text{rec}\;{x^\prime}\;{y})
\end{array}
\right\}
\end{array}
$$

Woe is us! We have *another* kind of problem! We now have to worry about programs like $\text{plus}\;(\lambda x.x)\;\text{zero}$. What does that even mean?! According to our semantics, it means exactly that, since it doesn’t reduce any further… It’s stuck on the pattern match on $\lambda x.x$, since there’s no case for functions.

Problems like these are less obvious when using, *e.g.*, Church encodings, since everything is just functions. For instance, if we use Church-encoded Peano numbers to compute $\text{plus}_{ch} \; (\lambda x.x) \; \text{zero}_{ch}$, and convert the result to our builtin Peano numbers, we find that adding the identity function to the number *zero* gives us *one*:

$$
\begin{array}{lcl}
\text{zero}_{ch} & \triangleq & \lambda s. \lambda z. z
\\
\text{suc}_{ch}  & \triangleq & \lambda n. \lambda s. \lambda z. s \; (n \; s \; z)
\\
\text{plus}_{ch} & \triangleq & \lambda m. \lambda n. \lambda s. \lambda z. m \; s \; (n \; s \; z)
\end{array}
$$

$$
\begin{array}{l}
\text{plus}_{ch} \; (\lambda x.x) \; \text{zero}_{ch} \; \text{suc} \; \text{zero}
\\
\quad
  \begin{array}{rl}
  \longrightarrow & (\lambda x.x) \; \text{suc} \; (\text{zero}_{ch} \; \text{suc} \; \text{zero})
  \\
  \longrightarrow & (\lambda x.x) \; \text{suc} \; \text{zero}
  \\
  \longrightarrow & \text{suc} \; \text{zero}
  \end{array}
\end{array}
$$

All in all, we’ve identified *two* main problems with using the untyped λ-calculus as a foundation for programming languages:

1. it has programs which loop forever but produce nothing; and
2. it has no way of making sure that data is used as intended.

Specifically, for the second problem, we have the choice between programs which get stuck and programs which compute nonsense. If we want programs that misuse data to get stuck, we tag our data—by using, *e.g.*, a syntactically distinct $\text{zero}$ constructor—and check the types of our data at runtime. If we’re fine with the misuse of data, we encode everything as functions, and accept whatever results fall out of our encoding.


## Taming the λ-calculus with types…

So, we’ve got a lot of scary stuff going on, stuff we really rather wouldn’t have, like programs which uselessly loop forever, and programs which try to add numbers to functions. What can we do?

One of the *simplest* solutions—horrible pun *absolutely* intended—is to use simple types. You see, in 1940, in an attempt to get rid of these unhelpful loops, Alonzo developed [the simply-typed λ-calculus][church1940].

We start by formalising what we mean by *type*. Since all we’ve got is functions, all we need is a function type $A \to B$ and *some* base type—it doesn’t really matter what, so we’re gonna call it $\star$:

$$
\begin{array}{l}
\text{Type} \; A, B, C
\\
\quad
  \begin{array}{rl}
  ::= & \star
  \;\mid\; A \to B
  \end{array}
\end{array}
$$

<!--!>
I'd do three things here:

1) Motivate why you need typing contexts: say a variable might 'stand for' a base value, or another function, and you need to differentiate between them statically

2)  Spell out the general form of the judgement: state that if you've got \Gamma \vdash M : A, then it means that 'under typing environment \Gamma, term M has type A'

3) Describe how to read the inference rules (e.g., 'if M has a function type A -> B, and N has type A, then the application M N has type B')
<---->

With types in hand, we write down some *typing rules*. The goal is that *if* we can construct a typing derivation for a term, that term will be well-behaved. Terms are checked in the context of some typing environment, which we’ll refer to with the variable $\Gamma$ or $\Delta$. Typing environments are just a bunch of typing assignments $x : A$. There are three rules, corresponding to the three term constructs:

::: mathpar
$\begin{array}{c}
x : A \in \Gamma
\\ \hline
\Gamma \vdash x : A
\end{array}$
$\begin{array}{c}
\Gamma, x : A \vdash M : B
\\ \hline
\Gamma \vdash \lambda x.M : A \to B
\end{array}$
$\begin{array}{c}
\Gamma \vdash M : A \to B \quad \Gamma \vdash N : A
\\ \hline
\Gamma \vdash M \; N : B
\end{array}$
:::

In order of appearance:

- A variable $x$ has type $A$ if there’s an assignment in $\Gamma$ that says so.
- If we’ve got something of type $B$ which uses something of type $A$ from the bag, we can abstract over that something to create a function of type $A \to B$.
- If we’ve got something of type $A \to B$ and something of type $A$, then we can apply the former to the latter to get something of type $B$.

Guess what?! It works! All the programs you can type with these rules are super well-behaved and nice! Buuuut… there’s kinda a lot of programs that are really nice and good, that you can’t type with these rules… Very, *very*, notably, you can’t type the $Y$ combinator. Oh no! We lost recursion!

Queue the history of type theory, trying to wrangle with this, trying to make this system more permissive while still keeping lots of the scary stuff out! 

It’s, *uh*, pretty hard to get *extactly* the bad looping stuff out, so some folks are like “eh, we’ll keep the looping stuff, but still use types to get rid of all that ‘adding functions to numbers’ nonsense”, whereas other folks are all hardcore and decide that “no it has to be terminating all the way even if it becomes pretty hard to use!”

<!--!>
Two minor points:

1) I'd put in a subheading here

2) I think you need to motivate why the "only once" restriction is important before going into the formalism, even if it's just referring back to Ada's channel endpoint back in the previous section
<---->

Let’s briefly talk about another type system for the λ-calculus—but only because it’ll turn out to be highly relevant to session types, I haven’t forgotten what I promised to write about! Let’s talk about [the linear λ-calculus][wadler1993].

In its most minimal form, the linear λ-calculus demands that every variable is used *exactly once*. When you check a function application, you have to decide which parts of the typing environment are gonna be used in the function, and which parts in the argument. By the time you’ve made it all the way down to a variable, the bag is supposed to be empty save for the variable you’re checking. Everything else must’ve already been split off for usage elsewhere.

Also, we now use this cute little lollipop instead of the function arrow:

$$
\begin{array}{l}
\text{Type} \; A, B, C
\\
\quad
  \begin{array}{rl}
  ::= & \star
  \;\mid\; A \multimap B
  \end{array}
\end{array}
$$

::: mathpar
$\begin{array}{c}
\\ \hline
x : A \vdash x : A
\end{array}$
$\begin{array}{c}
\Gamma, x : A \vdash M : B
\\ \hline
\Gamma \vdash \lambda x.M : A \multimap B
\end{array}$
$\begin{array}{c}
\Gamma \vdash M : A \multimap B \quad \Delta \vdash N : A
\\ \hline
\Gamma, \Delta \vdash M \; N : B
\end{array}$
:::

As a type system, this is *highly restrictive*. Essentially, what we’re left with is a calculus of permutations. Think of lists… if you’re writing a function from lists to lists, but you *have to* use every element in the list exactly once, what kinds of programs can you write? Permutations. That’s it.


## The π-calculus! *(Is even scarier…)*

Oof, that was a bit of a detour, wasn’t it? Wanna talk about *session types*, the thing that I promised I’d talk about? Okay, let’s do it! The π-calculus is pretty young—it didn’t show up until 1992, though it’s heavily influenced by ideas dating back to the 1980s. Unlike with the λ-calculus, there’s not really a *canonical* π-calculus that everyone agrees on, so the one I’m presenting here is just kinda the version that I felt like presenting.

It’s also pretty big! It’s got twice as many *things* in it as the λ-calculus. Instead of functions, we’re talking about processes, which are built using *six* different constructors:

$$
\begin{array}{l}
\text{Process}\;{P},{Q},{R}
\\
\quad
  \begin{array}{rll}
     ::=    & (\nu {x}){P}           &\text{— create new channel}
  \\\mid    & ({P}\parallel{Q})      &\text{— put }P\text{ and }Q\text{ in parallel}
  \\\mid    & 0                      &\text{— done}
  \\\mid    & x\langle{y}\rangle.{P} &\text{— send }y\text{ on }x
  \\\mid    & x(y).{P}               &\text{— receive }y\text{ on }x
  \\\mid    & !{P}                   &\text{— replicate }P
  \end{array}
\end{array}
$$

In order of appearance:

- We’ve got ν-binders, written $(\nu x)P$, which create a new channel $x$, which can be used in $P$.
- We’ve got parallel composition, written $\parallel$, to let you know that two processes are running in parallel.
- We’ve got nil, written $0$, the process which is done.
- We’ve got *send*, written $x \langle y \rangle.P$, which sends some $y$ on $x$, and then continues as $P$.
- We’ve got *receive*, written $x ( y ).P$, which receives some value on $x$, names it $y$, and then continues as $P$.
- We’ve got replication, written $! P$, which represents a process $P$ which is replicated an arbitrary number of times.

Replication isn’t truly *essential* to the π-calculus, it’s just that we can’t do any sort of *infinite* behaviour with just sending and receiving, so we have to add it explicitly. Other solutions, like adding recursive definitions, work as well.

There’s only one computation rule—if we’ve got a send and a receive in parallel, we perform the communication, and replace all instances of the name bound by the receive instruction by the actual value sent. The other three reduction rules are just there to let us reduce under parallel compositions and ν-binders:

::: mathpar
$\begin{array}{c}
x\langle{y}\rangle.{P}\parallel x(z).{Q}
\longrightarrow
{P}\parallel{Q}\{y/z\}
\end{array}$

$\begin{array}{c}
{P}
\longrightarrow
{P}^\prime
\\ \hline
(\nu x){P}
\longrightarrow
(\nu x){P}^\prime
\end{array}$
$\begin{array}{c}
{P}
\longrightarrow
{P}^\prime
\\ \hline
{P}\parallel{Q}
\longrightarrow
{P}^\prime\parallel{Q}
\end{array}$
$\begin{array}{c}
{Q}
\longrightarrow
{Q}^\prime
\\ \hline
{P}\parallel{Q}
\longrightarrow
{P}\parallel{Q}^\prime
\end{array}$
:::

However, these rules in and of themselves are not enough. You see, a parallel composition $P \parallel Q$ isn’t intended to be *ordered*—I mean, if you’ve got two processes in parallel, does it make sense to say that one of them is “to the left of” the other?—but we haven’t told the reduction semantics about that. That means that with the rules we’ve given so far, we cannot reduce the following:

$$
(\nu x)(x(z).{Q}\parallel x\langle{y}\rangle.{P})
$$

Why not? The send and the receive are in the wrong order—our computation rule requires that the send is *to the left of* the receive, so we can’t apply it:

$$
x\langle{y}\rangle.{P}\parallel x(z).{Q}
\longrightarrow
{P}\parallel{Q}\{y/z\}
$$

One solution is to tell the reduction semantics that the order of processes doesn’t matter—along with a few other things:

$$
\begin{array}{lrll}
  P \parallel Q
  & \equiv
  & Q \parallel P
  \\
  P \parallel (Q \parallel R)
  & \equiv
  & (P \parallel Q) \parallel R
  \\
  P \parallel 0
  & \equiv
  & P
  \\
  (\nu x)0
  & \equiv
  & 0
  \\
  (\nu x)(\nu y)P
  & \equiv
  & (\nu y)(\nu x)P
  \\
  (\nu x)(P \parallel Q)
  & \equiv
  & (\nu x)P \parallel Q,
  & \text{if}\;x\not\in{Q}
  \\
  !{P}
  & \equiv
  & !{P}\parallel{P}
\end{array}
$$

In order of appearance:

- Parallel composition is *commutative* and *associative*, *i.e.* the order of parallel processes doesn’t matter.
- We can remove (and add) processes which are done.
- We can remove (and add) ν-binders which aren’t used.
- The order of ν-binders doesn’t matter.
- We can swap ν-binders and parallel compositions as long as we don’t accidentally move an endpoint out of the scope of its binder.
- Replicated processes can be, *well*, replicated. We kinda forgot to add this at first, so we didn’t have any infinite behaviour… but now we do!

Now that we have this equivalence of processes—usually called *structural congruence*—we can embed it in the reduction relation:

$$
\begin{array}{c}
  P \equiv P^\prime \quad P^\prime \longrightarrow Q^\prime \quad Q^\prime \equiv Q
  \\ \hline
  P \longrightarrow Q
\end{array}
$$

The reason we’re embedding it this way, with a reduction step sandwiched between two equivalence, is because the equivalence relation isn’t super well-behaved—there’s plenty of infinite chains of rewrite rules, *e.g.*, imagine swapping $P \parallel Q$ back and forth forever, or duplicating $! P$ forever—and we’d prefer not to have any infinite chains of reductions. Embedding it this way forces there to be at least one *real* computation step in each reduction step, because the only way to construct a reduction is to start with a computation.

<!--!>
I think it may be worth stating explicitly  upfront that this isn't surprising, because as well as all the sequential problems, you've also got the way these sequential programs can interact
<---->

If you thought the λ-calculus had problems, have I got news for you. There’s all the old problems we had with the lambda calculus. We’ve got processes that reduce forever without doing anything:

$$
\begin{array}{l}
(\nu x)(!{x}\langle{y}\rangle.0 \parallel !{x}({z}).0)
\\
\quad
  \begin{array}{rl}
  \equiv
  & (\nu x)( !{x}\langle{y}\rangle.0 
    \parallel {x}\langle{y}\rangle.0 
    \parallel !{x}({z}).0 
    \parallel {x}({z}).0)
  \\
  \longrightarrow
  & (\nu x)( !{x}\langle{y}\rangle.0 \parallel !{x}({z}).0)
  \end{array}
\end{array}
$$

Ah! One process which just *keeps* sending the value $y$ to another process, which throws it away immediately, just sending and throwing away, *forever*. Isn’t that the kind of programs we all like to write?

Plus, if we add numbers, we could try to send over the number 5, foolishly assuming it’s a channel.

But *what fun*! There’s new problems as well! If we’ve got two *pairs* of processes, both of which are trying to communicate over $x$ at the same time, then reduction is not longer *deterministic*—it’s anyone’s guess which message will end up where! Yaaay, it’s *race conditions*:

$$
\begin{array}{l}
  (\nu x)%
  \left(
  \begin{array}{l}
  x\langle{y_1}\rangle.{P_1}\parallel x(z_1).{Q_1}\parallel
  \\
  x\langle{y_2}\rangle.{P_2}\parallel x(z_2).{Q_2}
  \end{array}
  \right)
  \\
  \\
  \quad
  \begin{array}{rl}
  \longrightarrow
  & (\nu x)%
    \left(
    \begin{array}{l}
    {P_1}\parallel {Q_1}\{y_1/z_1\}\parallel
    \\
    {P_2}\parallel {Q_2}\{y_2/z_2\}
    \end{array}
    \right)
  \\
  \mathit{or}
  \\
  \longrightarrow
  & (\nu x)%
    \left(
    \begin{array}{l}
    {P_1}\parallel {Q_1}\{y_2/z_1\}\parallel
    \\
    {P_2}\parallel {Q_2}\{y_1/z_2\}
    \end{array}
    \right)
  \end{array}
\end{array}
$$

Another *fun* thing we can do is write two processes which echo a message on $x$—they receive something and send it back… but the twist is, they’re both wanting to *receive* first! Ack, it’s a *deadlock*:

$$
\begin{array}{c}
  (\nu x)%
  \left(
  x(y).x\langle{y}\rangle.0 \parallel x(z).x\langle{z}\rangle.0
  \right)
  \not\longrightarrow
\end{array}
$$

To be fair, it’s not surprising that race conditions and deadlocks show up in a foundational calculus for *concurrency*—it’d be weird if they didn’t. But that does mean that as programming languages people, we now have two new problems to worry about! To summarise, we’ve identifier *four* main problems with the untyped π-calculus as a foundation for programming languages:

1. it has programs which loop forever but produce nothing;
2. it has no way of making sure that data is used as intended;
3. it has *race conditions*; and
4. it has *deadlocks*.


## Taming the π-calculus with types…

Oh dear, so many problems to solve. Where do we begin? 

It may help to think a little deeper about the latter three problems. In a sense, we could see a deadlock as the consequence of us not using a channel as intended. After all, we probably intended for one party to be sending while the other was receiving. We could see a race condition in a similar light. We intended for the two pairs of processes to communicate in a *predictable* pattern, in pairs of two. 

These are overly simplistic descriptions—sometimes we truly don’t care about the order of messages, and a bit of a race is fine. However, much like with the λ-calculus, we’re gonna try and cut all the bad stuff out first, no matter what cost, and then get to the business recovering what we lost.

Let’s have a look at *session types*, invented in the early 1990s by [Kohei Honda][honda1993], and let’s focus first and foremost on one important property—*session fidelity*. Essentially, it means that we communicate over a channel as intended by its protocol—or *session type*. 

Let’s start with the simplest system, where there’s only two things we can do with a channel—send and receive. That’ll be our language of session types—either we send on a channel, we receive from a channel, or we’re done:

$$
\begin{array}{l}
\text{Session type}\;{S}\quad
\\
\quad
  \begin{array}{rll}
     ::= & !S.S'        &\text{— send}
  \\\mid & ?S.S'        &\text{— receive}
  \\\mid & \mathbf{end} &\text{— done}
  \end{array}
\\
\end{array}
$$

A crucial notion in session types—as mentioned in the introduction—is *duality*, the idea that while I’m sending a message, you should be expecting to receive one, and vice versa. Duality is a function on session types:

$$
\begin{array}{lrl}
\overline{!S.S'}        & = & ?S.\overline{S'}
\\
\overline{?S.S'}        & = & !S.\overline{S'}
\\
\overline{\mathbf{end}} & = & \mathbf{end}
\end{array}
$$

Finally, before we get to the typing rules, we’re gonna make one tiny tweak to the syntax for processes. Before, our ν-binders introduced a *channel name*, which any process could then use to communicate on. Now, ν-binders introduce a channel by its two *channel endpoint names*. It’s gonna make the typing rules a *bunch* easier to write down:

$$
\begin{array}{l}
\text{Process}\;{P},{Q},{R}
\\
\quad
  \begin{array}{rll}
     ::=    & (\nu xx'){P} &\text{— create new channel }{x}{\leftrightarrow}{x'}
  \\\mid    & \dots
  \end{array}
\end{array}
$$

We’ve also gotta propagate this changes through the structural congruence and the reduction rules. It’s pretty straightforward for most of the changes—just replace $(\nu x)$ with $(\nu x x')$—but there’s one extra change we have to make to the computation rule, which is a bit of a shift in perspective. See, in our first iteration of the π-calculus, processes were connected by virtue of having access to the same *channel name*, and ν-binders were merely a convenience to let us hide channel names from the outside world. However, in our current version, processes use *unrelated* endpoint names, and it’s the ν-binder who connects them to form a channel. Or—if you will—each participant is just holding a tin can, and they can’t be used to communicate until they’re bound together with twine. That means that we’ll have to *require* that reduction takes place under a ν-binder:

$$
(\nu x x')(x\langle{y}\rangle.{P}\parallel x'(z).{Q})
\longrightarrow
(\nu x x')({P}\parallel{Q}\{y/z\})
$$

Okay, typing rules! One thing that’s very different from the λ-calculus is that the typing rules only check whether processes use channels correctly—the processes themselves don’t have types. There are six rules, corresponding to the six term constructs:

::: mathpar
$\begin{array}{c}
\Gamma, x : S, x' : \overline{S} \vdash P
\\ \hline
\Gamma \vdash (\nu xx') P
\end{array}$
$\begin{array}{c}
\Gamma \vdash P
\quad
\Delta \vdash Q
\\ \hline
\Gamma, \Delta \vdash P \parallel Q
\end{array}$
$\begin{array}{c}
\\ \hline
\varnothing \vdash 0
\end{array}$
$\begin{array}{c}
\Gamma \vdash P
\\ \hline
\Gamma, x : \mathbf{end} \vdash P
\end{array}$

$\begin{array}{c}
\Gamma, x : B \vdash P
\\ \hline
\Gamma, x : {!}A.B, y : A \vdash x\langle{y}\rangle.P
\end{array}$
$\begin{array}{c}
\Gamma, y : A, x : B \vdash P
\\ \hline
\Gamma, x : {?}A.B \vdash x(y).P
\end{array}$
:::

In order of appearance:

- If we’ve got a process $P$ which uses two endpoints $x$ and $x'$ at dual session types, then we can connect them to form a channel, written $(\nu x x')P$.
- If we’ve got two processes $P$ and $Q$, which use channels according to the session types in $\Gamma$ and $\Delta$, then we can put those processes in parallel, written $P \parallel Q$, and the resulting process will use channels according to the session types in $\Gamma, \Delta$, where $\Gamma$ and $\Delta$ *must be disjoint*.
- The terminated process is done—it doesn’t use any channels.
- If we’ve got a channel $x$ of type $!A.B$ and some $y$ of type $A$, then we can send $y$ over $x$. We lose access to $y$—we’ve sent it away, after all—and the channel $x$ continues as a channel of type $B$.
- If we’ve got a channel $x$ of type $?A.B$, then we can receive something of type $A$ over $x$—call it $y$—after which the channel $x$ continues as a channel of type $B$.
- Finally, if we’ve got a channel which is done, we can forget about it.

<!--!>
It is a bit jarring to contrast the two 'flavours' of linearity here. I can't see a better way of doing it, though. I guess it's worth reiterating that the process calculus formulation automatically does the rebinding in the continuation, much like if you did 'let' in the lambda calculus
<---->

This type system is *linear*, much like the linear λ-calculus we saw earlier. Told you it’d be relevant! Anyway, it’s not linear in quite the same way, since channels can be used *multiple times*. However, each step in the protocol has to be executed *exactly once*!

So, did it work? Are we safe from the bad programs? Yes and no. Which, *uh*, kinda just means no? But there’s *some* bad programs we got rid of! There are no longer programs which misuse data, since everything strictly follows protocols. Since every channel has *exactly two* processes communicating over it, we no longer have any race conditions. Furthermore, because those two processes must act *dually* on the channel, we no longer have any deadlocks *within a single session*—that is to say, as long as each two processes only share *one* means of communication, we don’t have any deadlocks. Unfortunately, it’s quite easy to write a program which interleaves *two* sessions and deadlocks:

$$
\begin{array}{c}
  (\nu x x')(\nu y y')%
  \left(
  x(z).y\langle{z}\rangle.0 \parallel y'(w).x'\langle{w}\rangle.0
  \right)
  \not\longrightarrow
\end{array}
$$

We also don’t have looping programs anymore, but, *uh*, that’s mostly because we removed replication, so… win?

How do we get rid of those last few deadlocks? The ones caused by having multiple open lines of communication with another process? There’s several different ways to do this, and they all have their advantages and disadvantages:

The first option, used in, *e.g.*, the logic-inspired session type systems by [Luís Caires and Frank Pfenning][caires2012] and [Philip Wadler][wadler2014], is to just say “The problem happens when you’ve got multiple open lines of communication with another process? Well, have you considered just not doing that?” Essentially, these type systems require that the *communication graph is acyclic*—what that means is that, if you drew all the participants, and then drew lines between each two participants who share a channel, you wouldn’t draw any cycles. This works! Can’t share multiple channels if you don’t share multiple channels, am I right? But it’s a *wee bit* restrictive… Got some homework about [forks and hungry, hungry philosophers][dining-philosophers] you need to do? Nope. Wanna write a neat cyclic scheduler? Not for you. However, it has some *nice* aspects too—it composes! Got two deadlock-free programs? Well, you can put ’em together, and it’s gonna be a deadlock-free program! If we updated our typing rules, they’d look a little like this:

::: mathpar
$\begin{array}{c}
\Gamma, x : S \vdash P
\quad
\Delta, x' : \overline{S} \vdash Q
\\ \hline
\Gamma \vdash (\nu x x')(P \parallel Q)
\end{array}$
$\begin{array}{c}
\\ \hline
\varnothing \vdash 0
\end{array}$
$\begin{array}{c}
\Gamma \vdash P
\\ \hline
\Gamma, x : \mathbf{end} \vdash P
\end{array}$

$\begin{array}{c}
\Gamma, x : B \vdash P
\\ \hline
\Gamma, x : {!}A.B, y : A \vdash x\langle{y}\rangle.P
\end{array}$
$\begin{array}{c}
\Gamma, y : A, x : B \vdash P
\\ \hline
\Gamma, x : {?}A.B \vdash x(y).P
\end{array}$
:::

We’ve glued the ν-binder and the parallel composition together in a single operation which makes sure that one endpoint goes one way and the other the other.

The second option, developed by [Naoki Kobayashi][kobayashi2002], is to just do a whole-program check for deadlocks. Take out a piece of paper, and draw a blob for every dual pair of send and receive actions in your program. For every action, if it *has to* happen before another action, draw an arrow between their blobs. Finally, check to see if there’s any *directed* cycles—for each blob, see if you can follow the arrows and end up back at the same blob. We can do this *formally* by adding some little blobs to our session types—since each session type connective corresponds to an action—and requiring a particular order on the little blobs. For reference, little blobs are more commonly known as *priorities*. If we updated our typing rules, they’d look a little like this… first, we add little blobs to our session types. Duality *preserves* priorities.

::: mathpar
$\begin{array}{l}
\text{Session type}\;{S}\quad
\\
\quad
  \begin{array}{rll}
     ::= & !^oS.S'        &\text{— send}
  \\\mid & ?^oS.S'        &\text{— receive}
  \\\mid & \mathbf{end}^o &\text{— done}
  \end{array}
\\
\end{array}$
$\begin{array}{lrl}
\overline{!^oS.S'}        & = & ?^oS.\overline{S'}
\\
\overline{?^oS.S'}        & = & !^oS.\overline{S'}
\\
\overline{\mathbf{end}^o} & = & \mathbf{end}^o
\end{array}$
:::

We also define a function, $\text{Pr}$, to nab the topmost priority from a session type. If we apply $\text{Pr}$ to a $\Gamma$ we mean to get the *smallest* priority of all types in $\Gamma$—actions with smaller priorities happen earlier, so we essentially wanna know when the *first* action in $\Gamma$ is gonna happen.

$$
\begin{array}{lrl}
\text{Pr}{(!^oS.S')}        & = & o
\\
\text{Pr}{(?^oS.S')}        & = & o
\\
\text{Pr}{(\mathbf{end}^o)} & = & o
\end{array}
$$

Then we change the typing rules:

::: mathpar
$\begin{array}{c}
\Gamma, x : S, x' : \overline{S} \vdash P
\\ \hline
\Gamma \vdash (\nu xx') P
\end{array}$
$\begin{array}{c}
\Gamma \vdash P
\quad
\Delta \vdash Q
\\ \hline
\Gamma, \Delta \vdash P \parallel Q
\end{array}$
$\begin{array}{c}
\\ \hline
\varnothing \vdash 0
\end{array}$
$\begin{array}{c}
\Gamma \vdash P
\\ \hline
\Gamma, x : \mathbf{end} \vdash P
\end{array}$

$\begin{array}{c}
\Gamma, x : B \vdash P
\quad
o < \text{Pr}{(\Gamma, x : B)}
\\ \hline
\Gamma, x : {!^o}A.B, y : A \vdash x\langle{y}\rangle.P
\end{array}$

$\begin{array}{c}
\Gamma, y : A, x : B \vdash P
\quad
o < \text{Pr}{(\Gamma, y : A, x : B)}
\\ \hline
\Gamma, x : {?^o}A.B \vdash x(y).P
\end{array}$
:::

We’re enforcing two things here:

1. If we write $x(y).P$, then the action $x(y)$ must happen before *everything else in* $P$. Similarly for $x\langle{y}\rangle.P$.
2. If we connect two endpoints, $(\nu x x')$, then the dual actions on those endpoints must each happen *at the same time*. 

There’s a really nice recent example of a session type system which uses this technique by [Ornela Dardha and Simon Gay][dardha2018]. The upside of this technique is that you can have all sorts of neat cyclic communication graphs, and still rest assured knowing that they don’t do anything scary. The downside is that it’s a *whole-program* check, meaning that if you’ve got two deadlock-free programs, and you put ’em together, you have to check again, to see that you didn’t introduce any deadlocks.

Anyway, we’ve now got a *mostly* safe foundation for session types! You don’t see type systems *this* simple touted in papers much—or *at all*, as far as I’m aware. The reason is probably that they’re kinda *too simple*. Just like with the *purely* linear λ-calculus, there’s not much you can actually compute with these programs, and you have to put in a little bit of work before you can actually get to a point where you get back enough expressivity to be taken seriously as a programming language. However, I thought it would be illustrative to discuss the *simplest possible* type systems.

Once we’ve got this foundation, we can get to work extending it! With a little bit of effort we could add branching, replication, recursion, several forms of shared state, polymorphism, higher-order processes, *etc.* However, figuring out how all this stuff works in the context of the π-calculus seems like a bit of a waste, especially when we already know how they work within the context of the λ-calculus. Plus, doesn’t that “adding higher-order processes” thing sound suspiciously like adding higher-order functions?


## Concurrent λ-calculus *(λ and π, together forever)*

::: mathpar
$\begin{array}{l}
  \begin{array}{l}
  \text{Term} \; L, M, N
  \\
  \quad
    \begin{array}{rl}
    ::=    & x 
    \\\mid & \lambda x.M
    \\\mid & M \; N
    \\\mid & K
    \end{array}
  \end{array}
\end{array}$
$\begin{array}{l}
\text{Process}\;{P},{Q},{R}
\\
\quad
  \begin{array}{rll}
     ::=    & (\nu xx'){P}
  \\\mid    & ({P}\parallel{Q})
  \\\mid    & \circ M
  \\\mid    & \bullet M
  \end{array}
\\
\end{array}$
:::

$$
\text{Const} \; K ::= \mathbf{send} \mid \mathbf{recv} \mid \mathbf{new} \mid \mathbf{spawn}
$$

We keep the reduction rules from both the lambda and pi calculus.
We keep the structural congruence from the pi calculus.
We add two new reduction rules for new and spawn.
Basically:
- if you’ve got a new term, you turn it into a nu-binder and return the channels.
- if you’ve got a spawn term, you spawn off its argument as a new thread.

We keep the typing rules from both the lambda and pi calculus.


---

**Disclaimer**: I haven’t proven any safety properties for any of the calculi presented here. The simply-typed λ-calculus is pretty well established, so you can trust that’s good, but the other systems are potentially destructive simplifications of existent systems, so all bets are off! I guess you could do the proofs yourself—or if you wanna be really safe, refer to the papers I’ve linked. However, I’ve opted to make these simplifications because the smallest typed π-calculi which are *actually* expressive tend to be pretty big already

[church1932]: https://www.jstor.org/stable/1968337
[church1940]: https://www.jstor.org/stable/2266170
[milner1992]: http://www.lfcs.inf.ed.ac.uk/reports/89/ECS-LFCS-89-85/
[wadler1993]: https://homepages.inf.ed.ac.uk/wadler/papers/lineartaste/lineartaste-revised.pdf
[honda1993]: http://www.kurims.kyoto-u.ac.jp/~kyodo/kokyuroku/contents/pdf/0851-05.pdf
[kobayashi2002]: https://doi.org/10.1016/S0890-5401(02)93171-8
[wadler2014]: https://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-sessions/propositions-as-sessions-jfp.pdf
[caires2012]: https://widgets.figshare.com/articles/6606974/embed
[dardha2018]: http://www.dcs.gla.ac.uk/~ornela/publications/DG18-Extended.pdf
[dining-philosophers]: https://en.wikipedia.org/wiki/Dining_philosophers_problem
[pi-calculus-impl]: https://en.wikipedia.org/wiki/%CE%A0-calculus#Implementations
[occam-pi]: https://web.archive.org/web/20190214193147/http://pop-users.org:80/occam-pi/
[pict]: https://www.cis.upenn.edu/~bcpierce/papers/pict/Html/Pict.html
