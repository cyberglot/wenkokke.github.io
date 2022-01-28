---
title: An Introduction to Session Types
katex: true
css-include:
- "/assets/css/highlight.css"
- "/assets/css/mathpar.css"
---

Session types. Ostensibly, I’ve studied them for the past few years, so I should know something about them, right? I am gonna try and explain the *foundations* of session types, and along the way, there will be programs which crash, Victorian ladies having milk puddings, and tin can telephones.

<!--more-->

Let’s start out with a *dramatis personæ*.

# Dramatis Personæ

Session types are about *channels*, which are like *tin can telephones*, in that you can use your tin to whisper sweet little nothings to every friend who has a tin connected to yours. I know, my love for tin can telephones is betraying my age a little—I’m an old Victorian lady.

<figure>
    [![Two victorian ladies hold a tin can telephone pulled taut between them.](/assets/images/tin-can-telephone.png)](https://books.google.com/books?id=IB8_AAAAYAAJ&dq=Ebenezer+Cobham+Brewer,+Fran%C3%A7ois+Napol%C3%A9on+Marie+Moigno,+Henri+de+Parville&pg=PA227&redir_esc=y#v=onepage&q&f=false)
    <figcaption>“You simply must try my milk puddings, Ada”, Briar whispers into the telephone.</figcaption>
</figure>

*Ada*
: A Victorian Lady.

*Briar*
: Ada’s Lady Friend.

*The Tin Labelled A*
: A Tin Held by Ada.

*The Tin Labelled B*
: A Tin Held by Briar.

*The Piece of Twine*
: A Piece of Twine Connecting Tin A and B.

In the vernacular of session types, the tin cans are referred to as *channel endpoints* or simply *endpoints*, and the collection of all tin cans held together by the twine is referred to as a *channel*. A series of messages whispered back and forth over a single channel is referred to as a *session*.

Most of the literature on session types considers only the classic scenario, in which we connect exactly two tin cans to form a channel—this is referred to as *binary session types*. Yet if we wanted to, we could make a telephone with any number of tin cans—this is referred to as *multiparty session types*.

In this blog post, we I’ll focus on *binary session types*


# Session Types *at a Glance*

Let’s imagine for a moment that Ada were to take Briar up on her offer, and ask her to sample her famous milk puddings. Briar, a proper lady, only offers her milk puddings to those who make a *sufficiently polite* request—Ada must be polite and say “please”, but she must not overuse it, lest she comes off as begging!

We <a id="Ada-and-Briar"></a>encode the interaction between Ada and Briar using *session types* in Haskell:

- Ada’s requests are represented using the `Request` datatype, which allows us to prefix a request for pudding with any number of uses of `Please`.
- Briar’s response is represented using the `Response` datatype, in which she can either grant permission, in which case Briar sends an `Allow` with a sample of pudding attached, or refuse Ada’s request, in which case she sends a `Deny` with a reason.

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
```

The example illustrates a crucial notions for session types:

Firstly, session types are *communication protocols.* If you glance at the types of the endpoints, you see that they represent the communication protocol from each participants perspective. For instance, Ada’s endpoint says she must send a request, receive a response, and then end the session.

Secondly, the types of the endpoints of a binary channel must be *dual*. When Ada’s endpoint says she must send, Briar’s endpoint says she must receive. For classical multiparty session types, the equivalent notion is called *coherence*, but the principle remains the same.

Finally, each endpoint must be used *exactly once* if we want to be sure to stick to the protocol. For instance, in the code above, each channel endpoint is only used once, and each send or receive returns a new channel on which to continue the communication. If we didn’t, we would be able to write a <a id="cheeky-Ada"></a>cheeky variant of Ada, who simply tries any number of pleases until she gets that sweet, sweet pudding:

```haskell
ada :: Send Request (Recv Response End) -> IO ()
ada chan = tryAll MayIHaveSomePudding chan
  where
    tryAll req chan = do
      chan' <- send req chan
      (resp, chan'') <- recv chan'
      case resp of
        Allow pudding -> putStrLn "I’m so happy!"
        Deny reason -> tryAll (Please req) chan
```

But that’s not what the protocol says! Briar doesn’t have time for more than one request, so after the first one has run its course, Ada whispers her second request into the tin can, then waits forever, pining for a response from Briar which will never come!


# A Bit of a Roadmap

Only a few short years after Ada and Briar enjoyed sweet milk puddings, a man by the name of *Alonzo Church* was born in Washington, D.C., in the United States. Three decades later, in the 1930s, Alonzo developed [the λ-calculus][church1932], a foundational calculus which studies computation using *functions*. To this day, the λ-calculus underpins most theories of functional programming languages. Talk about influential!

Only a few short years after Alonzo developed the λ-calculus, a man by the name of *Robin Milner* was born near Yealmpton, in England. Alonzo lived a long life, over nine decades! A few years before Alonzo’s death in the mid 1990s, Robin, together with Joachim Parrow and David Walker, developed the [π-calculus][milner1992], a foundational calculus which studies concurrent computation by processes using *message-passing communication*. It wasn’t the first process calculus—it itself was heavily influenced by ideas dating back to the early 1980s—but it’s certainly one of the most influential!

We’ll start out by discussing the untyped λ-calculus. It’s a wonderful little language, and it’s *really* powerful. Unfortunately, it has all sorts of programs that do all sorts of bad things, like loop forever, so with all that power, it’s *really scary* too! We’ll then discuss the idea of taming all that scary power using types, to try and get only well-behaved programs, and the challenges of taming it without taking all the *oomph* out.

Then, we’ll switch to discussing the π-calculus. It’s a wonderful little language, even if it’s twice as big as the λ-calculus—with *six* constructs instead of *three*! It’s even more powerful than the λ-calculus—it can express all sorts of concurrent behaviours that the λ-calculus has no hope of expressing. Unfortunately, it’s scarier as well—there’s way more things that can go wrong! Again, we’ll turn our attention to taming all that scary power using types, and the problems of *oomph*’lessness that comes with it.

Finally, we’ll talk about having the best of both worlds, in a concurrent λ-calculus, which is sorta what you get when you smash the λ-calculus and the π-calculus together at high speeds! The concurrent λ-calculus has the best of both worlds: higher-order functions and concurrency with message-passing communication!


# The λ-calculus! *(So powerful, so scary…)*

The untyped λ-calculus celebrated its 89th birthday last November, so to say that it’s been around for a while undersells it a bit. It’s a pretty small system—it has only three things—there’s variables, λ-abstractions to make functions, and function applications to get rid of 'em:

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

There’s only one computation rule—if a function $\lambda x.M$ meets its argument $N$ we replace all occurrences of $x$ in the function body $M$ with the argument $N$.

::: mathpar
$\begin{array}{c}
(\lambda x.M)\;N
\longrightarrow
M\{N/x\}
\end{array}$
:::

That’s not all, though, since we also need to let our calculus know that it’s okay to reduce under a function application. For this, we *could* just write out the following rules:

::: mathpar
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

However, things tends to compose a little better if you use a little trick called *evaluation contexts*. We’ll see an example of *how* evaluation contexts compose better later. Anyway, you write down all the partial terms under which it’s okay to normalise, and then write a single rule… Read $E[M]$ as “replace the single $\square$ in $E$ with $M$”:

::: mathpar
$\begin{array}{l}
\text{Evaluation Context} \; E
\\
\quad
  \begin{array}{rl}
  ::= & \square
  \;\mid\;    E \; N
  \;\mid\;    M \; E
  \end{array}
\end{array}$
$\begin{array}{c}
M
\longrightarrow
M^\prime
\\ \hline
E[ M ]
\longrightarrow
E[ M^\prime ]
\end{array}$
:::

When we choose what to put in our evaluation contexts, we determined where to allow and disallow reduction. For instance, here we’re saying “You’re allowed to reduce function calls and their arguments… but don’t you dare touch the function body before we’re actually calling it!” This is called an *evaluation strategy*, and the one we’re using here is called *call-by-name*. Usually, call-by-name is pretty terrible in terms of efficiency. Imagine you have a pretty expensive computation, and the result of that computation is used twenty times… call-by-name is likely to do the whole computation twenty times! In practice, you’ll want to use call-by-value or call-by-need, but those complicate things, so we’re not using them here!

Where were we? Oh, yes, λ-calculus, powerful, scary… Right! The λ-calculus is *very* powerful—*some stuff about it being a “universal model of computation”*—but that power comes at the cost of also being able to express quite a lot of scary programs that do bad stuff.

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

That’s good—as programmers, we like recursion! Really simplifies your programs, not having to write out the case for every possible input!

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

That’s scary, I’d prefer not to have that! Programs which run forever, but never do a single thing—or worse, programs which are doing things the whole time, but never produce any outputs!

Most functional languages don’t just implement the core λ-calculus, but rather extend the λ-calculus with various constructs—numbers, addition, multiplication, pairs, sums, *etc.* *Technically speaking*, these can all be encoded using just functions—see, *e.g.*, Church encodings—but it tends to be *a lot* more practical and faster to use, *e.g.*, machine numbers.

For example, we can extend the untyped λ-calculus with <a id="peano"></a>Peano numbers. First, we extend the term language with the number *zero*, written $\text{zero}$, the successor, written $\text{suc}$, and a pattern matching construct for numbers, written $\text{case}\;L\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}$:

$$
\begin{array}{l}
\text{Term} \; L, M, N
\\
\quad
  \begin{array}{rl}
  ::= & \dots
  \\\mid &    \text{zero}
  \;\mid\;    \text{suc}\;M
  \\\mid &    \text{case}\;L\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
  \end{array}
\end{array}
$$

Then, we extend the reduction rules with two reduction rules for pattern matches on numbers—depending on whether the number is zero or a successor:

::: mathpar
$\text{case}\;\text{zero}\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
\longrightarrow
M$

$\text{case}\;\text{suc}\;{L}\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
\longrightarrow
N\{L/x\}$
:::

And we shouldn’t forget to extend our evaluation contexts:

$$
\begin{array}{l}
\text{Evaluation Context} \; E
\\
\quad
  \begin{array}{rl}
  ::= & \dots
  \\\mid &    \text{suc}\;E
  \\\mid &    \text{case}\;E\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
  \end{array}
\end{array}
$$

We can now define addition on Peano numbers in our calculus! Ideally, we’d write something like the following, familiar definition for addition:

$$
\begin{array}{lllcl}
\text{plus} & \text{zero} & {N} & = & \text{zero}
\\
\text{plus} & (\text{suc}\;{M}) & {N} & = & \text{suc}\;(\text{plus}\;{M}\;{N})
\end{array}
$$

Our core language doesn’t support recursive or pattern matching definitions, so we’ll have to elaborate the above definition into something less familiar, which uses the $Y$ combinator and the pattern matching construct. It’s a bit more verbose, but it’s addition nonetheless:

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

Woe is us! We have *another* kind of problem! We now have to worry about programs like $\text{plus}\;(\lambda x.x)\;\text{zero}$. What does that even mean?! According to our semantics, it means exactly that, since it doesn’t reduce any further… It’s stuck on the pattern match on $\lambda x.x$, since there’s no case for functions.

Problems like these are less obvious when using, *e.g.*, Church encodings, since everything is just functions. For instance, if we use Church-encoded Peano numbers to compute $\text{plus}_{ch} \; (\lambda x.x) \; \text{zero}_{ch}$, and convert the result to our builtin Peano numbers, we find that adding the identity function to the number *zero* gives us *one*:

$$
\begin{array}{lcl}
\text{zero}_{ch} & \triangleq & \lambda s. \lambda z. z
\\
\text{suc}_{ch}  & \triangleq & \lambda n. \lambda s. \lambda z. s \; (n \; s \; z)
\\
\text{plus}_{ch} & \triangleq & \lambda m. \lambda n. \lambda s. \lambda z. m \; s \; (n \; s \; z)
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

Specifically, for the second problem, we have the choice between programs which get stuck and programs which compute nonsense. If we want programs that misuse data to get stuck, we tag our data—by using, *e.g.*, a syntactically distinct $\text{zero}$ constructor—and check the types of our data at runtime. If we’re fine with the misuse of data, we encode everything as functions, and accept whatever results fall out of our encoding.


# Taming the λ-calculus with types…

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
  \;\mid\; A \to B
  \end{array}
\end{array}
$$

With types in hand, we write down some *typing rules*. The goal is that *if* we can construct a typing derivation for a term, that term will be well-behaved.

Terms are checked in the context of some typing environment, which we’ll refer to with the variable $\Gamma$ or $\Delta$. Typing environments are just a bunch of typing assignments $x : A$. Why do we need typing environments? A variable in a program might stand for some base value, like an int, or it might stand for a function, and it’s kinda important to know ahead of time which one it is—you can’t apply an int, and you can’t add one to a function! We write $x : A \in \Gamma$ to mean that “somewhere in $\Gamma$ there’s the typing assignment $x : A$”.

We write $\Gamma \vdash M : A$ to mean that “using the variables from the typing environment $\Gamma$, the term $M$ has type $A$”—statements like these are called *typing judgements*. But we can’t just *claim* a some term has some time, we need to back it up with some actual proof! For that, we use *inference rules*, which is how most type systems are presented.

Inference rules are like a puzzle. They’re written as…

$$
\begin{array}{c}
\mathit{Premise}_1 \dots \mathit{Premise}_n
\\ \hline
\mathit{Conclusion}
\end{array}
$$

The puzzle you’re trying to solve is:

- Find a piece whose conclusion matches the thing you’re trying to prove;
- Oh no! All those things had premises! You gotta find puzzle pieces whose conclusions match those as well now!
- Keep going until there’s no more open premises! You got this!

For the simply-typed λ-calculus, there are three inference rules, one for each term construct:

::: mathpar
$\begin{array}{c}
{x : A \in \Gamma}
\\ \hline
\Gamma \vdash x : A
\end{array}$
$\begin{array}{c}
\Gamma, x : A \vdash M : B
\\ \hline
\Gamma \vdash \lambda x.M : A \to B
\end{array}$
$\begin{array}{c}
\Gamma \vdash M : A \to B \quad \Gamma \vdash N : A
\\ \hline
\Gamma \vdash M \; N : B
\end{array}$
:::

Oh no! All of these rules have some premises! Does that mean we’re gonna have to puzzle forever? Nope, all it means is that we are *immediately* complicating the puzzle analogy.

If you look at the first rule, the premise isn’t actually a typing judgement… It’s one of those thingies which checks whether or not $x$ has type $A$ in $\Gamma$! There’s a whole separate kind of puzzle for those! One that’s usually left implied, because it’s relatively simple:

::: mathpar
$\begin{array}{c}
\\ \hline
x : A \in x : A, \Gamma
\end{array}$
$\begin{array}{c}
x : A \in \Gamma
\\ \hline
x : A \in y : B, \Gamma
\end{array}$
:::

All those puzzle pieces say is “if you wanna know if $x : A$ is in $\Gamma$… go through all the thing in $\Gamma$ and check if one of ’em is $x : A$.” With those pieces made explicit, our puzzle will have a satisfying “no premise left unproven!” kind of feel to it… Though, mostly, folks just leave the proofs for $x : A \in \Gamma$ implicit, since once you write out $\Gamma$, they’re pretty obvious.

Anyway, back to our typing rules for the λ-calculus! In order of appearance:

- A variable $x$ has type $A$ if there’s an assignment in $\Gamma$ that says so.
- If we’ve got something of type $B$ which uses something of type $A$ from the typing environment, we can abstract over that something to create a function of type $A \to B$.
- If we’ve got something of type $A \to B$ and something of type $A$, then we can apply the former to the latter to get something of type $B$.

Guess what?! It works! All the programs you can type with these rules are super well-behaved and nice! Buuuut… there’s kinda a lot of programs that are really nice and good, that you can’t type with these rules… Very, *very*, notably, you can’t type the $Y$ combinator. Oh no! We lost recursion!

Queue the history of type theory, trying to wrangle with this, trying to make this system more permissive while still keeping lots of the scary stuff out!

It’s, *uh*, pretty hard to get *extactly* the bad looping stuff out, so some folks are like “eh, we’ll keep the looping stuff, but still use types to get rid of all that ‘adding functions to numbers’ nonsense”, whereas other folks are all hardcore and decide that “no it has to be terminating all the way even if it becomes pretty hard to use!”


# A detour into linearity!

Let’s briefly talk about another type system for the λ-calculus—but only because it’ll turn out to be highly relevant to session types, I haven’t forgotten what I promised to write about! Let’s talk about [the linear λ-calculus][wadler1993].

In its most minimal form, the linear λ-calculus demands that every variable is used *exactly once*. This’ll end up being a *very important* restriction for our session-typed calculus. Remember that cheeky implementation for Ada, which kept sending new and new requests for milk pudding, even though the protocol *clearly* stated she could only send one request? That’s where the *used exactly once* restriction comes in.

Okay, so how are we going to enforce this in the type system? When you check a function application, you have to decide which parts of the typing environment are gonna be used in the function, and which parts in the argument. By the time you’ve made it all the way down to a variable, the typing environment is supposed to be empty save for the variable you’re checking. Everything else must’ve already been split off for usage elsewhere.

Also, we now use this cute little lollipop instead of the function arrow:

$$
\begin{array}{l}
\text{Type} \; A, B, C
\\
\quad
  \begin{array}{rl}
  ::= & \star
  \;\mid\; A \multimap B
  \end{array}
\end{array}
$$

::: mathpar
$\begin{array}{c}
\\ \hline
x : A \vdash x : A
\end{array}$
$\begin{array}{c}
\Gamma, x : A \vdash M : B
\\ \hline
\Gamma \vdash \lambda x.M : A \multimap B
\end{array}$

$\begin{array}{c}
\Gamma \vdash M : A \multimap B \quad \Delta \vdash N : A
\\ \hline
\Gamma, \Delta \vdash M \; N : B
\end{array}$
:::

In order of appearance:

- A variable $x$ has type $A$ if the typing environment *only* contains $x : A$.
- If we’ve got something of type $B$ which uses something of type $A$ from the typing environment, we can abstract over that something to create a function of type $A \multimap B$. *(It’s the same as before!)*
- If we’ve got something of type $A \multimap B$ which uses some chunk of the typing environment called $\Gamma$, and something of type $A$ which uses the rest of the typing environment called $\Delta$, then we can apply the former to the latter to get something of type $B$ which uses both $\Gamma$ and $\Delta$.

Notice that $A \multimap B$ being a linear function isn’t something that follows from the rules for abstraction and application… it’s something to do with the structure of *all rules*.

As a type system, this is *highly restrictive*. Essentially, what we’re left with is a calculus of permutations. Think of lists… if you’re writing a function from lists to lists, but you *have to* use every element in the list exactly once, what kinds of programs can you write? Permutations. That’s it.


# The π-calculus! *(Is even scarier…)*

Oof, that was a bit of a detour, wasn’t it? Wanna talk about *session types*, the thing that I promised I’d talk about? Okay, let’s do it! The π-calculus is pretty young—it didn’t show up until 1992, though it’s heavily influenced by ideas dating back to the 1980s. Unlike with the λ-calculus, there’s not really a *canonical* π-calculus that everyone agrees on, so the one I’m presenting here is just kinda the version that I felt like presenting.

It’s also pretty big! It’s got twice as many *things* in it as the λ-calculus. Instead of functions, we’re talking about processes, which are built using *six* different constructors:

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

- We’ve got ν-binders, written $(\nu x)P$, which creates a new channel $x$, which can be used in $P$. (That ν is the Greek letter nu, which sure sounds a lot like “new”. It’s, *like*, the only well-chosen Greek letter we use in programming language theory.)
- We’ve got parallel composition, written $\parallel$, to let you know that two processes are running in parallel.
- We’ve got nil, written $0$, the process which is done.
- We’ve got *send*, written $x \langle y \rangle.P$, which sends some $y$ on $x$, and then continues as $P$.
- We’ve got *receive*, written $x ( y ).P$, which receives some value on $x$, names it $y$, and then continues as $P$.
- We’ve got replication, written $! P$, which represents a process $P$ which is replicated an arbitrary number of times.

Replication isn’t truly *essential* to the π-calculus, it’s just that we can’t do any sort of *infinite* behaviour with just sending and receiving, so we have to add it explicitly. Other solutions, like adding recursive definitions, work as well.

There’s only one computation rule—if we’ve got a send and a receive in parallel, we perform the communication, and replace all instances of the name bound by the receive instruction by the actual value sent:

::: mathpar
$\begin{array}{c}
x\langle{y}\rangle.{P}\parallel x(z).{Q}
\longrightarrow
{P}\parallel{Q}\{y/z\}
\end{array}$
:::

Plus our usual trick to let us reduce under parallel compositions and ν-binders:

::: mathpar
$\begin{array}{l}
\text{Evaluation Context} \; G
\\
\quad
  \begin{array}{rl}
  ::= & \square
  \;\mid\;    (\nu{x}){G}
  \;\mid\;    ({G}\parallel{Q})
  \;\mid\;    ({P}\parallel{G})
  \end{array}
\end{array}$
$\begin{array}{c}
{P}
\longrightarrow
{P}^\prime
\\ \hline
G[ P ]
\longrightarrow
G[ P^\prime ]
\end{array}$
:::

However, these rules in and of themselves are not enough. You see, a parallel composition $P \parallel Q$ isn’t intended to be *ordered*—I mean, if you’ve got two processes in parallel, does it make sense to say that one of them is “to the left of” the other?—but we haven’t told the reduction semantics about that. That means that with the rules we’ve given so far, we cannot reduce the following:

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
  & \text{if}\;{x}\not\in{P}
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

The reason we’re embedding it this way, with a reduction step sandwiched between two equivalence, is because the equivalence relation isn’t super well-behaved—there’s plenty of infinite chains of rewrite rules, *e.g.*, imagine swapping $P \parallel Q$ back and forth forever, or duplicating $! P$ forever—and we’d prefer not to have any infinite chains of reductions. Embedding it this way forces there to be at least one *real* computation step in each reduction step, because the only way to construct a reduction is to start with a computation.

If you thought the λ-calculus had problems, have I got news for you. There’s all the old problems we had with the lambda calculus. We’ve got processes that reduce forever without doing anything:

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

Plus, if we add numbers, we could try to send over the number 5, foolishly assuming it’s a channel.

But *what fun*! There’s new problems as well! If we’ve got two *pairs* of processes, both of which are trying to communicate over $x$ at the same time, then reduction is not longer *deterministic*—it’s anyone’s guess which message will end up where! Yaaay, it’s *race conditions*:

$$
\begin{array}{l}
  (\nu x)
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
  & (\nu x)
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
  & (\nu x)
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

Another *fun* thing we can do is write two processes which echo a message on $x$—they receive something and send it back… but the twist is, they’re both wanting to *receive* first! Ack, it’s a *deadlock*:

$$
\begin{array}{c}
  (\nu x)
  \left(
  x(y).x\langle{y}\rangle.0 \parallel x(z).x\langle{z}\rangle.0
  \right)
  \not\longrightarrow
\end{array}
$$

To be fair, it’s not surprising that race conditions and deadlocks show up in a foundational calculus for *concurrency*—it’d be weird if they didn’t. But that does mean that as programming languages people, we now have two new problems to worry about! To summarise, we’ve identifier *four* main problems with the untyped π-calculus as a foundation for programming languages:

1. it has programs which loop forever but produce nothing;
2. it has no way of making sure that data is used as intended;
3. it has *race conditions*; and
4. it has *deadlocks*.


# Taming the π-calculus with types…

Oh dear, so many problems to solve. Where do we begin?

It may help to think a little deeper about the latter three problems. In a sense, we could see a deadlock as the consequence of us not using a channel as intended. After all, we probably intended for one party to be sending while the other was receiving. We could see a race condition in a similar light. We intended for the two pairs of processes to communicate in a *predictable* pattern, in pairs of two.

These are overly simplistic descriptions—sometimes we truly don’t care about the order of messages, and a bit of a race is fine. However, much like with the λ-calculus, we’re gonna try and cut all the bad stuff out first, no matter what cost, and then get to the business recovering what we lost.

Let’s have a look at *session types*, invented in the early 1990s by [Kohei Honda][honda1993], and let’s focus first and foremost on one important property—*session fidelity*. Essentially, it means that we communicate over a channel as intended by its protocol—or *session type*.

Let’s start with the simplest system, where there’s only two things we can do with a channel—send and receive. That’ll be our language of session types—either we send on a channel, we receive from a channel, or we’re done:

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

A crucial notion in session types—as mentioned in the introduction—is *duality*, the idea that while I’m sending a message, you should be expecting to receive one, and vice versa. Duality is a function on session types. We write duality using an *overline*, so the dual of $S$ is $\overline{S}$:

$$
\begin{array}{lrl}
\overline{!S.S'}        & = & ?S.\overline{S'}
\\
\overline{?S.S'}        & = & !S.\overline{S'}
\\
\overline{\mathbf{end}} & = & \mathbf{end}
\end{array}
$$

Finally, before we get to the typing rules, we’re gonna make one tiny tweak to the syntax for processes. Before, our ν-binders introduced a *channel name*, which any process could then use to communicate on. Now, ν-binders introduce a channel by its two *channel endpoint names*. It’s gonna make the typing rules a *bunch* easier to write down:

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

We’ve also gotta propagate this changes through the structural congruence and the reduction rules. It’s pretty straightforward for most of the changes—just replace $(\nu x)$ with $(\nu x x')$—but there’s one extra change we have to make to the computation rule, which is a bit of a shift in perspective. See, in our first iteration of the π-calculus, processes were connected by virtue of having access to the same *channel name*, and ν-binders were merely a convenience to let us hide channel names from the outside world. However, in our current version, processes use *unrelated* endpoint names, and it’s the ν-binder who connects them to form a channel. Or—if you will—each participant is just holding a tin can, and they can’t be used to communicate until they’re bound together with twine. That means that we’ll have to *require* that reduction takes place under a ν-binder:

$$
(\nu x x')(x\langle{y}\rangle.{P}\parallel x'(z).{Q})
\longrightarrow
(\nu x x')({P}\parallel{Q}\{y/z\})
$$

Okay, typing rules! One thing that’s very different from the λ-calculus is that the typing rules only check whether processes use channels correctly—the processes themselves don’t have types. There are six rules, corresponding to the six process constructs:

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

This type system is *linear*, much like the linear λ-calculus we saw earlier. Told you it’d be relevant! Anyway, it’s not linear in quite the same way, since channels can be used *multiple times*. However, each step in the protocol has to be executed *exactly once*!

So, did it work? Are we safe from the bad programs? Yes and no. Which, *uh*, kinda just means no? But there’s *some* bad programs we got rid of! There are no longer programs which misuse data, since everything strictly follows protocols. Since every channel has *exactly two* processes communicating over it, we no longer have any race conditions. Furthermore, because those two processes must act *dually* on the channel, we no longer have any deadlocks *within a single session*—that is to say, as long as each two processes only share *one* means of communication, we don’t have any deadlocks. Unfortunately, it’s quite easy to write a program which interleaves *two* sessions and deadlocks:

$$
\begin{array}{c}
  (\nu x x')(\nu y y')
  \left(
  x(z).y\langle{z}\rangle.0 \parallel y'(w).x'\langle{w}\rangle.0
  \right)
  \not\longrightarrow
\end{array}
$$

We also don’t have looping programs anymore, but, *uh*, that’s mostly because we removed replication, so… win?

How do we get rid of those last few deadlocks? The ones caused by having multiple open lines of communication with another process? There’s several different ways to do this, and they all have their advantages and disadvantages:

The first option, used in, *e.g.*, the logic-inspired session type systems by [Luís Caires and Frank Pfenning][caires2012] and [Philip Wadler][wadler2014], is to just say “The problem happens when you’ve got multiple open lines of communication with another process? Well, have you considered just not doing that?” Essentially, these type systems require that the *communication graph is acyclic*—what that means is that, if you drew all the participants, and then drew lines between each two participants who share a channel, you wouldn’t draw any cycles. This works! Can’t share multiple channels if you don’t share multiple channels, am I right? But it’s a *wee bit* restrictive… Got some homework about [forks and hungry, hungry philosophers][dining-philosophers] you need to do? Nope. Wanna write a neat cyclic scheduler? Not for you. However, it has some *nice* aspects too—it composes! Got two deadlock-free programs? Well, you can put ’em together, and it’s gonna be a deadlock-free program! If we updated our typing rules, they’d look a little like this:

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

The second option, developed by [Naoki Kobayashi][kobayashi2002], is to just do a whole-program check for deadlocks. Take out a piece of paper, and draw a blob for every dual pair of send and receive actions in your program. For every action, if it *has to* happen before another action, draw an arrow between their blobs. Finally, check to see if there’s any *directed* cycles—for each blob, see if you can follow the arrows and end up back at the same blob. We can do this *formally* by adding some little blobs to our session types—since each session type connective corresponds to an action—and requiring a particular order on the little blobs. For reference, little blobs are more commonly known as *priorities*. If we updated our typing rules, they’d look a little like this… first, we add little blobs to our session types. Duality *preserves* priorities:

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

We also define a function, $\text{Pr}$, to nab the topmost priority from a session type. If we apply $\text{Pr}$ to a $\Gamma$ we mean to get the *smallest* priority of all types in $\Gamma$—actions with smaller priorities happen earlier, so we essentially wanna know when the *first* action in $\Gamma$ is gonna happen.

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
o < \text{Pr}{(\Gamma, x : B)}
\\ \hline
\Gamma, x : {!^o}A.B, y : A \vdash x\langle{y}\rangle.P
\end{array}$

$\begin{array}{c}
\Gamma, y : A, x : B \vdash P
\quad
o < \text{Pr}{(\Gamma, y : A, x : B)}
\\ \hline
\Gamma, x : {?^o}A.B \vdash x(y).P
\end{array}$
:::

We’re enforcing two things here:

1. If we write $x(y).P$, then the action $x(y)$ must happen before *everything else in* $P$. Similarly for $x\langle{y}\rangle.P$.
2. If we connect two endpoints, $(\nu x x')$, then the dual actions on those endpoints must each happen *at the same time*.

There’s a really nice recent example of a session type system which uses this technique by [Ornela Dardha and Simon Gay][dardha2018]. The upside of this technique is that you can have all sorts of neat cyclic communication graphs, and still rest assured knowing that they don’t do anything scary. The downside is that it’s a *whole-program* check, meaning that if you’ve got two deadlock-free programs, and you put ’em together, you have to check again, to see that you didn’t introduce any deadlocks.

Anyway, we’ve now got a *mostly* safe foundation for session types! You don’t see type systems *this* simple touted in papers much—or *at all*, as far as I’m aware. The reason is probably that they’re kinda *too simple*. Just like with the *purely* linear λ-calculus, there’s not much you can actually compute with these programs, and you have to put in a little bit of work before you can actually get to a point where you get back enough expressivity to be taken seriously as a programming language. However, I thought it would be illustrative to discuss the *simplest possible* type systems.

Once we’ve got this foundation, we can get to work extending it! With a little bit of effort we could add branching, replication, recursion, several forms of shared state, polymorphism, higher-order processes, *etc.* However, figuring out how all this stuff works in the context of the π-calculus seems like a bit of a waste, especially when we already know how they work within the context of the λ-calculus. Plus, doesn’t that “adding higher-order processes” thing sound suspiciously like adding higher-order functions?


# Concurrent λ-calculus *(λ and π, together forever)*

Okay, this will be the *final* computational model I’m introducing in this by now rather long blog post, I promise! The point of concurrent λ-calculus, in short, is this: the π-calculus is great for modelling concurrency, but it’s a rather, *uh*, unpleasant language to actually write programs in, and instead of figuring out how to make it a nice language, why not just smash it together with a thing that we already know and love!

So, here’s the plan: we’re gonna start with the λ-calculus as a model of *sequential* computation, and then we’re gonna add a smattering of π-calculus processes on top as a model of sequential computations running *concurrently*.

First off, we’re going to take our λ-calculus terms, and extend then with some constants $K$, which will be our concurrency primitives. We’re also going to add *units* and *pairs*—we’ll need them in a bit! We write $()$ for the unit value and $\mathbf{let} \; () = M \; \mathbf{in} \; N$ for pattern matching on units. We write $(M, N)$ for the pair of $M$ and $N$ and $\mathbf{let} \; (x, y) = M \; \mathbf{in} \; N$ for pattern matching on pairs.

Second, we’re going to wrap those terms up in π-calculus processes—we’ll have ν-binders and parallel compositions, and threads which run terms. There’s two kinds of threads—*main* threads, written $\bullet M$, and *child* threads, written $\circ M$. Why? Well, we expect functional programs to produce a value, but processes just send and receive things, and then stop. Programs in our concurrent λ-calculus are going to have *exactly one* main thread, which is going to compute a value. Child threads will send and receive, but eventually compute the unit value:

::: mathpar
$\begin{array}{l}
  \begin{array}{l}
  \text{Term} \; L, M, N
  \\
  \quad
    \begin{array}{rl}
    ::=    & x \mid \lambda x.M \mid M \; N
    \\\mid & ()
    \\\mid & \mathbf{let} \; x = M \; \mathbf{in} \; N
    \\\mid & K
    \end{array}
  \end{array}
\end{array}$
$\begin{array}{l}
\text{Process}\;{P},{Q},{R}
\\
\quad
  \begin{array}{rl}
     ::= & (\nu xx'){P}
  \\\mid & ({P}\parallel{Q})
  \\\mid & \phi M
  \end{array}
\end{array}$

$\begin{array}{l}
\text{Const} \; K
\\
\quad
  \begin{array}{rlcl}
     ::= & \mathbf{send} & \mid & \mathbf{recv}
  \\\mid & \mathbf{new}  & \mid & \mathbf{spawn}
  \end{array}
\end{array}$
$\text{Flag} \; \phi ::= \bullet \mid \circ$
:::

So what are the semantics of our calculus going to be?

First off, we just keep the reduction rules for our λ-calculus—and we’re just gonna sneak those rules for units and pairs on in there while you’re distracted. We’re gonna call this reduction arrow $\longrightarrow_M$, to distinguish it from the reduction on processes:

::: mathpar
$\begin{array}{c}
(\lambda x.M)\;N
\longrightarrow_M
M\{N/x\}
\end{array}$

$\begin{array}{c}
\mathbf{let} \; () = () \; \mathbf{in} \; N
\longrightarrow_M
N
\end{array}$

$\begin{array}{c}
\mathbf{let} \; (x, y) = (L, M) \; \mathbf{in} \; N
\longrightarrow_M
N\{L/x\}\{M/y\}
\end{array}$
:::

Let’s not forget our usual “evaluation context” shenanigans:

::: mathpar
$\begin{array}{l}
\text{Evaluation Context} \; E
\\
\quad
  \begin{array}{rl}
  ::= & \square
  \\\mid &    E \; N
  \;\mid\;    M \; E
  \\\mid &    \mathbf{let} \; () = E \; \mathbf{in} \; N
  \\\mid &    (E, N)
  \;\mid\;    (M, E)
  \\\mid &    \mathbf{let} \; (x, y) = E \; \mathbf{in} \; N
  \end{array}
\end{array}$

$\begin{array}{c}
M
\longrightarrow_M
M^\prime
\\ \hline
E[ M ]
\longrightarrow_M
E[ M^\prime ]
\end{array}$
:::

We’re also going to just copy over the structural congruence from the π-calculus, best as we can—the, *uh*, terminated process has disappeared, and in it’s place we’ve now got child threads which are done, *i.e.*, $\circ()$, so it’s a *little* different, but otherwise, pretty much the same:

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
  P \parallel \circ()
  & \equiv
  & P
  \\
  (\nu x x')P
  & \equiv
  & P
  & \text{if}\;{x},{x'}\not\in{P}
  \\
  (\nu x x')(\nu y y')P
  & \equiv
  & (\nu y y')(\nu x x')P
  \\
  (\nu x x')(P \parallel Q)
  & \equiv
  & (\nu x x')P \parallel Q,
  & \text{if}\;{x},{x'}\not\in{Q}
\end{array}
$$

Then we copy over our rules from the π-calculus… but what’s this! We don’t have send and receive in the process language anymore? They’re all awkwardly wedged into the term language now… So terms reduce, and at some point, they get stuck on a $\mathbf{send}$ or $\mathbf{recv}$ that they don’t know how to deal with, and then the π-calculus rules have to take over. Our first instinct might be to write something like…

::: mathpar
$\begin{array}{c}
(\nu x x^\prime)(\phi\;\mathbf{send}\;{M}\;{x} \parallel \phi^\prime\;\mathbf{recv}\;{x^\prime})
\\ \\
\downarrow
\\ \\
(\nu x x^\prime)(\phi\;() \parallel \phi^\prime\;{M})
\end{array}$
:::

…which sure looks a lot like our π-calculus rule. Unfortunately, it only captures top-level $\mathbf{send}$ or $\mathbf{recv}$ operations… and usually, we’ll want to bind and actually use the value we receive! Plus, we actually lose access to the channel this way… our intention is that each endpoint can only be used once, and that $\mathbf{send}$ and $\mathbf{recv}$ return the endpoint for the next step in the session along with any potential result… Now, we *could* discard the old channel, create a new one, and return it… and in an implementation, this may well be what you do… but here? Why would we? We’ve got a perfectly good channel, with two endpoints we know aren’t going to be used anyway… so why not just return the same channel?

::: mathpar
$\begin{array}{c}
(\nu x x^\prime)(\phi\;E[ \mathbf{send}\;{M}\;{x} ] \parallel \phi^\prime\;E^\prime[ \mathbf{recv}\;{x^\prime} ])
\\ \\
\downarrow
\\ \\
(\nu x x^\prime)(\phi\;E[ x ] \parallel \phi^\prime\;E^\prime[ (M, x^\prime) ])
\end{array}$
:::

This is a great example of why evaluation contexts compose better! Try and write this rule without them! Oh, yeah, almost forgot! We’ve still gotta add evaluation contexts for the process language, plus that thing where we tell reduction that it’s okay to rewrite using our structural congruence, and a new rule where we tell π-calculus reduction that it’s okay for them to use the λ-calculus rules as well:

::: mathpar
$\begin{array}{l}
\text{Evaluation Context} \; G
\\
\quad
  \begin{array}{rl}
  ::= & \square
  \;\mid\;    (\nu{x x'}){G}
  \;\mid\;    ({G}\parallel{Q})
  \;\mid\;    ({P}\parallel{G})
  \end{array}
\end{array}$

$\begin{array}{c}
{P}
\longrightarrow
{P}^\prime
\\ \hline
G[ P ]
\longrightarrow
G[ P^\prime ]
\end{array}$
$\begin{array}{c}
  P \equiv P^\prime \quad P^\prime \longrightarrow Q^\prime \quad Q^\prime \equiv Q
  \\ \hline
  P \longrightarrow Q
\end{array}$
$\begin{array}{c}
  M \longrightarrow_{M} M^\prime
  \\ \hline
  \phi\;M \longrightarrow \phi\;M^\prime
\end{array}$
:::

We’re *almost done!* You might’ve noticed that we’ve got $\mathbf{new}$ and $\mathbf{spawn}$ in our term language *as well as* ν-binders and parallel compositions in our process language. That’s seems kinda redundant, doesn’t it? We’d like to think of the *terms* as the programs that we actually write, and of the *processes* as modelling the configuration of threads and shared channels created while *running those programs*. So the $\mathbf{new}$ and $\mathbf{spawn}$ functions really just mean “create a ν-binder” and “create a parallel composition” on the process level! So we’ve got to add two more rules:

::: mathpar
$\begin{array}{l}
  \phi\;E[ \mathbf{new} ]
  \longrightarrow
  (\nu x x')(\phi\;E[ (x, x') ])
\end{array}$
$\begin{array}{l}
  \phi\;E[ \mathbf{spawn}\;{M} ]
  \longrightarrow
  \phi\;E[ () ] \parallel \circ\;{M}\;()
\end{array}$
:::

Oof, we’ve done it! We’ve got the whole reduction system!


# Two Victorian Ladies *(More Formal, Somehow?)*

Our formal concurrent λ-calculus is getting pretty close to being able to encode the interaction between [Ada and Briar](#Ada-and-Briar)! Remember that, like a billion words ago? There’s two problems left, if we want to encode our example:

1. Ada prints a string, but we don’t really have strings or, *uh*, the ability to print strings.
2. Ada and Briar send values of data types back and forth, but we don’t have *data types*.

For the first one, we’re just gonna take $\mathbf{putStrLn}$ and strings as primitives in our calculus, with no associated behaviour, and if our programs reduce to, *e.g.*, $\mathbf{putStrLn}\;\text{``Hello, Ada!''}$ we’ll say that’s fine.

For the second one, *well*, we can make this work without having to add full-fledged data types to our language. See, the request data type… it essentially encodes the number of *pleases*, right? It’s kinda like [Peano numbers](#peano), where `MayIHaveSomePudding` is $\mathbf{zero}$ and `Please` is the successor $\mathbf{suc}$. Remember that from the first section? And the `Response`? Well, Ada doesn’t actually use the pudding *or* the reason, so that’s pretty much just a Boolean… and those should be relatively easy to add! First, we extend our terms:

$$
\begin{array}{l}
\text{Term} \; L, M, N
\\
\quad
  \begin{array}{rl}
  ::= & \dots
  \\\mid &    \mathbf{true}
  \;\mid\;    \mathbf{false}
  \;\mid\;    \mathbf{if}\;L\;\mathbf{then}\;M\;\mathbf{else}\;N
  \end{array}
\end{array}
$$

Then, we extend the reduction rules with two reduction rules for if-statements—one for when it’s true, and one for when it’s false:

::: mathpar
$\mathbf{if}\;\mathbf{true}\;\mathbf{then}\;M\;\mathbf{else}\;N \longrightarrow M$

$\mathbf{if}\;\mathbf{false}\;\mathbf{then}\;M\;\mathbf{else}\;N \longrightarrow N$
:::

And we extend our evaluation contexts:

$$
\begin{array}{l}
\text{Evaluation Context} \; E
\\
\quad
  \begin{array}{rl}
  ::= & \dots
  \\\mid &    \text{if}\;E\;\text{then}\;M\;\text{else}\;N
  \end{array}
\end{array}
$$

Great! <a id="formal-Ada-and-Briar"></a> Now we can encode our example!

$$
\begin{array}{l}
\text{ada} \triangleq \lambda a.
\\
\quad
  \begin{array}{l}
  \mathbf{let}\;a^\prime = \mathbf{send}\;(\mathbf{suc}\;\mathbf{zero})\;a\;\mathbf{in}
  \\
  \mathbf{let}\;(x,a^{\prime\prime}) = \mathbf{recv}\;a^\prime\;\mathbf{in}
  \\
  \!\!\!
  \begin{array}{lll}
    \mathbf{if}\;x
       & \!\!\!\!\mathbf{then}\!\!\!\! & \mathbf{putStrLn}\;\text{``I'm so happy!''}
    \\ & \!\!\!\!\mathbf{else}\!\!\!\! & \mathbf{putStrLn}\;\text{``Woe is me!''}
  \end{array}
  \end{array}
\end{array}
$$

$$
\begin{array}{l}
\text{briar} \triangleq \lambda b.
\\
\quad
  \begin{array}{l}
  \mathbf{let}\;(x,b^\prime) = \mathbf{recv}\;b\;\mathbf{in}
  \\
  \mathbf{let}\;y = \mathbf{case}\;x\;\mathbf{of}
  \\
  \quad
    \begin{array}{l}
    \{ \; \text{zero} \mapsto \mathbf{false}
    \\
    ;  \; \text{suc}\;{x^\prime} \mapsto \mathbf{case}\;{x^\prime}\;\mathbf{of}
    \\
    \quad
      \begin{array}{l}
      \{ \; \text{zero} \mapsto \mathbf{true}
      \\
      ;  \; \text{suc}\;{x^{\prime\prime}} \mapsto \mathbf{false}
      \\
      \}
      \end{array}
    \\
    \}
    \end{array}
  \\
  \mathbf{let} \; b^{\prime\prime} = \mathbf{send}\;{y}\;{b^\prime} \; \mathbf{in}
  \\
  ()
  \end{array}
\end{array}
$$

And let’s put it all together in a single $\text{main}$ process. We’ve not done this so far, since it was *hopefully* pretty clear how Ada and Briar were meant to share a channel, but if we want to actually evaluate our processes, we’ll have to create a channel to connect Ada and Briar:

$$
\begin{array}{l}
\text{main} \triangleq
\\
\quad
  \begin{array}{l}
  \mathbf{let} \; (a,b) = \mathbf{new} \; \mathbf{in}
  \\
  \mathbf{let} \; () = \mathbf{spawn} \; (\text{briar} \; b) \; \mathbf{in}
  \\
  \text{ada} \; a
  \end{array}
\end{array}
$$

Right, let’s see if our encoding does what we think it should do! I’m gonna spare no detail, so, *uh*, very long series of evaluation steps coming up.

$$
\begin{array}{c}
  \begin{array}{l}
  \mathbf{let} \; (a,b) = \mathbf{new} \; \mathbf{in}
  \\
  \mathbf{let} \; () = \mathbf{spawn} \; (\text{briar} \; b) \; \mathbf{in}
  \\
  \text{ada} \; a
  \end{array}
\\ \\
\downarrow
\\ \\
  (\nu a b)
  \left(
  \bullet\;
  \begin{array}{l}
  \mathbf{let} \; (a,b) = (a,b) \; \mathbf{in}
  \\
  \mathbf{let} \; () = \mathbf{spawn} \; (\text{briar} \; b) \; \mathbf{in}
  \\
  \text{ada} \; a
  \end{array}
  \right)
\\ \\
\downarrow
\\ \\
  (\nu a b)
  \left(
  \bullet\;
  \begin{array}{l}
  \mathbf{let} \; () = \mathbf{spawn} \; (\text{briar} \; b) \; \mathbf{in}
  \\
  \text{ada} \; a
  \end{array}
  \right)
\\ \\
\downarrow
\\ \\
  (\nu a b)
  \left(
  \begin{array}{l}
  \bullet\;
    \begin{array}{l}
    \mathbf{let} \; () = () \; \mathbf{in}
    \\
    \text{ada} \; a
    \end{array}
    \parallel
  \\
  \circ\;
    \begin{array}{l}
    \text{briar} \; b
    \end{array}
  \end{array}
  \right)
\\ \\
\downarrow
\\ \\
  (\nu a b)
  \left(
  \bullet\;
    \text{ada} \; a
    \parallel
  \circ\;
    \text{briar} \; b
  \right)
\end{array}
$$

Whew, so far so good! The $\mathbf{new}$ and $\mathbf{spawn}$ have done their jobs, and created a channel and a parallel composition! We’re right down to Ada and Briar now! Things are about to get messy! (We’ll sometimes write $\downarrow_n$ when we’re doing multiple steps at a time, either because Ada and Briar are computing things in parallel, or because I didn’t feel like writing the whole thing out.)

$$
\begin{array}{c}
  (\nu c c^\prime)
  \left(
  \bullet\;
    \text{ada} \; c
    \parallel
  \circ\;
    \text{briar} \; c^\prime
  \right)
\\ \\
\downarrow_2
\\ \\
  (\nu a b)
  \left(
  \begin{array}{l}
  \bullet\;
    \begin{array}{l}
    \mathbf{let}\;a^\prime = \mathbf{send}\;(\mathbf{suc}\;\mathbf{zero})\;a\;\mathbf{in}
    \\
    \mathbf{let}\;(x,a^{\prime\prime}) = \mathbf{recv}\;a^\prime\;\mathbf{in}
    \\
    \!\!\!
    \begin{array}{lll}
      \mathbf{if}\;x
         & \!\!\!\!\mathbf{then}\!\!\!\! & \mathbf{putStrLn}\;\text{``I'm so happy!''}
      \\ & \!\!\!\!\mathbf{else}\!\!\!\! & \mathbf{putStrLn}\;\text{``Woe is me!''}
    \end{array}
    \end{array}
    \parallel
  \\ \\
  \circ\;
    \begin{array}{l}
    \mathbf{let}\;(x,b^\prime) = \mathbf{recv}\;b\;\mathbf{in}
    \\
    \mathbf{let}\;y = \mathbf{case}\;x\;\mathbf{of}
    \\
    \quad
      \begin{array}{l}
      \{ \; \text{zero} \mapsto \mathbf{false}
      \\
      ;  \; \text{suc}\;{x^\prime} \mapsto \mathbf{case}\;{x^\prime}\;\mathbf{of}
      \\
      \quad
        \begin{array}{l}
        \{ \; \text{zero} \mapsto \mathbf{true}
        \\
        ;  \; \text{suc}\;{x^{\prime\prime}} \mapsto \mathbf{false}
        \\
        \}
        \end{array}
      \\
      \}
      \end{array}
    \\
    \mathbf{let} \; b^{\prime\prime} = \mathbf{send}\;{y}\;{b^\prime} \; \mathbf{in}
    \\
    ()
    \end{array}
  \end{array}
  \right)
\\ \\
\downarrow
\\ \\
  (\nu a b)
  \left(
  \begin{array}{l}
  \bullet\;
    \begin{array}{l}
    \mathbf{let}\;a^\prime = a \;\mathbf{in}
    \\
    \mathbf{let}\;(x,a^{\prime\prime}) = \mathbf{recv}\;a^\prime\;\mathbf{in}
    \\
    \!\!\!
    \begin{array}{lll}
      \mathbf{if}\;x
         & \!\!\!\!\mathbf{then}\!\!\!\! & \mathbf{putStrLn}\;\text{``I'm so happy!''}
      \\ & \!\!\!\!\mathbf{else}\!\!\!\! & \mathbf{putStrLn}\;\text{``Woe is me!''}
    \end{array}
    \end{array}
    \parallel
  \\ \\
  \circ\;
    \begin{array}{l}
    \mathbf{let}\;(x,b^\prime) = (\mathbf{suc}\;\mathbf{zero},b)\;\mathbf{in}
    \\
    \mathbf{let}\;y = \mathbf{case}\;x\;\mathbf{of}
    \\
    \quad
      \begin{array}{l}
      \{ \; \text{zero} \mapsto \mathbf{false}
      \\
      ;  \; \text{suc}\;{x^\prime} \mapsto \mathbf{case}\;{x^\prime}\;\mathbf{of}
      \\
      \quad
        \begin{array}{l}
        \{ \; \text{zero} \mapsto \mathbf{true}
        \\
        ;  \; \text{suc}\;{x^{\prime\prime}} \mapsto \mathbf{false}
        \\
        \}
        \end{array}
      \\
      \}
      \end{array}
    \\
    \mathbf{let} \; b^{\prime\prime} = \mathbf{send}\;{y}\;{b^\prime}\;\mathbf{in}
    \\
    ()
    \end{array}
  \end{array}
  \right)
\\ \\
\downarrow_2
\\ \\
  (\nu a b)
  \left(
  \begin{array}{l}
  \bullet\;
    \begin{array}{l}
    \mathbf{let}\;(x,a^{\prime\prime}) = \mathbf{recv}\;a\;\mathbf{in}
    \\
    \!\!\!
    \begin{array}{lll}
      \mathbf{if}\;x
         & \!\!\!\!\mathbf{then}\!\!\!\! & \mathbf{putStrLn}\;\text{``I'm so happy!''}
      \\ & \!\!\!\!\mathbf{else}\!\!\!\! & \mathbf{putStrLn}\;\text{``Woe is me!''}
    \end{array}
    \end{array}
    \parallel
  \\ \\
  \circ\;
    \begin{array}{l}
    \mathbf{let}\;y = \mathbf{case}\;\mathbf{suc}\;\mathbf{zero}\;\mathbf{of}
    \\
    \quad
      \begin{array}{l}
      \{ \; \text{zero} \mapsto \mathbf{false}
      \\
      ;  \; \text{suc}\;{x^\prime} \mapsto \mathbf{case}\;{x^\prime}\;\mathbf{of}
      \\
      \quad
        \begin{array}{l}
        \{ \; \text{zero} \mapsto \mathbf{true}
        \\
        ;  \; \text{suc}\;{x^{\prime\prime}} \mapsto \mathbf{false}
        \\
        \}
        \end{array}
      \\
      \}
      \end{array}
    \\
    \mathbf{let} \; b^{\prime\prime} = \mathbf{send}\;{y}\;b\;\mathbf{in}
    \\
    ()
    \end{array}
  \end{array}
  \right)
\\ \\
\downarrow_2
\\ \\
  (\nu a b)
  \left(
  \begin{array}{l}
  \bullet\;
    \begin{array}{l}
    \mathbf{let}\;(x,a^{\prime\prime}) = \mathbf{recv}\;a\;\mathbf{in}
    \\
    \!\!\!
    \begin{array}{lll}
      \mathbf{if}\;x
         & \!\!\!\!\mathbf{then}\!\!\!\! & \mathbf{putStrLn}\;\text{``I'm so happy!''}
      \\ & \!\!\!\!\mathbf{else}\!\!\!\! & \mathbf{putStrLn}\;\text{``Woe is me!''}
    \end{array}
    \end{array}
    \parallel
  \\ \\
  \circ\;
    \begin{array}{l}
    \mathbf{let}\;y = \mathbf{true}
    \\
    \mathbf{let} \; b^{\prime\prime} = \mathbf{send}\;{y}\;b\;\mathbf{in}
    \\
    ()
    \end{array}
  \end{array}
  \right)
\\ \\
\downarrow
\\ \\
  (\nu a b)
  \left(
  \begin{array}{l}
  \bullet\;
    \begin{array}{l}
    \mathbf{let}\;(x,a^{\prime\prime}) = \mathbf{recv}\;a\;\mathbf{in}
    \\
    \!\!\!
    \begin{array}{lll}
      \mathbf{if}\;x
         & \!\!\!\!\mathbf{then}\!\!\!\! & \mathbf{putStrLn}\;\text{``I'm so happy!''}
      \\ & \!\!\!\!\mathbf{else}\!\!\!\! & \mathbf{putStrLn}\;\text{``Woe is me!''}
    \end{array}
    \end{array}
    \parallel
  \\ \\
  \circ\;
    \begin{array}{l}
    \mathbf{let} \; b^{\prime\prime} = \mathbf{send}\;\mathbf{true}\;b\;\mathbf{in}
    \\
    ()
    \end{array}
  \end{array}
  \right)
\\ \\
\downarrow
\\ \\
  (\nu a b)
  \left(
  \begin{array}{l}
  \bullet\;
    \begin{array}{l}
    \mathbf{let}\;(x,a^{\prime\prime}) = (\mathbf{true},a)\;\mathbf{in}
    \\
    \!\!\!
    \begin{array}{lll}
      \mathbf{if}\;x
         & \!\!\!\!\mathbf{then}\!\!\!\! & \mathbf{putStrLn}\;\text{``I'm so happy!''}
      \\ & \!\!\!\!\mathbf{else}\!\!\!\! & \mathbf{putStrLn}\;\text{``Woe is me!''}
    \end{array}
    \end{array}
    \parallel
  \\ \\
  \circ\;
    \begin{array}{l}
    \mathbf{let} \; b^{\prime\prime} = b\;\mathbf{in}
    \\
    ()
    \end{array}
  \end{array}
  \right)
\\ \\
\downarrow_2
\\ \\
  (\nu a b)
  \left(
  \begin{array}{l}
  \bullet\;
    \begin{array}{lll}
      \mathbf{if}\;\mathbf{true}
         & \!\!\!\!\mathbf{then}\!\!\!\! & \mathbf{putStrLn}\;\text{``I'm so happy!''}
      \\ & \!\!\!\!\mathbf{else}\!\!\!\! & \mathbf{putStrLn}\;\text{``Woe is me!''}
    \end{array}
    \parallel
  \\ \\
  \circ\;
    ()
  \end{array}
  \right)
\\ \\
\downarrow
\\ \\
  (\nu a b)(\bullet\;\mathbf{putStrLn}\;\text{``I'm so happy!''})
\end{array}
$$

*Yes*, we’ve shown that our program is correct! It makes Ada happy! What more could you want?


# Taming the concurrent λ-calculus with types…

Types? Is it types? It *should be!* Just because our happy example works out, doesn’t mean the calculus as a whole is well-behaved. See, we can still encode <a id="formal-cheeky-Ada"></a> [cheeky Ada](#cheeky-Ada), who’ll do anything for that sweet, sweet pudding:

$$
\begin{array}{l}
\text{ada} \triangleq \lambda{a}.(Y \; (\text{tryAll}\;a))\;\mathbf{zero}
\\
\quad\mathbf{where}
\\
\qquad
  \begin{array}{l}
  \text{tryAll} \triangleq \lambda{a}.\lambda\text{rec}.\lambda{x}.
  \\
  \quad
    \begin{array}{l}
    \mathbf{let}\;a^\prime = \mathbf{send}\;x\;a\;\mathbf{in}
    \\
    \mathbf{let}\;(y,a^{\prime\prime}) = \mathbf{recv}\;a^\prime\;\mathbf{in}
    \\
    \!\!\!
    \begin{array}{lll}
    \mathbf{if}\;\mathbf{true}
       & \!\!\!\!\mathbf{then}\!\!\!\! & \mathbf{putStrLn}\;\text{``I'm so happy!''}
    \\ & \!\!\!\!\mathbf{else}\!\!\!\! & \text{rec}\;(\mathbf{suc}\;x)
    \end{array}
    \end{array}
  \end{array}
\end{array}
$$

I’m not gonna write out the whole evaluation, like I did with the previous example, but you can verify for yourself that evaluation gets stuck after a single back-and-forth, with Briar being done with Ada’s cheek and reducing to $\circ\;()$, while Ada still wants to talk.

We’d like to rule out this sort of failing interaction *a priori*. Briar was *very clear* about her boundaries of only taking a *single* request for cake, so we should’ve never set her up with *cheeky* Ada. How are we gonna do this? *With types!*

Developing the type system for the concurrent λ-calculus will be a very similar experience to developing its reduction semantics… we’re mostly just smashing stuff from the λ-calculus and the π-calculus together, and seeing what falls out.

To start off with, we’re gonna copy over the whole type system for the linear λ-calculus, adding the rules for units and pairs as needed. Similar to how we write $A \multimap B$ in place of $A \to B$ for *linear* functions, we write $A \otimes B$ in place of $A \times B$ for *linear* pairs:

$$
\begin{array}{l}
\text{Type} \; A, B, C
\\
\quad
  \begin{array}{rl}
  ::= & \star
  \;\mid\; A \multimap B
  \;\mid\; \mathbf{1}
  \;\mid\; A \otimes B
  \end{array}
\end{array}
$$

::: mathpar
$\begin{array}{c}
\\ \hline
x : A \vdash x : A
\end{array}$

$\begin{array}{c}
\Gamma, x : A \vdash M : B
\\ \hline
\Gamma \vdash \lambda x.M : A \multimap B
\end{array}$
$\begin{array}{c}
\Gamma \vdash M : A \multimap B \quad \Delta \vdash N : A
\\ \hline
\Gamma, \Delta \vdash M \; N : B
\end{array}$

$\begin{array}{c}
\\ \hline
\varnothing \vdash () : \mathbf{1}
\end{array}$
$\begin{array}{c}
\Gamma \vdash M : \mathbf{1} \quad \Delta \vdash N : A
\\ \hline
\Gamma, \Delta \vdash \mathbf{let}\;() = M\;\mathbf{in}\;N : A
\end{array}$

$\begin{array}{c}
\Gamma \vdash M : A \quad \Delta \vdash N : B
\\ \hline
\Gamma, \Delta \vdash (M, N) : A \otimes B
\end{array}$
$\begin{array}{c}
\Gamma \vdash M : A \otimes B \quad \Delta, x : A, y : B \vdash N : C
\\ \hline
\Gamma, \Delta \vdash \mathbf{let}\;(x,y) = M\;\mathbf{in}\;N : C
\end{array}$
:::

First come the rules for variables and functions, which we’ve seen before:

- A variable $x$ has type $A$ if the typing environment *only* contains $x : A$. *(If we’d allow other variables to appear, those would be discarded, since the variable $x$ doesn’t use them, and hence wouldn’t be linear!)*
- If we’ve got something of type $B$ which uses something of type $A$ from the typing environment, we can abstract over that something to create a function of type $A \multimap B$.
- If we’ve got something of type $A \multimap B$ which uses some chunk of the typing environment called $\Gamma$, and something of type $A$ which uses the rest of the typing environment called $\Delta$, then we can apply the former to the latter to get something of type $B$ which uses both $\Gamma$ and $\Delta$.

Then, the rules for units:

- We can always construct the unit value, and doing so uses no resources.
- If we’ve got something of the unit type $\mathbf{1}$ which uses some chunk of the typing environment called $\Gamma$, and something of type $A$ which uses the rest of the typing environment called $\Delta$, then we can put them together to get something of type $A$ which uses both $\Gamma$ and $\Delta$.

And finally, the rules for pairs:

- If we’ve got something of type $A$ which uses some chunk of the typing environment called $\Gamma$, and something of type $B$ which uses the rest of the typing environment $\Delta$, then we can put them together as a pair of type $A \otimes B$ which uses both $\Gamma$ and $\Delta$.
- If we’ve got something of type $A \otimes B$ which uses some chunk of the typing environment called $\Gamma$, and something of type $C$ which uses the rest of the typing environment $\Delta$ *plus* something of type $A$ and something of type $B$, we can put them together to get something of type $C$ which uses both $\Gamma$ and $\Delta$.

Great, that settles it for our term language, doesn’t it? Oh, right! We’re gonna have to give types to $\mathbf{new}$, $\mathbf{spawn}$, $\mathbf{send}$ and $\mathbf{recv}$ as well! And for that, we’ll need session types! We’re gonna import those from the π-calculus, but we’re making one change—instead of only sending and receiving values of *session types* we’re gonna allow ourselves to send and receive values of *any type*:

::: mathpar
$\begin{array}{l}
\text{Type} \; A, B, C
\\
\quad
  \begin{array}{rl}
     ::= & \dots
  \;\mid\; S
  \end{array}
\end{array}$
$\begin{array}{l}
\text{Session type}\;{S}\quad
\\
\quad
  \begin{array}{rl}
     ::= & !A.S
  \;\mid\; ?A.S
  \;\mid\; \mathbf{end}
  \end{array}
\\
\end{array}$
:::

Remember *duality* on session types? Yeah, we’re also gonna need that:

$$
\begin{array}{lrl}
\overline{!A.S}        & = & ?A.\overline{S}
\\
\overline{?A.S}        & = & !A.\overline{S}
\\
\overline{\mathbf{end}} & = & \mathbf{end}
\end{array}
$$

Okay, and we’re finally ready to give types to our concurrency primitives:

::: mathpar
$\begin{array}{c}
\\ \hline
\varnothing\vdash\mathbf{new} : {S\otimes\overline{S}}
\end{array}$
$\begin{array}{c}
\\ \hline
\varnothing\vdash\mathbf{spawn} : (\mathbf{1}\multimap\mathbf{1})\multimap\mathbf{1}
\end{array}$

$\begin{array}{c}
\\ \hline
\varnothing\vdash\mathbf{send} : {A\multimap{!A.S}\multimap{S}}
\end{array}$
$\begin{array}{c}
\\ \hline
\varnothing\vdash\mathbf{recv} : {{?A.S}\multimap{A\otimes{S}}}
\end{array}$
:::

In order of appearance:

- $\mathbf{new}$ creates a new session-typed channel, and returns a pair of two endpoints with dual types.
- $\mathbf{spawn}$ takes a *thunk*, a function of type $\mathbf{1}\multimap\mathbf{1}$, and spawns it off as a thread. This has to be a function, since otherwise we’d be able to start evaluating the thunk before it’s spawned off.
- $\mathbf{send}$ takes a value, and an endpoint to a session-typed channel over which such a value can be send, and returns the continuation of the session.
- $\mathbf{recv}$ takes an endpoint to a session-typed channel over which a value can be received, and returns a pair of the received value and the continuation of the session.

Right, so that’s terms properly taken care of. What about processes? To the surprise of, I hope, absolutely nobody, we’re pretty much gonna copy over the typing rules from the π-calculus, best we can. There’s one small difference. Remember how we were marking threads as either *main* or *child* threads, and only the main thread could return a value? That’s gonna show in our typing rules. First off, we’ll have two ways of embedding terms as processes—either as a main thread or as a child thread—which will show up in the typing judgement:

::: mathpar
$\begin{array}{c}
\Gamma \vdash M : A
\\ \hline
\Gamma \vdash^\bullet \bullet\;M
\end{array}$
$\begin{array}{c}
\Gamma \vdash M : \mathbf{1}
\\ \hline
\Delta \vdash^\circ \circ\;M
\end{array}$
:::

The *premises* here refer back to our term typing rules, but the conclusions uses our process typing rules. In our process typing judgements, we’re marking which kind of thread we’re dealing with on top of the $\vdash$, *e.g.*, as $\vdash^\bullet$ or $\vdash^\circ$. When we compose multiple processes in parallel, we’re going to want to keep track of whether the composition as a whole contains the main thread or not—we’ll do this by combining the markings of the two processes, written $\phi+\phi^\prime$:

::: mathpar
$\begin{array}{lclcl}
\bullet & + & \circ & = & \circ
\\
\circ & + & \bullet & = & \circ
\\
\circ & + & \circ & = & \circ
\end{array}$
:::

We’re not listing what $\bullet+\bullet$ equals—it’s not allowed! If you try to add two main threads, that’s a big no-no, as it violates our *only one main thread* restriction! Okay, so we’re now in a place where we can copy over the remaining rules—the ones for ν-binders and parallel composition:

::: mathpar
$\begin{array}{c}
\Gamma, x : S, x' : \overline{S} \vdash^\phi P
\\ \hline
\Gamma \vdash^\phi (\nu xx') P
\end{array}$
$\begin{array}{c}
\Gamma \vdash^\phi P
\quad
\Delta \vdash^{\phi^\prime} Q
\\ \hline
\Gamma, \Delta \vdash^{\phi+\phi^\prime} P \parallel Q
\end{array}$
:::

Phew, I think that’s it! We’ve got typing rules! Specifically, we’ve now got typing rules which ensure that the session protocol is followed… so we should be able to show that [the interaction between Ada and Briar](#formal-Ada-and-Briar) is well-typed. I’ll leave it up to you to verify this, as the proof is quite big and I’m pretty tired of typesetting things by now. And, great news, [cheeky Ada](#formal-cheeky-Ada) is *not* well-typed! There’s two reasons—one is a bit cheeky, but the other one is a bit more satisfactory:

1. We cannot type cheeky Ada because we have no recursion *(cheeky)*.
2. We cannot type cheeky Ada because she uses the communication channel repeatedly, which violates linearity *(satisfactory)*.

Unfortunately, this type system for the concurrent λ-calculus has similar problems to the type system we showed for the π-calculus… it’s really very restrictive, and yet it still has deadlocks if you start mixing multiple sessions. Fortunately, the same solutions we gave for the π-calculus can be used here. [Philip Wadler][wadler2014] has an example of the first solution, where you glue together ν-binders and parallel composition, in a calculus he calls *Good Variation*. [Luca Padovani and Luca Novara][padovani2015] have an example of the second solution, where you do a global check to see if you have any cyclic dependencies.


# Session End

Whew, that sure was quite a number of words! Let’s look back on what we learned:

# The λ-calculus

- The untyped λ-calculus is a really neat model of computation, but it’s got some problems, namely programs which do nothing forever, and programs which do silly things like adding numbers to functions.
- There’s several approaches to mitigate these problems via type systems, but it’s always a struggle between how many bad programs you rule out versus how many good programs you rule out with them—and how unwieldy your type system gets.

# The π-calculus

- The untyped π-calculus is, like the λ-calculus, a really neat model of computation, and it’s even more expressive, in that it can model concurrency. However, this comes with all the problems of concurrency. Suddenly, we find ourselves facing deadlocks and race conditions!
- There’s several approaches to mitigate these problems via type systems, but again, it’s always a struggle between how many bad programs you rule out versus how many good programs you rule out with them—and how unwieldy your type system gets.

# The concurrent λ-calculus

- We can smash together the λ-calculus and the π-calculus to get the concurrent λ-calculus, with the best of both worlds—it has higher-order functions *and* can model concurrency—and the worst of both worlds—now you’ve got to reason about higher-order functions *and* concurrency!
- Unsurprisingly, the semantics and type systems for the concurrent λ-calculus look a lot like what you’d get if you smashed the semantics and type systems for the λ-calculus and the π-calculus together, but there’s some tiny tweaks we need to make to get them to behave like we want to.

If you made it this far, thanks for reading altogether too many words! If you didn’t—*how are you reading this?!*—thanks for reading whatever number of words you thought was appropriate.

---

**Disclaimer**: I haven’t proven any safety properties for any of the calculi presented here. The simply-typed λ-calculus is pretty well established, so you can trust that’s good, but the other systems are potentially destructive simplifications of existent systems, so all bets are off! I guess you could do the proofs yourself—or if you wanna be really safe, refer to the papers I’ve linked. However, I’ve opted to make these simplifications because the smallest typed π-calculi which are *actually* expressive tend to be pretty big already

[church1932]: https://www.jstor.org/stable/1968337
[church1940]: https://www.jstor.org/stable/2266170
[milner1992]: http://www.lfcs.inf.ed.ac.uk/reports/89/ECS-LFCS-89-85/
[wadler1993]: https://homepages.inf.ed.ac.uk/wadler/papers/lineartaste/lineartaste-revised.pdf
[padovani2015]: http://dx.doi.org/10.1007/978-3-319-19195-9_1
[honda1993]: http://www.kurims.kyoto-u.ac.jp/~kyodo/kokyuroku/contents/pdf/0851-05.pdf
[kobayashi2002]: https://doi.org/10.1016/S0890-5401(02)93171-8
[wadler2014]: https://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-sessions/propositions-as-sessions-jfp.pdf
[caires2012]: https://widgets.figshare.com/articles/6606974/embed
[dardha2018]: http://www.dcs.gla.ac.uk/~ornela/assetsations/DG18-Extended.pdf
[dining-philosophers]: https://en.wikipedia.org/wiki/Dining_philosophers_problem
[pi-calculus-impl]: https://en.wikipedia.org/wiki/%CE%A0-calculus#Implementations
[occam-pi]: https://web.archive.org/web/20190214193147/http://pop-users.org:80/occam-pi/
[pict]: https://www.cis.upenn.edu/~bcpierce/papers/pict/Html/Pict.html
