---
title: An Introduction to Session Types
katex: true
---

Session types. Ostensibly, I’ve studied them for the past few years. So I should know sometime about them, right?

I’ll explain the basics of session types. What are they? What are the foundations for session types like? What are the interesting problems?

Let’s start out with a *dramatis personæ*. 

## Dramatis Personæ

Session types are about *channels*, which are like *tin can telephones*, in that you can use your tin to whisper sweet little nothings to every friend who has a tin connected to yours. I know, I’m betraying my age a little—I’m an old Victorian lady.

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

In the vernacular of session types, the tin cans are referred to as *channel-endpoints* or simply *endpoints*, and the collection of all tin cans held together by the twine is referred to as a *channel*. A series of messages whispered back and forth over a single channel is referred to as a *session*. 

Most of the literature on session types considers only the classic scenario, in which we connect exactly two tin cans to form a channel—this is referred to as *binary session types*. Yet if we wanted to, we could make a telephone with any number of tin cans—this is referred to as *multiparty session types*.

In this blog post, we I’ll focus on *binary session types*


## Session Types *at a Glance*

Let’s imagine for a moment that Ada were to take Briar up on her offer, and ask her to sample her famous milk puddings. Briar, a proper lady, only offers her milk puddings to those who make a *sufficiently polite* request—Ada must be polite and say “please”, but she must not overuse it, lest she comes off as begging! 

We encode the interaction between Ada and Briar using *session types* in Haskell:

- Ada’s requests are represented using the `Request` datatype, which allows us to prefix a request for pudding with any number of uses of `Please`;
- Briar’s response is represented using the `Response` datatype, in which she can either grant permission, in which case Briar sends an `Allow` with a sample of pudding attached, or refuse Ada’s request, in which case she sends a `Deny` with a reason.

The functions `ada` and `briar` represent Ada and Briar—these functions each receive an endpoint for the shared channel, and communicate along the lines of our story—Ada sends a request, Briar evaluates her politeness and responds with either pudding or a refusal, and finally Ada evaluates Briars response, and expresses her emotions accordingly.

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

Secondly, the types of the endpoints of a binary channel must be *dual*. When Ada’s endpoint says she must send, Briar’s endpoint says she must receive. For multiparty session types, the equivalent notion is called *coherence*, but the principle remains the same.

Finally, each endpoint must be used *exactly once* if we want to be sure to stick to the protocol. For instance, in the code above, each channel-endpoint is only used once, and each send or receive returns a new channel on which to continue the communication. If we didn’t, we would be able to write a cheeky variant of Ada, who simply tries any number of pleases until she gets that sweet, sweet pudding.

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

Only a few short years after Ada and Briar enjoyed sweet milk puddings, a man by the name of *Alonzo Church* was born in Washington, D.C., in the United States. Three decades later, in the 1930s, Alonzo would develop [the λ-calculus][church1932], a foundational calculus which studies computation using *functions*. To this day, the λ-calculus underpins most theories of functional programming languages. Talk about influential! 

Only a few short years after Alonzo developed the λ-calculus, a man by the name of *Robin Milner* was born near Yealmpton, in England. Alonzo lived a long live, over nine decades! A few years before Alonzo’s death in the mid 1990s, Robin, together with Joachim Parrow and David Walker, developed the [π-calculus][milner1992], a foundational calculus which studies concurrent computation by processes using *message-passing communication*. It wasn’t the first process calculus—it itself was heavily influenced by ideas dating back to the early 1980s—but it’s certainly one of the most influential!

We’ll start out by discussing the untyped λ-calculus. It’s a wonderful little language, and it’s *really* powerful. Unfortunately, it has all sorts of programs that do all sorts of bad things, like loop forever, so with all that power, it’s *really scary* too! We’ll then discuss the idea of taming all that scary power using types, to try and get only well-behaved programs, and the challenges of taming it without taking all the *oomph* out.

Then, we’ll switch to discussing the π-calculus. It’s a wonderful little language, even if it’s twice as big as the λ-calculus—with *six* constructs instead of *three*! It’s even more powerful than the λ-calculus—it can express all sorts of concurrent behaviours that the λ-calculus has no hope of expressing. Unfortunately, it’s scarier as well—there’s way more things that can go wrong! Again, we’ll turn our attention to taming all that scary power using types, and the problems of *oomph*’lessness that come with it.

Finally, we’ll talk about having the best of both worlds, in a concurrent λ-calculus, which is sorta what you get when you smash the λ-calculus and the π-calculus together at high speeds! The concurrent λ-calculus has the best of both worlds: higher-order functions and concurrency with message-passing communication!

- The untyped λ-calculus! *(So powerful, so scary…)*
- Taming the λ-calculus with types…
- The untyped π-calculus! *(Is even scarier…)*
- Taming the π-calculus with types…
- Concurrent λ-calculus! *(λ and π, together forever!)*

[church1932]: https://www.jstor.org/stable/1968337
[milner1992]: http://www.lfcs.inf.ed.ac.uk/reports/89/ECS-LFCS-89-85/
