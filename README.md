eng_auto_dict
================

## Overview

Welcome to my latest masterpiece, the Email Words-a-Day project! As a
lazy language learner, I was tired of my old routine of cramming new
words into my brain and promptly forgetting them the next day. So, I
decided to take matters into my own hands and create a daily email
reminder featuring a new English word to help me expand my vocabulary
without lifting a finger (well, except writing dozens lines of code).
And thus, the Email Words-a-Day project was born! Join me on this
journey of linguistic laziness as we expand our vocabularies one day at
a time. You’ll never have to worry about forgetting a new word again, as
long as you remember to check your inbox!

## Instalation

Installing this bad boy is a piece of cake…well, sort of. First things
first, you’ll need to git pull this repo. Once you’ve got it, run
`renv::restore()` to make sure everything’s in tip-top shape.

Now, onto the fun stuff. To get this baby up and running, you’ll need a
Gmail tech account and a list of words you want to learn. I personally
like to grab my words from Google Translate, because let’s face it,
nothing beats the thrill of learning a new language at 2 AM.

Once you’ve got your words loaded up into a handy dandy Google Sheet
(which you’ll have to do manually, sorry not sorry), you’re ready to run
the code and start learning! Just make sure you have a solid internet
connection and a cup of coffee nearby, because things are about to get
wild.

Oh, and if you want to make this a daily thing (because why wouldn’t
you?), just set up a Windows task scheduler on your laptop and let the
magic happen. It’s like having a personal language tutor in your inbox
every single day. You’re welcome.

## Sources

Sources, sources, sources… where to begin? Let’s start with the heart
and soul of this project: [Cambridge
Dictionary](https://dictionary.cambridge.org/). Why? Because for me,
it’s all about context. I need to see how these new words fit into
real-life situations and understand their meanings on a deeper level.
Plus, who doesn’t love a British accent?

But let me tell you, getting information from the Cambridge Dictionary
website is like searching for a needle in a haystack… blindfolded… with
one hand tied behind your back. The HTML structure is more convoluted
than a cat trying to play Jenga, and some words have unexpected pages
that can send you down a rabbit hole of confusion.

But wait, there’s more! To really take this project to the next level, I
also use a Python library
[word_forms](https://github.com/gutfeeling/word_forms) (thank you,
reticulate!). Translation function into tidy R is below. This handy
dandy tool gives me all the word-building information I need to really
flex my vocab muscles. I’m talking prefixes, suffixes, roots, you name
it.

``` r
py_word_forms <- function(word) {
  word_forms <- reticulate::import("word_forms.word_forms")
  word_list <- word_forms$get_word_forms(word)
  purrr::map(word_list, reticulate::iterate)
}

py_word_forms("bland")
```

    ## $n
    ## [1] "blandness"   "blandnesses"
    ## 
    ## $a
    ## [1] "bland"
    ## 
    ## $v
    ## list()
    ## 
    ## $r
    ## [1] "blandly"

## Email structure

Every day, I pick three lucky words - one new word and two that have
been sent before (because who doesn’t love a good reunion?). But don’t
worry, it’s all fair and square - the probability of a word being chosen
depends on how many times it has been sent before, the formula is so
simple, it’s practically invisible `prob = 1L/(.)^2L`.

For each word, I take the time to find all its different forms and
collect data about its meaning, examples, pronunciation, part of speech
and special emoji by `emo::ji()` - I don’t half-ass anything, even my
words.

But wait, there’s more! To add a bit of fun to the email, I also include
a test section with three other randomly chosen words from previous
emails. It’s like a little game that I play with myself, and you get to
reap the benefits.

So, sit back, relax, and let the words wash over you. Who knows, you
might even learn something (besides the fact that I’m a word nerd).

Of course, the daily email isn’t just about learning new words - it’s
also a chance to praise and motivate yourself! That’s why at the end of
each email, I’ve included a special message to give you a little boost
of confidence. Trust me, you’ll feel like a linguistic superhero in no
time!

``` r
paste0(
  sample(stringr::str_to_sentence(praise::praise_parts$exclamation), 1), "! ", 
  praise::praise()
)
```

    ## [1] "Hurrah! You are finest!"
