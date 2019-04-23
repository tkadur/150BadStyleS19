Hello everyone!

Announcing the official S19 TA Bad Style Contest!

This is a semi-regular tradition where TAs try to write the worst possible code
for a given task and the students pick the best worst code. You'll be able to pick
your favorite bad style submission in a poll on Piazza.

Here's this semester's task:

Implement the following function:
```sml
(* isEmpty: 'a list -> bool
 * REQUIRES: true
 * ENSURES: isEmpty L ==> true if L is empty
 *                        false otherwise
 *)
```

A normal implementation might look like this:
```sml
fun isEmpty [] = true
  | isEmpty _ = false
```

Without any further ado, here are the submissions (accompanied by descriptions from the authors):

### Submissions

##### 1. Jeanne

[Click here for the code](https://github.com/tkadur/150BadStyleS19/blob/master/Jeanne/badstyle.sml)

>This code extremely repetitively checks the in-memory representation of the list to see if it is nil or not. Turns out nil is unboxed whereas all other lists are boxed.

##### 2. Henry

[Click here for the code](https://github.com/tkadur/150BadStyleS19/blob/master/Henry/empty.sml)

>A production-grade implementation of isEmpty. Written in x86 assembly for performance.


##### 3. Thejas

[Click here for the code](https://github.com/tkadur/150BadStyleS19/tree/master/Thejas)

> This submission thoroughly embraces imperative programming.
>
> A large portion of the code is written in ![equation](https://latex.codecogs.com/svg.latex?%5Ctext%7BC%7D_%5Ctext%7Bnot%7D), which is executed by compiling to x86-64 assembly.

##### 4. Vijay

[Click here for the code](https://github.com/tkadur/150BadStyleS19/blob/master/Vijay/badstyle.sml)


##### 5. Jacob

[Click here for the code](https://github.com/tkadur/150BadStyleS19/blob/master/Jacob/bad.sml)

>Beve Strookes  
>American, 1997-2147
>
>"OCowML", 2019  
>epoxy, PVC, duct tape, human tears, 24K gold flakes, SML code, oil paint
>
>Acquired through the generosity of the Sarah Mellon Scaife Family, 69.42.0
> <br/><br/>
>
>This piece presents a collage of many of the tropes of the FP Flavortext art
>movement, which flourished in Pittsburgh in the early 21st century. The movement
>was defined in cultural opposition to the then-dominant Imperative style, and
>is considered a major force contributing to the eventual dominance of functional
>programming and the widespread abandonment of the C language (and its
>derivatives).
>
>FP Flavortext artists principally utilized post-ironic memes to adorn and
>highlight important techniques and principles of functional programming.
>This piece is noteworthy for it's radical inversion of this relationship:
>instead of utilizing memes as auxiliary adornment to functional content,
>this piece employs functional content in support of a meme. The "I am Cow"
>trope (which serves as this work's central focus) had already fallen out of
>style by the time this piece was produced, but continued to serve as a
>powerful symbol for the FP Flavortext community.
>
>This work also presents the culmination of Strookes' experimentation with the
>use of SML code as a canvas for dank in-jokes. Though the broader FP Flavortext
>art scene was primarily focused on literary works (such as the flavortext
>classics "JVB's Cool Beans Party" and "MkPotionMix"), the annual "Bad Style
>Competition" (for which this work was submitted) served as a venue for the
>community to showcase and celebate advances in the visual media. This work
>is considered by experts to be one of Strooke's finest works in that regard.

### Awards

Submissions will be eligible for the following awards:

* _The Student Choice Award_: for the submission which receives the most student votes
* _The Ariel Davis Award for Achievements in Ridiculous Overkill_: for the most unnecessarily elaborate submission which does way more than necessary to solve the problem
* _The Jacob Neumann Award for Achievements in Jokery and Memecraft_: for the submission with the best memes
* _The Cameron Wong Award for Achievements in Technical Awfulness_: for the submission which best abuses the SML type system, does awful things with imperative code, and generally does everything we tell students not to do
