
R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin17.0 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[R.app GUI 1.73 (7892) x86_64-apple-darwin17.0]

2021-01-24 14:29:06.095 R[79998:2930655] +[NSSavePanel _warmUp] attempted warmup
> install.packages("swirl")
--- Please select a CRAN mirror for use in this session ---
also installing the dependencies ‘utf8’, ‘prettyunits’, ‘pillar’, ‘pkgconfig’, ‘vctrs’, ‘sys’, ‘assertthat’, ‘fansi’, ‘rprojroot’, ‘pkgbuild’, ‘rstudioapi’, ‘diffobj’, ‘rematch2’, ‘tibble’, ‘askpass’, ‘glue’, ‘magrittr’, ‘stringi’, ‘brio’, ‘callr’, ‘cli’, ‘crayon’, ‘desc’, ‘ellipsis’, ‘evaluate’, ‘jsonlite’, ‘lifecycle’, ‘pkgload’, ‘praise’, ‘processx’, ‘ps’, ‘R6’, ‘rlang’, ‘waldo’, ‘withr’, ‘curl’, ‘mime’, ‘openssl’, ‘bitops’, ‘stringr’, ‘testthat’, ‘httr’, ‘yaml’, ‘RCurl’, ‘digest’

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/utf8_1.1.4.tgz'
Content type 'application/x-gzip' length 195526 bytes (190 KB)
==================================================
downloaded 190 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/prettyunits_1.1.1.tgz'
Content type 'application/x-gzip' length 34704 bytes (33 KB)
==================================================
downloaded 33 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/pillar_1.4.7.tgz'
Content type 'application/x-gzip' length 176337 bytes (172 KB)
==================================================
downloaded 172 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/pkgconfig_2.0.3.tgz'
Content type 'application/x-gzip' length 17738 bytes (17 KB)
==================================================
downloaded 17 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/vctrs_0.3.6.tgz'
Content type 'application/x-gzip' length 1403267 bytes (1.3 MB)
==================================================
downloaded 1.3 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/sys_3.4.tgz'
Content type 'application/x-gzip' length 47291 bytes (46 KB)
==================================================
downloaded 46 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/assertthat_0.2.1.tgz'
Content type 'application/x-gzip' length 52572 bytes (51 KB)
==================================================
downloaded 51 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/fansi_0.4.2.tgz'
Content type 'application/x-gzip' length 212149 bytes (207 KB)
==================================================
downloaded 207 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/rprojroot_2.0.2.tgz'
Content type 'application/x-gzip' length 96290 bytes (94 KB)
==================================================
downloaded 94 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/pkgbuild_1.2.0.tgz'
Content type 'application/x-gzip' length 140593 bytes (137 KB)
==================================================
downloaded 137 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/rstudioapi_0.13.tgz'
Content type 'application/x-gzip' length 271959 bytes (265 KB)
==================================================
downloaded 265 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/diffobj_0.3.3.tgz'
Content type 'application/x-gzip' length 992982 bytes (969 KB)
==================================================
downloaded 969 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/rematch2_2.1.2.tgz'
Content type 'application/x-gzip' length 44807 bytes (43 KB)
==================================================
downloaded 43 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/tibble_3.0.5.tgz'
Content type 'application/x-gzip' length 682557 bytes (666 KB)
==================================================
downloaded 666 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/askpass_1.1.tgz'
Content type 'application/x-gzip' length 21511 bytes (21 KB)
==================================================
downloaded 21 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/glue_1.4.2.tgz'
Content type 'application/x-gzip' length 139018 bytes (135 KB)
==================================================
downloaded 135 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/magrittr_2.0.1.tgz'
Content type 'application/x-gzip' length 224854 bytes (219 KB)
==================================================
downloaded 219 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/stringi_1.5.3.tgz'
Content type 'application/x-gzip' length 13641892 bytes (13.0 MB)
==================================================
downloaded 13.0 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/brio_1.1.1.tgz'
Content type 'application/x-gzip' length 36742 bytes (35 KB)
==================================================
downloaded 35 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/callr_3.5.1.tgz'
Content type 'application/x-gzip' length 385435 bytes (376 KB)
==================================================
downloaded 376 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/cli_2.2.0.tgz'
Content type 'application/x-gzip' length 442114 bytes (431 KB)
==================================================
downloaded 431 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/crayon_1.3.4.tgz'
Content type 'application/x-gzip' length 748897 bytes (731 KB)
==================================================
downloaded 731 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/desc_1.2.0.tgz'
Content type 'application/x-gzip' length 285234 bytes (278 KB)
==================================================
downloaded 278 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/ellipsis_0.3.1.tgz'
Content type 'application/x-gzip' length 33497 bytes (32 KB)
==================================================
downloaded 32 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/evaluate_0.14.tgz'
Content type 'application/x-gzip' length 74355 bytes (72 KB)
==================================================
downloaded 72 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/jsonlite_1.7.2.tgz'
Content type 'application/x-gzip' length 492071 bytes (480 KB)
==================================================
downloaded 480 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/lifecycle_0.2.0.tgz'
Content type 'application/x-gzip' length 91596 bytes (89 KB)
==================================================
downloaded 89 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/pkgload_1.1.0.tgz'
Content type 'application/x-gzip' length 150877 bytes (147 KB)
==================================================
downloaded 147 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/praise_1.0.0.tgz'
Content type 'application/x-gzip' length 16137 bytes (15 KB)
==================================================
downloaded 15 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/processx_3.4.5.tgz'
Content type 'application/x-gzip' length 279427 bytes (272 KB)
==================================================
downloaded 272 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/ps_1.5.0.tgz'
Content type 'application/x-gzip' length 276257 bytes (269 KB)
==================================================
downloaded 269 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/R6_2.5.0.tgz'
Content type 'application/x-gzip' length 82447 bytes (80 KB)
==================================================
downloaded 80 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/rlang_0.4.10.tgz'
Content type 'application/x-gzip' length 1327903 bytes (1.3 MB)
==================================================
downloaded 1.3 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/waldo_0.2.3.tgz'
Content type 'application/x-gzip' length 70175 bytes (68 KB)
==================================================
downloaded 68 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/withr_2.4.0.tgz'
Content type 'application/x-gzip' length 204348 bytes (199 KB)
==================================================
downloaded 199 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/curl_4.3.tgz'
Content type 'application/x-gzip' length 741519 bytes (724 KB)
==================================================
downloaded 724 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/mime_0.9.tgz'
Content type 'application/x-gzip' length 35303 bytes (34 KB)
==================================================
downloaded 34 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/openssl_1.4.3.tgz'
Content type 'application/x-gzip' length 2863764 bytes (2.7 MB)
==================================================
downloaded 2.7 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/bitops_1.0-6.tgz'
Content type 'application/x-gzip' length 24529 bytes (23 KB)
==================================================
downloaded 23 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/stringr_1.4.0.tgz'
Content type 'application/x-gzip' length 210650 bytes (205 KB)
==================================================
downloaded 205 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/testthat_3.0.1.tgz'
Content type 'application/x-gzip' length 2898430 bytes (2.8 MB)
==================================================
downloaded 2.8 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/httr_1.4.2.tgz'
Content type 'application/x-gzip' length 498155 bytes (486 KB)
==================================================
downloaded 486 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/yaml_2.2.1.tgz'
Content type 'application/x-gzip' length 203104 bytes (198 KB)
==================================================
downloaded 198 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/RCurl_1.98-1.2.tgz'
Content type 'application/x-gzip' length 1061896 bytes (1.0 MB)
==================================================
downloaded 1.0 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/digest_0.6.27.tgz'
Content type 'application/x-gzip' length 300368 bytes (293 KB)
==================================================
downloaded 293 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/swirl_2.4.5.tgz'
Content type 'application/x-gzip' length 347939 bytes (339 KB)
==================================================
downloaded 339 KB


The downloaded binary packages are in
	/var/folders/91/02w8ykd931ldzcqr0c3djq040000gn/T//Rtmpjl6HL6/downloaded_packages
> library(swirl)

| Hi! Type swirl() when you are ready to begin.

> library(swirl)
> swirl()

| Welcome to swirl! Please sign in. If you've been here before, use the same
| name as you did then. If you are new, call yourself something unique.

What shall I call you? kingcalyb123

| Thanks, kingcalyb123. Let's cover a couple of quick housekeeping items
| before we begin our first lesson. First of all, you should know that when
| you see '...', that means you should press Enter when you are done reading
| and ready to continue.

...  <-- That's your cue to press Enter to continue

| Also, when you see 'ANSWER:', the R prompt (>), or when you are asked to
| select from a list, that means it's your turn to enter a response, then
| press Enter to continue.

Select 1, 2, or 3 and press Enter 

1: Continue.
2: Proceed.
3: Let's get going!

Selection: 1

| You can exit swirl and return to the R prompt (>) at any time by pressing
| the Esc key. If you are already at the prompt, type bye() to exit and save
| your progress. When you exit properly, you'll see a short message letting
| you know you've done so.

| When you are at the R prompt (>):
| -- Typing skip() allows you to skip the current question.
| -- Typing play() lets you experiment with R on your own; swirl will ignore
| what you do...
| -- UNTIL you type nxt() which will regain swirl's attention.
| -- Typing bye() causes swirl to exit. Your progress will be saved.
| -- Typing main() returns you to swirl's main menu.
| -- Typing info() displays these options again.

| Let's get started!

...

| To begin, you must install a course. I can install a course for you from the
| internet, or I can send you to a web page
| (https://github.com/swirldev/swirl_courses) which will provide course
| options and directions for installing courses yourself. (If you are not
| connected to the internet, type 0 to exit.)

1: R Programming: The basics of programming in R
2: Regression Models: The basics of regression modeling in R
3: Statistical Inference: The basics of statistical inference in R
4: Exploratory Data Analysis: The basics of exploring data in R
5: Don't install anything for me. I'll do it myself.

Selection: 1
  |=====================================================================| 100%

| Course installed successfully!


| Please choose a course, or type 0 to exit swirl.

1: R Programming
2: Take me to the swirl course repository!

Selection: 1

| Please choose a lesson, or type 0 to return to course menu.

 1: Basic Building Blocks      2: Workspace and Files     
 3: Sequences of Numbers       4: Vectors                 
 5: Missing Values             6: Subsetting Vectors      
 7: Matrices and Data Frames   8: Logic                   
 9: Functions                 10: lapply and sapply       
11: vapply and tapply         12: Looking at Data         
13: Simulation                14: Dates and Times         
15: Base Graphics             

Selection: 1
  |                                                                     |   0%

| In this lesson, we will explore some basic building blocks of the R
| programming language.

...
  |==                                                                   |   3%
| If at any point you'd like more information on a particular topic related to
| R, you can type help.start() at the prompt, which will open a menu of
| resources (either within RStudio or your default web browser, depending on
| your setup). Alternatively, a simple web search often yields the answer
| you're looking for.

...
  |====                                                                 |   5%
| In its simplest form, R can be used as an interactive calculator. Type 5 + 7
| and press Enter.

> 5+7
[1] 12

| Your dedication is inspiring!
  |=====                                                                |   8%
| R simply prints the result of 12 by default. However, R is a programming
| language and often the reason we use a programming language as opposed to a
| calculator is to automate some process or avoid unnecessary repetition.

...
  |=======                                                              |  11%
| In this case, we may want to use our result from above in a second
| calculation. Instead of retyping 5 + 7 every time we need it, we can just
| create a new variable that stores the result.

...
  |=========                                                            |  13%
| The way you assign a value to a variable in R is by using the assignment
| operator, which is just a 'less than' symbol followed by a 'minus' sign. It
| looks like this: <-

...
  |===========                                                          |  16%
| Think of the assignment operator as an arrow. You are assigning the value on
| the right side of the arrow to the variable name on the left side of the
| arrow.

...
  |=============                                                        |  18%
| To assign the result of 5 + 7 to a new variable called x, you type x <- 5 +
| 7. This can be read as 'x gets 5 plus 7'. Give it a try now.

> x <- 5 + 7

| Perseverance, that's the answer.
  |===============                                                      |  21%
| You'll notice that R did not print the result of 12 this time. When you use
| the assignment operator, R assumes that you don't want to see the result
| immediately, but rather that you intend to use the result for something else
| later on.

...
  |================                                                     |  24%
| To view the contents of the variable x, just type x and press Enter. Try it
| now.

> x
[1] 12

| Keep working like that and you'll get there!
  |==================                                                   |  26%
| Now, store the result of x - 3 in a new variable called y.

> y <- x - 3

| Great job!
  |====================                                                 |  29%
| What is the value of y? Type y to find out.

> y
[1] 9

| Perseverance, that's the answer.
  |======================                                               |  32%
| Now, let's create a small collection of numbers called a vector. Any object
| that contains data is called a data structure and numeric vectors are the
| simplest type of data structure in R. In fact, even a single number is
| considered a vector of length one.

...
  |========================                                             |  34%
| The easiest way to create a vector is with the c() function, which stands
| for 'concatenate' or 'combine'. To create a vector containing the numbers
| 1.1, 9, and 3.14, type c(1.1, 9, 3.14). Try it now and store the result in a
| variable called z.

> z <- c(1.1, 9, 3.14)

| You are amazing!
  |=========================                                            |  37%
| Anytime you have questions about a particular function, you can access R's
| built-in help files via the `?` command. For example, if you want more
| information on the c() function, type ?c without the parentheses that
| normally follow a function name. Give it a try.

> ?c
starting httpd help server ... done

| Excellent work!
  |===========================                                          |  39%
| Type z to view its contents. Notice that there are no commas separating the
| values in the output.

> z
[1] 1.10 9.00 3.14

| Keep up the great work!
  |=============================                                        |  42%
| You can combine vectors to make a new vector. Create a new vector that
| contains z, 555, then z again in that order. Don't assign this vector to a
| new variable, so that we can just see the result immediately.

> c(z, 555, z)
[1]   1.10   9.00   3.14 555.00   1.10   9.00   3.14

| You are amazing!
  |===============================                                      |  45%
| Numeric vectors can be used in arithmetic expressions. Type the following to
| see what happens: z * 2 + 100.

> z * 2 + 100
[1] 102.20 118.00 106.28

| Nice work!
  |=================================                                    |  47%
| First, R multiplied each of the three elements in z by 2. Then it added 100
| to each element to get the result you see above.

...
  |==================================                                   |  50%
| Other common arithmetic operators are `+`, `-`, `/`, and `^` (where x^2
| means 'x squared'). To take the square root, use the sqrt() function and to
| take the absolute value, use the abs() function.

...
  |====================================                                 |  53%
| Take the square root of z - 1 and assign it to a new variable called
| my_sqrt.

> my_sqrt <- sqrt(z - 1)

| Nice work!
  |======================================                               |  55%
| Before we view the contents of the my_sqrt variable, what do you think it
| contains?

1: a vector of length 3
2: a vector of length 0 (i.e. an empty vector)
3: a single number (i.e a vector of length 1)

Selection: 2

| Not exactly. Give it another go.

| Think about how R handled the other 'vectorized' operations:
| element-by-element.

1: a vector of length 3
2: a single number (i.e a vector of length 1)
3: a vector of length 0 (i.e. an empty vector)

Selection: 1

| You got it!
  |========================================                             |  58%
| Print the contents of my_sqrt.

> my_sqrt
[1] 0.3162278 2.8284271 1.4628739

| Nice work!
  |==========================================                           |  61%
| As you may have guessed, R first subtracted 1 from each element of z, then
| took the square root of each element. This leaves you with a vector of the
| same length as the original vector z.

...
  |============================================                         |  63%
| Now, create a new variable called my_div that gets the value of z divided by
| my_sqrt.

> my_div <- z/my_sqrt

| Great job!
  |=============================================                        |  66%
| Which statement do you think is true?

1: The first element of my_div is equal to the first element of z divided by the first element of my_sqrt, and so on...
2: my_div is undefined
3: my_div is a single number (i.e a vector of length 1)

Selection: 
Enter an item from the menu, or 0 to exit
Selection: 1

| Nice work!
  |===============================================                      |  68%
| Go ahead and print the contents of my_div.

> my_div
[1] 3.478505 3.181981 2.146460

| That's correct!
  |=================================================                    |  71%
| When given two vectors of the same length, R simply performs the specified
| arithmetic operation (`+`, `-`, `*`, etc.) element-by-element. If the
| vectors are of different lengths, R 'recycles' the shorter vector until it
| is the same length as the longer vector.

...
  |===================================================                  |  74%
| When we did z * 2 + 100 in our earlier example, z was a vector of length 3,
| but technically 2 and 100 are each vectors of length 1.

...
  |=====================================================                |  76%
| Behind the scenes, R is 'recycling' the 2 to make a vector of 2s and the 100
| to make a vector of 100s. In other words, when you ask R to compute z * 2 +
| 100, what it really computes is this: z * c(2, 2, 2) + c(100, 100, 100).

...
  |======================================================               |  79%
| To see another example of how this vector 'recycling' works, try adding c(1,
| 2, 3, 4) and c(0, 10). Don't worry about saving the result in a new
| variable.

> c(1, 2, 3, 4) + c(0, 10)
[1]  1 12  3 14

| That's a job well done!
  |========================================================             |  82%
| If the length of the shorter vector does not divide evenly into the length
| of the longer vector, R will still apply the 'recycling' method, but will
| throw a warning to let you know something fishy might be going on.

...
  |==========================================================           |  84%
| Try c(1, 2, 3, 4) + c(0, 10, 100) for an example.

> c(1, 2, 3, 4) + c(0, 10, 100)
[1]   1  12 103   4
Warning message:
In c(1, 2, 3, 4) + c(0, 10, 100) :
  longer object length is not a multiple of shorter object length

| Your dedication is inspiring!
  |============================================================         |  87%
| Before concluding this lesson, I'd like to show you a couple of time-saving
| tricks.

...
  |==============================================================       |  89%
| Earlier in the lesson, you computed z * 2 + 100. Let's pretend that you made
| a mistake and that you meant to add 1000 instead of 100. You could either
| re-type the expression, or...

...
  |================================================================     |  92%
| In many programming environments, the up arrow will cycle through previous
| commands. Try hitting the up arrow on your keyboard until you get to this
| command (z * 2 + 100), then change 100 to 1000 and hit Enter. If the up
| arrow doesn't work for you, just type the corrected command.

> z * 2 + 1000
[1] 1002.20 1018.00 1006.28

| That's a job well done!
  |=================================================================    |  95%
| Finally, let's pretend you'd like to view the contents of a variable that
| you created earlier, but you can't seem to remember if you named it my_div
| or myDiv. You could try both and see what works, or...

...
  |===================================================================  |  97%
| You can type the first two letters of the variable name, then hit the Tab
| key (possibly more than once). Most programming environments will provide a
| list of variables that you've created that begin with 'my'. This is called
| auto-completion and can be quite handy when you have many variables in your
| workspace. Give it a try. (If auto-completion doesn't work for you, just
| type my_div and press Enter.)

> my_div
[1] 3.478505 3.181981 2.146460

| Keep working like that and you'll get there!
  |=====================================================================| 100%
| Would you like to receive credit for completing this course on Coursera.org?

1: No
2: Yes

Selection: 1