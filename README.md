FP 2013 Course
==============

01. Introduction
----------------

This course online: http://kspt.icc.spbstu.ru/2013/course/lang

Why should we study various programming languages?

Why is there no universal language sufficient for all tasks?

Important qualities of the code:

* It would work
* It should be easy to read/understand/debug
* It should be easy to modify/re-use/extend
* It should be fast/resource-efficient

Core ideas of functional programming:

* Pure functions without side effects
* Immutable data
* Functions are first-class citizens


02. Erlang Basics
-----------------

Read the first chapters of "Learn You Some Erlang for Great
Good!": from "Starting Out" till "Recursion".


03. Recursion and Lists Handling
--------------------------------

We start with recursion vs iteration (recursion.erl).

Why it is impossible to use loops like `while` and `for` in Erlang?

How can recursion replace iteration?

We continue with different ways of defining linked lists, including
the standard Erlang lists (list_handling.erl). Then we write
some functions over lists and discuss the method of structural
recursion.


04. Higher-order Functions
--------------------------

We start with a task for the students to create a Java function
that transforms an IPv4 address that is given a list of integers
into its canonical string representation. The task continues
with IPv6 addresses (IpAddressUtil.java, IpAddressUtilTest.java).

Why do we need unit tests?

The obvious idea is to copy and paste the IPv4 version and start
modifying it.

Why copy-pasting code is a bad idea?

Name some flaws in the initial imperative implementation, see
`IpAddressUtil.ipV4ToString()`.

The students then suggest to create a single
`ipToString(List<Integer> parts, boolean isIpV4)` function that
does IPv4 or IPv6 conversion depending on a boolean flag. Why is
it a bad idea? What re-usable parts do we get from this change?

Next we step back and create a higher-level description of the
transformation algorithm:

* First we transform each number in the address into its string
  representation (decimal or hexadecimal or whatever)
* Then we join the results using the specified separator (dot
  or semicolon or something else)

What we see here is two more general operations:

* Transform each element of the list using some transformation
  (it should be a function, but we cannot use functions as
  function parameters in Java, so it should be an interface with
  a single method)
* Join a list of strings using a separator (useful for joining
  lists of things via ", " as enumerations or "\n" as lines)

These functions have nothing to do with IP addresses: the first is
a general `map()` function on lists, and the second is a `join()`
function on strings.

See the final versions of our IP to string functions.

