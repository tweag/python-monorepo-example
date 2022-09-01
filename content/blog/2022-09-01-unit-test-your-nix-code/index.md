---
title: Unit Test Your Nix Code
author: Daniel Baker
tags: [nix]
description: "Cleanly and seamlessly add unit tests to your nix functions."
---

At Tweag, we write a lot of code using the [Nix language][nix-language]. Most of that code produces derivations or packages but occasionally we write small helper functions that are algorithmic in nature. As diligent developers and when appropriate, we should write unit tests to ensure the correctness and maintainability of our code. This post will demonstrate and compare various ways available in the Nix ecosystem to add unit testing to your Nix code -- specifically Nix functions.

[nix-language]: https://nixos.org/manual/nix/stable/language/index.html
[nix-unit-testing]: https://github.com/tweag/nix-unit-testing

## Minimal Examples

We will start with minimal working examples from 3 different frameworks and then show off some extra features and capabilities. If you would like to test out these examples yourself, you can find them all [here][nix-unit-testing].

### Nixpkgs Runtests

Below we have our `math.nix` file; it returns an attribute set with a single function, `isEven`. If we import this file with a statement like `math = import ./math.nix {inherit lib;};`, we would have access to our function like so: `math.isEven`.

```nix
# math.nix
{lib}: {
  # Returns true if integer is even.
  isEven = x: lib.mod x 2 == 0;
}
```

Below we have our `test.nix` file which we will use to test our `isEven` function. `nixpkgs.lib.debug` has a useful function [`runTests`][runtests]. It takes an attribute set of tests where a test is an attribute set with the attribute names `expr` and `expected`. `expr` is the expression we want to test and `expected` is, as its name suggests, the value we expect from our expression. `runTests` will return an empty list if all the tests pass. Otherwise, the list will be populated with information telling you which test failed.

[runtests]: https://nixos.org/manual/nixpkgs/stable/#function-library-lib.debug.runTests

```nix
# test.nix
{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) lib;
  inherit (lib) runTests;
  math = import ./math.nix {inherit lib;};
in
  runTests {
    testIsEven_1 = {
      expr = math.isEven 2;
      expected = true;
    };

    testIsEven_2 = {
      expr = math.isEven (-3);
      expected = true;
    };
  }
```

I have purposefully left an error in our test. Let us evaluate the `test.nix` file and examine the output; we can do so with the `nix eval` command and it returns the following.

```shell
$ nix eval --impure --expr 'import ./test.nix {}'
[ { expected = true; name = "testIsEven_2"; result = false; } ]
```

You can see in the returned list, there is a single attribute set with the name of the test that failed, what it expected, and what the actual result was. After correcting the expected value for `testIsEven_2`, if we evaluate our test again, we get `[ ]`. Perfect!

### Nixt

[Nixt][nixt] is a CLI unit testing framework for the Nix language. The way Nixt creates tests is a bit different from `runTests`; you create test suites with multiple test cases. Below is the code from a file called `test.nixt`. Notice the file extension; Nixt has a [few special rules][nixt-rules] for how test files should be named.

[nixt]: https://github.com/nix-community/nixt
[nixt-rules]: https://github.com/nix-community/nixt#writing-tests

```nix
# test.nixt
{
  pkgs ? import <nixpkgs> {},
  nixt,
}: let
  inherit (pkgs) lib;
  math = import ./math.nix {inherit lib;};
in
  nixt.mkSuite "check isEven" {
    "even number" = math.isEven 2 == true;
    "odd number" = math.isEven (-3) == true;
  }
```

I have purposefully left the same error as before so we can see how Nixt reports errors in comparison to `runTests`. We can run Nixt tests with the following command:

```shell
$ nix run github:nix-community/nixt -- test.nixt

Found 2 cases in 1 suites over 1 files.

  ✗ 1 cases failed.

┏ /home/bakerdn/dev/nix-unit-testing/impure/test.nixt
┃   check isEven
┗     ✗ odd number
```

Note that instead of a single test file `test.nixt`, we could pass a directory path, and Nixt would evaluate all Nixt test files in that directory and its subdirectories.

Nixt provides an absolute path to the offending file along with the suite and case names. If we fix the test and run it again, we get the following:

```shell
Found 2 cases in 1 suites over 1 files.

  ✓ 0 cases failed.

┏ /home/bakerdn/dev/nix-unit-testing/impure/test.nixt
```

### Pythonix

[Pythonix][pythonix] is Python package that uses Nix language internals to evaluate Nix expressions. This allows us to use any Python unit testing framework we like to test our Nix code. Below we have our `test_isEven.py` file and we will use the same `math.nix` file from before. At the time of writing this post, the Pythonix maintainer has archived the project; if this project is meaningful to you, contact them about adopting the project.

[pythonix]: https://github.com/Mic92/pythonix

```python
# test_isEven.py
import nix
from pathlib import Path

'''Note that the path to the file we want to test was declared in Python.
pythonix has some issues evaluating relative file paths.'''
test_file = Path(__file__).parent.resolve() / "math.nix"

def isEven_expr(file: Path, value: int) -> bool:
    '''Note that we could use f-strings here but we would have to escape all
    the curly braces which makes it more difficult to read.'''
    return '''
    (
      {pkgs ? import <nixpkgs> {}}: let
        inherit (pkgs) lib;
        math = import %s {inherit lib;};
      in
        math.isEven (%s)
    ) {}
    ''' % (file, str(value))

def test_isEven_1():
    expr = nix.eval(isEven_expr(file=test_file, value=2))
    assert expr == True

def test_isEven_2():
    expr = nix.eval(isEven_expr(file=test_file, value=-3))
    assert expr == True

```

Just as before, I have purposefully left the same error so we can compare the error reports. I am using pytest to evaluate the test file and we can do so with the following command:

```shell
$ nix shell --impure --expr '(import <nixpkgs> {}).python3.withPackages (p: with p; [ pytest pythonix ])' --command pytest test_isEven.py

============================= test session starts =============================
platform linux -- Python 3.9.12, pytest-6.2.5, py-1.10.0, pluggy-1.0.0
rootdir: /home/bakerdn/dev/nix-unit-testing/impure
collected 2 items

test_isEven.py .F                                                       [100%]

================================== FAILURES ===================================
________________________________ test_isEven_2 ________________________________

    def test_isEven_2():
        expr = nix.eval(isEven_expr(file=test_file, value=-3))
>       assert expr == True
E       assert False == True

test_isEven.py:32: AssertionError
=========================== short test summary info ===========================
FAILED test_isEven.py::test_isEven_2 - assert False == True
========================= 1 failed, 1 passed in 0.20s =========================
```

Note that pytest also has rules for how test files and test functions are named so be aware when writing your tests. It can also be pointed at a directory to search its subdirectories for tests and evaluate them.

pytest is very verbose; it has given us all the information we need to rework the failing test. After fixing the test and running the command to evaluate again, we get the following:

```shell
============================= test session starts =============================
platform linux -- Python 3.9.12, pytest-6.2.5, py-1.10.0, pluggy-1.0.0
rootdir: /home/bakerdn/dev/nix-unit-testing/impure
collected 2 items

test_isEven.py ..                                                       [100%]

============================== 2 passed in 0.19s ==============================
```

## Going Further

### Nixt

Nixt has some extra features that are worth mentioning. There are the `--list` and `--verbose` flags which, when used with the `nixt` command, will find and list all the test cases. You can see an example of this below. Additionally, it has a `--watch` flag which will continuously watch a specified file or directory for changes and reevaluate the tests.

```shell
$ nix run github:nix-community/nixt -- . --list --verbose

Found 2 cases in 1 suites over 1 files.


┏ /home/bakerdn/dev/nix-unit-testing/impure/test.nixt
┃   check isEven
┃     - even number
┗     - odd number
```

### Pythonix

Using pythonix with Python allows for new unit testing capabilities: test strategies and exception handling. To showcase these capabilities, I have created a new `math.nix` file as shown below. It implements a factorial function with some cases that throw exceptions.

```nix
# math.nix
{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) lib;
in rec {
  # Returns the factorial of a non-negative integer.
  factorial = x:
    if (x < 0) || !(builtins.isInt x)
    then throw "factorial only takes non-negative integers, got x = ${toString x}"
    else if x == 0
    then 1
    else x * factorial (x - 1);
}
```

Below we have our `test_math.py` file with a new import, [Hypothesis][hypothesis], a property-based testing library. With Hypothesis, we can setup testing strategies that test behaviour rather than explicit test cases. In our `test_positive_integers` test case, we have instructed Hypothesis to randomly sample integers inclusively between 0 and 20. Similarly, for the `test_negative_integers` test case, we have instructed Hypothesis to randomly sample integers that are less than zero. Normally, we would not be able to do this within Nix. However, because we are using Pythonix, we can capture the exception with `pytest.raises`.

Note I have left two errors in this test. The assertion for `test_positive_integers` is `assert expr < 100` and the factorial function quickly surpasses 100. Additionally, for `test_negative_integers`, I have commented out `with pytest.raises(nix.NixError):` which would have caught a `nix.NixError` exception.

[hypothesis]: https://hypothesis.readthedocs.io/en/latest/

```python
# test_math.py
import nix
from pathlib import Path
from hypothesis import given, strategies as st
import pytest

test_file = Path(__file__).parent.resolve() / "math.nix"

def expression(file: Path, value: int) -> int:
  return '''
  ( {pkgs ? import <nixpkgs> {}}: let
      math = import %s {inherit pkgs;};
    in
      math.factorial (%s)
  ) {}
  ''' % (file, str(value))

# max value limited to 20 because larger values go beyond 64-bit precision
@given(st.integers(min_value=0, max_value=20))
def test_positive_integers(x):
    expr = nix.eval(expression(file=test_file, value=x))
    assert expr < 100

@given(st.integers(max_value=-1))
def test_negative_integers(x):
    # with pytest.raises(nix.NixError):
        expr = nix.eval(expression(file=test_file, value=x))
```

Because we added Hypothesis to our unit testing, we do have to modify the command to include the `hypothesis` Python package. We can evaluate our test file with the following command:

```shell
$ nix shell --impure --expr '(import <nixpkgs> {}).python3.withPackages (p: with p; [ hypothesis pytest pythonix ])' --command pytest test_math.py
============================= test session starts =============================
platform linux -- Python 3.9.12, pytest-6.2.5, py-1.10.0, pluggy-1.0.0
rootdir: /home/bakerdn/dev/nix-unit-testing/hypothesis
plugins: hypothesis-6.24.5
collected 2 items

test_math.py FF                                                         [100%]

================================== FAILURES ===================================
___________________________ test_positive_integers ____________________________

    @given(st.integers(min_value=1, max_value=20))
>   def test_positive_integers(x):

test_math.py:23:
_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

x = 5

    @given(st.integers(min_value=1, max_value=20))
    def test_positive_integers(x):
        expr = nix.eval(expression(file=test_file, value=x))
>       assert expr < 100
E       assert 120 < 100

test_math.py:25: AssertionError
--------------------------------- Hypothesis ----------------------------------
Falsifying example: test_positive_integers(
    x=5,
)
___________________________ test_negative_integers ____________________________

    @given(st.integers(max_value=0))
>   def test_negative_integers(x):

test_math.py:28:
_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

x = 0

    @given(st.integers(max_value=0))
    def test_negative_integers(x):
        # with pytest.raises(nix.NixError):
>           expr = nix.eval(expression(file=test_file, value=x))
E           nix.NixError: factorial only takes positive integers. got x = 0

test_math.py:30: NixError
--------------------------------- Hypothesis ----------------------------------
Falsifying example: test_negative_integers(
    x=0,
)
=========================== short test summary info ===========================
FAILED test_math.py::test_positive_integers - assert 120 < 100
FAILED test_math.py::test_negative_integers - nix.NixError: factorial only t...
============================== 2 failed in 0.09s ==============================
```

Hypothesis helped us identify a simple counter example, specifically that 120 or 5! is not less than 100. Also, our test for negative numbers failed because we did not let pytest know that there would be an exception. If we fix our assertion for `test_positive_integers` to `assert expr > 0` and uncomment the exception catch in `test_negative_integers`, we get the following:

```shell
============================= test session starts =============================
platform linux -- Python 3.9.12, pytest-6.2.5, py-1.10.0, pluggy-1.0.0
rootdir: /home/bakerdn/dev/nix-unit-testing/hypothesis
plugins: hypothesis-6.24.5
collected 2 items

test_math.py ..                                                         [100%]

============================== 2 passed in 0.17s ==============================
```

## Conclusion

Each unit testing methodology has its pros and cons. If your needs are simple and you are comfortable with the Nix language, then `runTests` might be the right choice. If the previous applies but you want a little more from error messages and continuous test evaluation is desirable, then Nixt might be a better choice. Lastly, if you need the extra capabilities of Pytest and Hypothesis, then you will probably want to go with Pythonix. What you need to decide is what level of testing does your project need and how much effort you and your team are willing to dedicate to maintaining your tests.

|                        | runTests | nixt | pythonix |
| :--------------------: | :------: | :--: | :------: |
|  Available in nixpkgs  |   yes    |  no  |    no    |
| Can test eval failures |    no    |  no  |   yes    |
|       Maintained       |   yes    | yes  |    no    |
