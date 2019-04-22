# Types in Pidgin

Author: Steven Heidel

The JSON spec defines seven values which map well to Python types:
string -> string
number -> int/float *
object -> dict
array -> list
true -> True
false -> False
null -> None

Pidgin should have the ability to define these types and annotate them in
the constructor.

Take a look at this example of an ideal constructor:
Note: this is only Python 3 because it's clearer, for the real life version
we'll have to use 2/3 syntax that I specify below.

```python
from typing import List, Optional


class Pulse:
    def __init__(self, time: float, frame: str, waveform: str, phase: float,
                scale: float, actual_if: float):
        self.time = time
        # and so on
```

Type Validation

It's also possible to add runtime checks to the constructor which may aid in
development. Contrast the difference between getting a runtime error on the
client side vs. having the server throw a less comprehensible error on its
machine which may/may not be propogated back.

```python
class Pulse2:
    def __init__(self, time: float, frame: str, waveform: str, phase: float,
                 scale: float, actual_if: float):
        if not isinstance(time, float):
            raise TypeError("parameter time must be a float")
        # and so on

        self.time = time
        # and so on
```

Optionals and defaults

A field may or may not have a default that gets used if you don't
provide that value explicitly.
A field may or may not allow the value None.

These two rules end up creating three possibilities for each type which may
appear counterintuitive at first but are needed to cover all possible cases.

1. Required with no default
    - invalid not to provide a value for this field
    - invalid to pass None for this field
2. Required with a default
    - if a value is not provided, then a fallback value will be used
    - invalid to pass None for this field
3. Optional with no default (equivalent to optional with a default of None)
    - valid to either not provide a value or to pass None for this field
4. Optional with a default
    - if a value is not provided, then a fallback value is used
    - you can explicitly pass None for this field

All required types must be listed before all optional types.

```python
class Example1:
    def __init__(self, required_no_default: str,
                 required_with_default: str = "default",
                 optional_no_default: Optional[str] = None,
                 optional_with_default: Optional[str] = "default"):
        if required_no_default is None:
            raise ValueError("required_no_default cannot be None")
        if required_with_default is None:
            raise ValueError("required_with_default cannot be None")

        # type checks
        # initialization
```

Lists and Dicts

Lists and Dicts require special handling for two reasons:
1. Mutable default arguments in Python
2. Both Lists and Dicts can be "empty" which is different than if they're None

Point 2 needs to be explained further. If lists are not passed to a constructor
then the default must be a valid list and cannot be None. For lists, empty list
takes the place of None. In almost all use cases there are no semantic differences
between empty list and None and having such a distinction is a common source of
confusion.

For the rare use cases that _does_ have semantic difference between an empty
list and None the types can be modelled differently.

Lists and Dicts also take optional type parameters. It would be best if these
type parameters were filled in as well.

```python
class Example2:
    # optional cases do not apply for lists since they can't be None
    def __init__(self, required_no_default: List[Pulse],
                 required_with_default: List[Pulse] = None):
        if required_no_default is None:
            raise ValueError("required_no_default cannot be None")
        if required_with_default is None:
            required_with_default = []

        # type checks
        # initialization
```

Python 2/3 Compatibility

This brings us to the final version using Python 2 compatible type annotations

Also added some various other boilerplate methods which will come in handy

```python
class Example3:
    def __init__(self, required_no_default,
                 required_list,
                 required_with_default = "default",
                 optional_no_default = None,
                 optional_with_default = "default",
                 optional_list = None):
        # type: (str, List[str], str, Optional[str], Optional[str], List[str]) -> None

        # Step 1: check requirements
        if required_no_default is None:
            raise ValueError("required_no_default cannot be None")
        if required_list is None:
            raise ValueError("required_list cannot be None")

        # Step 2: deal with mutable default arguments
        if optional_list is None:
            optional_list = []

        # Step 3: type checks
        if not isinstance(required_no_default, str):
            raise TypeError("parameter required_no_default must be a str")
        if not isinstance(required_list, list):
            raise TypeError("parameter required_list must be a list")
        if not isinstance(required_with_default, str):
            raise TypeError("parameter required_with_default must be a str")
        if not (isinstance(optional_no_default, str) or optional_no_default is None):
            raise TypeError("parameter optional_no_default must be a str")
        if not (isinstance(optional_with_default, str) or optional_with_default is None):
            raise TypeError("parameter optional_with_default must be a str")
        if not isinstance(optional_list, list):
            raise TypeError("parameter optional_list must be a list")

        # Step 4: initialization
        self.required_no_default = required_no_default  # type: str
        self.required_list = required_list  # type: List[str]
        self.required_with_default = required_with_default  # type: str
        self.optional_no_default = optional_no_default  # type: str
        self.optional_with_default = optional_with_default  # type: str
        self.optional_list = optional_list  # type: List[str]

    def __eq__(self, other):
        if other.__class__ is self.__class__:
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __ne__(self, other):
        # __ne__ is not the opposite of __eq__ in Python 2 and needs to be specified manually
        return not self.__eq__(other)

    def __repr__(self):
        return "{}({})".format(
            self.__class__.__name__,
            ", ".join("{}={!r}".format(k, v) for k, v in sorted(self.__dict__.items(),
                                                                key=lambda kv: kv[0])))

    def __hash__(self):
        return hash(self.__dict__)

    def __copy__(self):
        return self.__class__(**self.__dict__)

    def __deepcopy__(self, memo):
        ret = self.__class__(**deepcopy(self.__dict__, memo=memo))
        memo[id(self)] = ret
        return ret
```

Closing Thoughts

Wow, that's a lot of code! However fortunately for us it's automatically
generated which is fantastic. Given the choice between a short implementation
and a better user experience you should _always_ pick the better user experience
when the code is autogenerated. Implementation length only really matters when
the code is constructed by hand instead of programmatically.

Miscellaneous:

#1
In JSON you have to make a choice about whether there should be a difference
between fields that are not present vs. fields that contain the value null.

So the question is, are the following two objects the same?
{"time": null} == {} ?

My choice would be to say: yes, they are equivalent. The right hand is a more
efficient serialization and there's no semantic difference between the two.

#2
In Python 3 we can make a number of improvements to the type syntax. We can also
use the * parameter after all the required arguments have been listed to force
end users to use named parameters for all the optional ones.

#3
In Python 3.7 (coming soon) there is a new feature called @dataclasses which
basically do ever thing above automagically. It supports type validation,
defaults, mutable vs immutable fields, etc. and will also have IDE support
when it is ready.
