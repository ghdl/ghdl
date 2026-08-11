# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
# Authors:
#   Patrick Lehmann
#
# Package module:   Python binding and low-level API for shared library 'libghdl'.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2021 Tristan Gingold
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.
#
# SPDX-License-Identifier: GPL-2.0-or-later
# ============================================================================
#
"""
Decorators binding a Python function to a subprogram in the shared library ``libghdl``.

:func:`BindToLibGHDL` reads a function's type hints, derives the matching :mod:`ctypes` signature and replaces the
function body with a call into the shared library. :func:`EnumLookupTable` precalculates an enum position to name
table.
"""

from ctypes import (
    c_int32,
    c_uint32,
    c_char_p,
    c_void_p,
    c_bool,
    c_double,
    Structure,
    c_char,
    c_uint64,
    c_int64,
)
from enum import IntEnum
from functools import wraps
from typing import Callable, List, Dict, Any, TypeVar

from pyTooling.Decorators import export

from pyGHDL.libghdl import libghdl, LibGHDLException


@export
def EnumLookupTable(cls) -> Callable:
    """
    Decorator to precalculate an enum lookup table (LUT) for enum position to
    enum literal name.

    :returns: Decorator function replacing the placeholder with a table lookup.
    """

    def decorator(func) -> Callable:
        """
        Replace the decorated placeholder with a lookup in the precalculated table.

        :param func: The placeholder function being decorated.
        :returns:    A function mapping an enum position to the literal's name.
        """

        def gen() -> List[str]:
            """
            Build the lookup table from an enum position to the literal's name.

            :returns: The list of literal names, indexed by enum position.
            """
            d = [e for e in dir(cls) if e[0].isupper() and e[0] != "_"]
            res = [None] * len(d)
            for e in d:
                res[getattr(cls, e)] = e
            return res

        __lut = gen()

        @wraps(func)
        def wrapper(id: int) -> str:
            """
            Return the name of the enum literal at the given position.

            :param id: The position of the enum literal.
            :returns:  The literal's name.
            """
            return __lut[id]

        return wrapper

    return decorator


def BindToLibGHDL(subprogramName):
    """
    This decorator creates a Python function to interface with subprograms in
    libghdl via :mod:`ctypes`.

    :param subprogramName: Name of the subprogram in *libghdl*.
    """

    def PythonTypeToCtype(typ):
        """
        Translate a Python type hint to the matching :mod:`ctypes` type.

        .. list-table::
           :header-rows: 1
           :widths: 40 60

           * - Type hint
             - ctypes type
           * - ``None``
             - ``None`` - a procedure's return type
           * - :class:`int`
             - ``c_int32``
           * - :class:`float`
             - ``c_double``
           * - :class:`bool`
             - ``c_bool``
           * - :class:`bytes`
             - ``c_char_p`` - passed through, *not* encoded
           * - ``c_char``, ``c_char_p``, ``c_uint32``, ``c_void_p``
             - itself
           * - a :class:`~typing.TypeVar` from :mod:`~pyGHDL.libghdl._types`
             - its ``__bound__``, or ``c_int32`` / ``c_double`` when bound to :class:`int` / :class:`float`
           * - an :class:`~enum.IntEnum`
             - ``c_int32``
           * - a :class:`~ctypes.Structure`
             - itself

        :param typ:        The type hint to translate.
        :returns:          The matching ctypes type, or ``None`` for a procedure's return type.
        :raises TypeError: If the type hint has no ctypes equivalent.
        """
        if typ is None:
            return None
        elif typ is int:
            return c_int32
        elif typ is float:
            return c_double
        elif typ is bool:
            return c_bool
        elif typ is bytes:
            return c_char_p
        elif typ in (c_char, c_char_p, c_uint32, c_void_p):
            return typ
        elif isinstance(typ, TypeVar):
            # Humm, recurse ?
            if typ.__bound__ is int:
                return c_int32
            if typ.__bound__ is float:
                return c_double
            if typ.__bound__ in (
                c_bool,
                c_uint32,
                c_int32,
                c_uint64,
                c_int64,
                c_double,
            ):
                return typ.__bound__
            raise TypeError(f"Unsupported typevar bound to {typ.__bound__!s}")
        elif issubclass(typ, IntEnum):
            return c_int32
        elif issubclass(typ, Structure):
            return typ
        raise TypeError

    def wrapper(func: Callable):
        """
        Replace the decorated placeholder with a call into the shared library.

        The :mod:`ctypes` signature is derived from the function's type hints, so the placeholder needs
        a full annotation and no body.

        :param func:        The placeholder function being decorated.
        :returns:           A function calling ``subprogramName`` in *libghdl*.
        :raises ValueError: If the function is not annotated, or the annotations do not match its parameters.
        """
        typeHints: Dict[str, Any] = func.__annotations__
        typeHintCount = len(typeHints)

        if typeHintCount == 0:
            raise ValueError(f"Function {func.__name__} is not annotated with types.")

        try:
            returnType = typeHints["return"]
        except KeyError:
            raise ValueError(f"Function {func.__name__} is not annotated with a return type.")

        if (typeHintCount - 1) != func.__code__.co_argcount:
            raise ValueError(
                f"Number of type annotations ({typeHintCount - 1}) for function '{func.__name__}' does not match number of parameters ({func.__code__.co_argcount})."
            )

        # 		print(typeHints)

        parameters = typeHints.copy()
        del parameters["return"]

        parameterTypes = []
        for parameter in parameters.values():
            try:
                parameterTypes.append(PythonTypeToCtype(parameter))
            except TypeError:
                raise TypeError(f"Unsupported parameter type '{parameter!s}' in function '{func.__name__}'.")

        try:
            resultType = PythonTypeToCtype(returnType)
        except TypeError:
            raise TypeError(f"Unsupported return type '{returnType!s}' in function '{func.__name__}'.")

        functionPointer = getattr(libghdl, subprogramName)
        functionPointer.argtypes = parameterTypes
        functionPointer.restype = resultType

        if isinstance(returnType, type) and issubclass(returnType, IntEnum):

            @wraps(func)
            def inner(*args):
                """
                Call ``subprogramName`` in *libghdl* and translate a failure into an exception.

                :param args:              The arguments to pass to the subprogram.
                :returns:                 The subprogram's result.
                :raises LibGHDLException: If the call into the shared library fails.
                """
                try:
                    returnValue = functionPointer(*args)
                except OSError as ex:
                    errors = [str(ex)]
                    raise LibGHDLException(
                        f"Caught exception when calling '{subprogramName}' in libghdl.",
                        errors,
                    ) from ex

                return returnType(returnValue)

            return inner
        else:

            @wraps(func)
            def inner(*args):
                """
                Call ``subprogramName`` in *libghdl* and translate a failure into an exception.

                :param args:              The arguments to pass to the subprogram.
                :returns:                 The subprogram's result.
                :raises LibGHDLException: If the call into the shared library fails.
                """
                try:
                    return functionPointer(*args)
                except OSError as ex:
                    errors = [str(ex)]
                    raise LibGHDLException(
                        f"Caught exception when calling '{subprogramName}' in libghdl.",
                        errors,
                    ) from ex

            return inner

    return wrapper
