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
from ctypes import (
    c_int32,
    c_uint32,
    c_char_p,
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

    :param cls: Enumerator class for which a LUT shall be pre-calculated.
    """

    def decorator(func) -> Callable:
        def gen() -> List[str]:
            d = [e for e in dir(cls) if e[0].isupper() and e[0] != "_"]
            res = [None] * len(d)
            for e in d:
                res[getattr(cls, e)] = e
            return res

        __lut = gen()

        @wraps(func)
        def wrapper(id: int) -> str:
            # function that replaces the placeholder function
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
        elif typ in (c_char, c_char_p, c_uint32):
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
        functionPointer.parameterTypes = parameterTypes
        functionPointer.restype = resultType

        if isinstance(returnType, type) and issubclass(returnType, IntEnum):

            @wraps(func)
            def inner(*args):
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
