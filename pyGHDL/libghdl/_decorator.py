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
from ctypes import c_int32, c_char_p, c_bool, Structure, c_char
from functools import wraps
from typing import Callable, List, Dict, Any, TypeVar

from pydecor import export

from pyGHDL.libghdl import libghdl


@export
def EnumLookupTable(cls) -> Callable:
    """
    Decorator to precalculate a enum lookup table (LUT) for enum position to
    enum literal name.

    :param cls: Enumerator class for which a LUT shall be pre-calculated.
    """

    def decorator(func) -> Callable:
        def gen() -> List[str]:
            d = [e for e in dir(cls) if e[0] != "_"]
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

    def wrapper(func: Callable):
        typeHints: Dict[str, Any] = func.__annotations__
        typeHintCount = len(typeHints)

        if typeHintCount == 0:
            raise ValueError("Function {0} is not annotated with types.".format(func.__name__))

        try:
            returnType = typeHints['return']
        except KeyError:
            raise ValueError("Function {0} is not annotated with a return type.".format(func.__name__))

        if (typeHintCount - 1) != func.__code__.co_argcount:
            raise ValueError("Number of type annotations ({0}) for function '{1}' does not match number of parameters ({2}).".format(
                typeHintCount - 1,
                func.__name__,
                func.__code__.co_argcount)
            )

        #		print(typeHints)

        parameters = typeHints.copy()
        del parameters['return']

        parameterTypes = []
        for parameter in parameters.values():
            if parameter is int:
                parameterTypes.append(c_int32)
            elif parameter is bool:
                parameterTypes.append(c_bool)
            elif parameter is bytes:
                parameterTypes.append(c_char_p)
            elif parameter is c_char:
                parameterTypes.append(c_char)
            elif parameter is c_char_p:
                parameterTypes.append(c_char_p)
            elif isinstance(parameter, TypeVar):
                if (parameter.__bound__ is int) or (parameter.__bound__ is c_int32):
                    parameterTypes.append(c_int32)
                else:
                    raise TypeError("Unsupported parameter type '{0!s}' in function '{1}'.".format(parameter, func.__name__))
            else:
                raise TypeError("Unsupported parameter type '{0!s}' in function '{1}'.".format(parameter, func.__name__))

        if returnType is None:
            resultType = None
        elif returnType is bytes:
            resultType = c_char_p
        elif returnType is c_char:
            resultType = c_char
        elif returnType is c_char_p:
            resultType = c_char_p
        elif (returnType is int):
            resultType = c_int32
        elif (returnType is bool):
            resultType = c_bool
        elif isinstance(returnType, TypeVar):
            if (returnType.__bound__ is int) or (returnType.__bound__ is c_int32):
                resultType = c_int32
            else:
                raise Exception("Unsupported return type '{0!s}' in function '{1}'.".format(returnType, func.__name__))
        elif issubclass(returnType, Structure):
            resultType = returnType
        else:
            raise Exception("Unsupported return type '{0!s}' in function '{1}'.".format(returnType, func.__name__))

        functionPointer = getattr(libghdl, subprogramName)
        functionPointer.parameterTypes = parameterTypes
        functionPointer.restype = resultType

        @wraps(func)
        def inner(*args):
            return functionPointer(*args)

        return inner

    return wrapper
