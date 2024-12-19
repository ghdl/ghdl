# =============================================================================
#               ____ _   _ ____  _          _
#  _ __  _   _ / ___| | | |  _ \| |      __| | ___  _ __ ___
# | '_ \| | | | |  _| |_| | | | | |     / _` |/ _ \| '_ ` _ \
# | |_) | |_| | |_| |  _  | |_| | |___ | (_| | (_) | | | | | |
# | .__/ \__, |\____|_| |_|____/|_____(_)__,_|\___/|_| |_| |_|
# |_|    |___/
# =============================================================================
# Authors:
#   Patrick Lehmann
#
# Package module:   DOM: Interface items (e.g. generic or port)
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
"""
This module implements derived name classes from :mod:`pyVHDLModel.Name`.
"""
from typing import List

from pyTooling.Decorators import export

from pyVHDLModel.Name import Name as VHDLModel_Name
from pyVHDLModel.Name import SimpleName as VHDLModel_SimpleName
from pyVHDLModel.Name import ParenthesisName as VHDLModel_ParenthesisName
from pyVHDLModel.Name import IndexedName as VHDLModel_IndexedName
from pyVHDLModel.Name import SlicedName as VHDLModel_SlicedName
from pyVHDLModel.Name import SelectedName as VHDLModel_SelectedName
from pyVHDLModel.Name import AttributeName as VHDLModel_AttributeName
from pyVHDLModel.Name import AllName as VHDLModel_AllName
from pyVHDLModel.Name import OpenName as VHDLModel_OpenName

from pyGHDL.libghdl._types import Iir
from pyGHDL.dom import DOMMixin


@export
class SimpleName(VHDLModel_SimpleName, DOMMixin):
    """
    Represents a single name (identifier) to a language entity.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Name.SimpleName`.

    .. admonition:: Example

       .. code-block:: VHDL

          ieee
    """

    def __init__(self, node: Iir, identifier: str) -> None:
        super().__init__(identifier)
        DOMMixin.__init__(self, node)


@export
class ParenthesisName(VHDLModel_ParenthesisName, DOMMixin):
    """
    Represents a parenthesis (index access or function call) following a name.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Name.ParenthesisName`.

    .. admonition:: Example

       .. code-block:: VHDL

          arr(0)
          -- ^^^
    """

    def __init__(self, node: Iir, prefix: VHDLModel_Name, associations: List) -> None:
        super().__init__(prefix, associations)
        DOMMixin.__init__(self, node)


@export
class IndexedName(VHDLModel_IndexedName, DOMMixin):
    def __init__(self, node: Iir, identifier: str) -> None:
        super().__init__(identifier)
        DOMMixin.__init__(self, node)


@export
class SlicedName(VHDLModel_SlicedName, DOMMixin):
    """
    Represents a slice following a name.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Name.SlicedName`.

    .. admonition:: Example

       .. code-block:: VHDL

          arr(7 downto 0)
          -- ^^^^^^^^^^^^
    """

    def __init__(self, node: Iir, identifier: str) -> None:
        super().__init__(identifier)
        DOMMixin.__init__(self, node)


@export
class SelectedName(VHDLModel_SelectedName, DOMMixin):
    """
    Represents a name following a name in dot-notation.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Name.SelectedName`.

    .. admonition:: Example

       .. code-block:: VHDL

          rec.elem
          -- ^^^^^
    """

    def __init__(self, node: Iir, identifier: str, prefix: VHDLModel_Name) -> None:
        super().__init__(identifier, prefix)
        DOMMixin.__init__(self, node)


@export
class AttributeName(VHDLModel_AttributeName, DOMMixin):
    """
    Represents a name following a name in tick-notation.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Name.AttributeName`.

    .. admonition:: Example

       .. code-block:: VHDL

          arr'length
          -- ^^^^^^^
    """

    def __init__(self, node: Iir, identifier: str, prefix: VHDLModel_Name) -> None:
        super().__init__(identifier, prefix)
        DOMMixin.__init__(self, node)


@export
class AllName(VHDLModel_AllName, DOMMixin):
    """
    Represents a keyword ``all`` following a name in dot-notation.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Name.AllName`.

    .. admonition:: Example

       .. code-block:: VHDL

          ptr.all
          -- ^^^^
    """

    def __init__(self, node: Iir, prefix: VHDLModel_Name) -> None:
        super().__init__(prefix)
        DOMMixin.__init__(self, node)


@export
class OpenName(VHDLModel_OpenName, DOMMixin):
    """
    Represents the keyword ``open`` used as a special name.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Name.OpenName`.

    .. admonition:: Example

       .. code-block:: VHDL

          open
    """

    def __init__(self, node: Iir) -> None:
        super().__init__()
        DOMMixin.__init__(self, node)
