# =============================================================================
# Authors:
#   Patrick Lehmann
#
# Package module:   Generate the AST node reference from src/vhdl/vhdl-nodes.ads.
#
# License:
# ============================================================================
#  Copyright (C) 2026-2026 Patrick Lehmann - Boetzingen, Germany
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
Render the IIR node reference from the meta-model in :file:`src/vhdl/vhdl-nodes.ads`.

The Ada source is parsed by :mod:`scripts.pnodes`, the same script that generates the Ada accessors and
the :mod:`pyGHDL.libghdl` bindings, so the reference cannot describe a node that does not exist or miss
a field that was added. The pages are written when Sphinx starts, not committed.
"""
import sys
from pathlib import Path
from textwrap import wrap
from typing import Dict, List, Optional as Nullable, Tuple

#: Node references are the fields worth drawing an arrow for.
LINK_TYPES = ("Iir", "Iir_List", "Iir_Flist", "PSL_Node", "PSL_NFA")

#: Graphviz edge styles: owned is black, a reference blue, a forward reference orange, a chain green.
OWNED = 'color="#000000"'
REFERENCE = 'color="#3465a4"'
FORWARD = 'color="#f57900"'
CHAIN = 'color="#4e9a06"'

#: How the access attribute of a field is rendered: (label, description, graphviz edge style).
ACCESS_KINDS = {
	None: ("owned", "The node owns the referenced node.", OWNED),
	"Ref": ("ref", "A reference to a node owned elsewhere.", REFERENCE + " style=dashed"),
	"Maybe_Ref": ("maybe ref", "A reference if ``Is_Ref`` is set, otherwise owned.", REFERENCE + " style=dotted"),
	"Of_Ref": ("of ref", "The elements of the list are references.", REFERENCE + " style=dashed"),
	"Of_Maybe_Ref": ("of maybe ref", "The elements are references if ``Is_Ref`` is set.", REFERENCE + " style=dotted"),
	"Forward_Ref": ("forward ref", "A reference resolved only by semantic analysis.", FORWARD + " style=dashed"),
	"Maybe_Forward_Ref": ("maybe forward ref", "A forward reference if ``Is_Forward_Ref``.", FORWARD + " style=dotted"),
	"Chain": ("chain", "The head of a chain of nodes linked by ``Get_Chain``.", CHAIN),
	"Chain_Next": ("chain next", "The next element of the chain this node belongs to.", CHAIN + " style=bold"),
}


class Group:
	"""A run of consecutive node kinds published as one page."""

	def __init__(self, name: str, title: str, kinds: List[str]) -> None:
		self.name = name
		self.title = title
		self.kinds = kinds


def LoadModel(root: Path):
	"""
	Parse the Ada meta-model with :mod:`scripts.pnodes`.

	:param root: The repository root, containing ``scripts`` and ``src/vhdl``.
	:returns:    The tuple ``(kinds, ranges, funcs, nodes)`` as :func:`scripts.pnodes.parse_files` returns it.
	"""
	sys.path.insert(0, str(root / "scripts"))
	import pnodes

	vhdl = root / "src" / "vhdl"
	(_, _, kinds, ranges, funcs, nodes) = pnodes.parse_files(
		str(vhdl / "vhdl-nodes.ads"),
		str(vhdl / "vhdl-nodes.adb.in"),
		str(vhdl / "vhdl-nodes.ads"),
		str(vhdl / "vhdl-nodes.adb.in"),
		"vhdl-nodes_meta",
	)
	return (kinds, ranges, funcs, nodes)


def BuildGroups(kinds: List[str], ranges: Dict[str, List[str]]) -> List[Group]:
	"""
	Partition the node kinds into pages, following the order of the ``Iir_Kind`` enumeration.

	``Iir_Kinds_*`` are contiguous intervals over that enumeration and they nest, so they are not a
	partition: 58 kinds are in no range at all and some are in six. The *maximal* ranges - those not
	contained in a larger one - are used as page boundaries, each kind is assigned to the first one that
	contains it, and the kinds between two ranges keep their place in the enumeration as a page of their
	own. Tristan's ordering is therefore preserved and every kind appears exactly once.

	:param kinds:  All node kinds, in declaration order.
	:param ranges: Mapping from a ``Iir_Kinds_*`` range name to the kinds it covers.
	:returns:      The groups, in declaration order.
	"""
	index = {k: i for i, k in enumerate(kinds)}
	spans = {r: (index[v[0]], index[v[-1]]) for r, v in ranges.items() if v}

	maximal = [
		r
		for r, (a, b) in spans.items()
		if not any(o != r and c <= a and b <= d and (d - c) > (b - a) for o, (c, d) in spans.items())
	]
	maximal.sort(key=lambda r: spans[r])

	owner: List[Nullable[str]] = [None] * len(kinds)
	for r in maximal:
		(a, b) = spans[r]
		for i in range(a, b + 1):
			if owner[i] is None:
				owner[i] = r

	groups: List[Group] = []
	start = 0
	while start < len(kinds):
		current = owner[start]
		stop = start
		while stop + 1 < len(kinds) and owner[stop + 1] == current:
			stop += 1
		members = kinds[start : stop + 1]
		if current is None:
			name = f"other-{start}"
			title = f"Other nodes ({members[0]} ... {members[-1]})" if len(members) > 1 else members[0].replace("_", " ")
		else:
			name = current.lower().replace("_", "-")
			title = current.replace("_", " ")
		groups.append(Group(name, title, members))
		start = stop + 1

	return groups


def EscapeReST(text: str) -> str:
	"""
	Escape the characters ReST would read as markup.

	The text comes verbatim from an Ada comment, which is prose rather than ReST, so a stray backtick or a table drawn
	with ``|`` would otherwise be parsed as an unterminated role or as substitution references. ``Iir_Kind_Entity_Class``
	writes ``` `<>' ``` and ``Iir_Kind_Constant_Declaration`` draws a table with ``|``.

	:param text: The comment text to escape.
	:returns:    The same text, safe to place in a ReST document.
	"""
	for character in ("\\", "`", "|", "*"):
		text = text.replace(character, "\\" + character)
	return text


def Reflow(lines: List[str], indent: str = "", width: int = 120) -> List[str]:
	"""
	Reflow a comment block into ReST paragraphs, keeping literal blocks literal.

	A paragraph is reflowed to ``width``. A run of lines that looks like a grammar production - it
	contains ``::=`` or is indented relative to the paragraph it follows - is emitted as a literal block
	instead, because reflowing ``design_file ::= design_unit { design_unit }`` would destroy it.

	:param lines:  The comment lines, without the leading ``--``.
	:param indent: Indentation to put in front of every emitted line.
	:param width:  Maximum line length, including the indentation.
	:returns:      The rendered ReST lines.
	"""
	result: List[str] = []
	paragraph: List[str] = []
	literal: List[str] = []

	def flushParagraph() -> None:
		if paragraph:
			text = " ".join(paragraph)
			result.extend(indent + l for l in wrap(text, width=width - len(indent)) or [""])
			result.append("")
			paragraph.clear()

	def flushLiteral() -> None:
		if literal:
			result.append(indent + "::")
			result.append("")
			result.extend((indent + "   " + l).rstrip() for l in literal)
			result.append("")
			literal.clear()

	for line in lines:
		if not line:
			flushParagraph()
			flushLiteral()
		elif "::=" in line or (literal and line.startswith(" ")):
			flushParagraph()
			literal.append(line)
		else:
			flushLiteral()
			paragraph.append(line)

	flushParagraph()
	flushLiteral()
	while result and not result[-1]:
		result.pop()
	return result


def FieldRows(node, funcs) -> List[Tuple[str, str, str, str]]:
	"""
	Collect the fields of a node as ``(accessor, physical field, type, access label)`` rows.

	:param node:  The :class:`~scripts.pnodes.NodeDesc` to describe.
	:param funcs: All accessors, used to look up aliases.
	:returns:     One row per field, in the order the Ada source declares them.
	"""
	rows = []
	seen = set()
	for field in node.order:
		func = node.fields[field]
		if func is None or func.name in seen:
			continue
		seen.add(func.name)
		(label, _, _) = ACCESS_KINDS.get(func.acc, (func.acc, "", ""))
		rows.append((func.name, field, func.rtype, label))
	return rows


def NodeDiagram(node, funcs) -> List[str]:
	"""
	Draw the node as a record, with one port per field and an edge out of every field that is a link.

	The node is drawn the way its storage is laid out: the fields in the order the Ada source declares
	them, each one a port of a `record shaped <https://graphviz.org/doc/info/shapes.html>`__ node. A
	field that holds a link gets an edge leaving *from its own port*, so it is visible which slot the
	link lives in; a field holding a value - an identifier, a flag - is shown in place with no edge.

	The Ada accessors return a bare ``Iir`` for 237 of the 395 fields, so the *kind* a field points at
	is not knowable from the source. What is knowable, and what the edges carry, is the nature of the
	link: owned, a reference, a forward reference resolved during semantic analysis, or the head of a
	chain.

	:param node:  The :class:`~scripts.pnodes.NodeDesc` to draw.
	:param funcs: All accessors.
	:returns:     The lines of a ``.. graphviz::`` directive, or an empty list if the node has no fields.
	"""
	fields = []
	seen = set()
	for field in node.order:
		func = node.fields[field]
		if func is None or func.name in seen:
			continue
		seen.add(func.name)
		fields.append(func)

	if not fields:
		return []

	# A record label separates its fields with '|', so a field's text must not contain one.
	cells = "|".join(f"<{func.name}> {func.name}" for func in fields)
	label = f"{{{node.name}|{{{cells}}}}}"

	lines = [
		".. graphviz::",
		"",
		"   digraph {",
		'      rankdir="LR"',
		'      node [shape=record fontname="monospace" fontsize=10]',
		'      edge [fontname="monospace" fontsize=9]',
		"",
		f'      "{node.name}" [style=filled fillcolor="#eeeeec" label="{label}"]',
	]
	for func in fields:
		if func.rtype not in LINK_TYPES:
			continue
		(_, _, style) = ACCESS_KINDS.get(func.acc, (func.acc or "owned", "", ""))
		target = f"{node.name}__{func.name}"
		shape = "box3d" if func.rtype in ("Iir_List", "Iir_Flist") else "box"
		lines.append(f'      "{target}" [shape={shape} label="{func.rtype}"]')
		lines.append(f'      "{node.name}":{func.name} -> "{target}" [{style}]')
	lines.extend(["   }", ""])
	return lines


def RenderNode(node, funcs, accessorDescriptions: Dict[str, List[str]]) -> List[str]:
	"""
	Render one node kind as a ReST section.

	:param node:                 The :class:`~scripts.pnodes.NodeDesc` to render.
	:param funcs:                All accessors.
	:param accessorDescriptions: Mapping from an accessor name to its shared description.
	:returns:                    The ReST lines of the section.
	"""
	title = f"Iir_Kind_{node.name}"
	lines = [f".. _INT:AST:{node.name}:", "", title, "=" * len(title), ""]

	if node.description:
		lines.extend(Reflow(node.description))
		lines.append("")

	lines.append(f"Format: ``{node.format}``")
	lines.append("")

	rows = FieldRows(node, funcs)
	if rows:
		lines.extend([".. list-table::", "   :header-rows: 1", "   :widths: 30 12 18 12 28", ""])
		lines.extend(["   * - Accessor", "     - Field", "     - Type", "     - Access", "     - Description"])
		for (accessor, field, rtype, access) in rows:
			description = node.field_descriptions.get(accessor) or accessorDescriptions.get(accessor) or []
			text = EscapeReST(" ".join(l for l in description if l)) or "—"
			lines.extend([
				f"   * - :ref:`{accessor} <INT:AST:field:{accessor}>`",
				f"     - ``{field}``",
				f"     - ``{rtype}``",
				f"     - {access}",
				f"     - {text}",
			])
		lines.append("")

	if node.disabled_fields:
		lines.extend([".. note::", ""])
		for (name, description) in node.disabled_fields:
			text = " ".join(l for l in description if l)
			lines.extend(Reflow([f"``Get/Set_{name}`` is present in the source but commented out. {text}"], "   "))
			lines.append("")

	lines.extend(NodeDiagram(node, funcs))
	return lines


def RenderGroup(group: Group, nodes, funcs, accessorDescriptions) -> str:
	"""
	Render one group of node kinds as a page.

	:param group:                The group to render.
	:param nodes:                Mapping from kind name to :class:`~scripts.pnodes.NodeDesc`.
	:param funcs:                All accessors.
	:param accessorDescriptions: Mapping from an accessor name to its shared description.
	:returns:                    The contents of the ``.rst`` file.
	"""
	lines = [
		".. # This page is generated by doc/ASTReference.py when Sphinx starts.  Do not edit it.",
		"",
		group.title,
		"#" * len(group.title),
		"",
	]
	# Every page draws diagrams, so every page repeats the colour key.  Graphviz names its output
	# by the hash of the source, so the same legend on 50 pages is rendered once and reused.
	lines.extend(
		Reflow(
			[
				"The diagrams below draw one box per node kind, with one port per field. The edge colours say "
				"how a link is held; they are listed in full on the :ref:`INT:AST:Fields` page.",
			]
		)
	)
	lines.append("")
	lines.extend(Legend())

	for kind in group.kinds:
		lines.extend(RenderNode(nodes[kind], funcs, accessorDescriptions))
		lines.append("")
	return "\n".join(lines) + "\n"


def Legend() -> List[str]:
	"""
	Draw one edge of each access kind, so the colours on the node diagrams can be read.

	:returns: The lines of a ``.. graphviz::`` directive.
	"""
	lines = [
		".. graphviz::",
		"",
		"   digraph {",
		'      rankdir="LR"',
		"      nodesep=0.1",
		'      node [shape=point width=0.06 color="#888a85"]',
		'      edge [fontname="monospace" fontsize=10 minlen=3]',
		"",
	]
	for (index, (access, (label, _, style))) in enumerate(ACCESS_KINDS.items()):
		lines.append(f'      l{index}a -> l{index}b [label="{label}" {style}]')
	lines.extend(["   }", ""])
	return lines


def RenderAccessors(funcs) -> str:
	"""
	Render the accessor reference: every ``Get_``/``Set_`` pair once, with its shared description.

	The node pages link here, so a field that appears on forty kinds is described in one place.

	:param funcs: All accessors.
	:returns:     The contents of the ``.rst`` file.
	"""
	title = "Fields"
	lines = [
		".. # This page is generated by doc/ASTReference.py when Sphinx starts.  Do not edit it.",
		"",
		".. _INT:AST:Fields:",
		"",
		title,
		"#" * len(title),
		"",
	]
	lines.extend(
		Reflow(
			[
				"Every field of every node is read and written by a ``Get_``/``Set_`` pair. A field means the "
				"same thing on every kind that has it, so it is described here once and the node pages link "
				"back to this page.",
				"",
				"The access column says how the link is held:",
			]
		)
	)
	lines.append("")
	for (access, (label, description, _)) in ACCESS_KINDS.items():
		lines.append(f"* **{label}** - {description}")
	lines.append("")
	lines.extend(
		Reflow(["The node diagrams use the same names, drawn as edge colours:"])
	)
	lines.append("")
	lines.extend(Legend())

	for func in sorted(funcs, key=lambda f: f.name):
		(label, _, _) = ACCESS_KINDS.get(func.acc, (func.acc, "", ""))
		heading = f"Get_{func.name} / Set_{func.name}"
		lines.extend([f".. _INT:AST:field:{func.name}:", "", heading, "=" * len(heading), ""])
		if func.description:
			lines.extend(Reflow(func.description))
			lines.append("")
		lines.append(f"Type: ``{func.rtype}`` — Access: {label} — Slot: " + ", ".join(f"``{f}``" for f in func.fields))
		lines.append("")

	return "\n".join(lines) + "\n"


def Generate(root: Path, outputDirectory: Path) -> List[Group]:
	"""
	Write one page per group of node kinds.

	:param root:            The repository root.
	:param outputDirectory: Directory the pages are written to. It is created if needed.
	:returns:               The groups that were written, in declaration order.
	"""
	(kinds, ranges, funcs, nodes) = LoadModel(root)
	groups = BuildGroups(kinds, ranges)
	accessorDescriptions = {f.name: f.description for f in funcs}

	outputDirectory.mkdir(parents=True, exist_ok=True)
	for stale in outputDirectory.glob("*.rst"):
		stale.unlink()

	# The toctree globs this directory, so the file names carry the position of
	# the group in the Iir_Kind enumeration.
	(outputDirectory / "00-fields.rst").write_text(RenderAccessors(funcs), encoding="utf-8")
	for (position, group) in enumerate(groups, start=1):
		(outputDirectory / f"{position:02d}-{group.name}.rst").write_text(
			RenderGroup(group, nodes, funcs, accessorDescriptions), encoding="utf-8"
		)

	return groups
