from pydecor import export

from pyVHDLModel.VHDLModel import  Mode

from pyGHDL.libghdl      import LibGHDLException, name_table
from pyGHDL.libghdl.vhdl import nodes


__all__ = []

__MODE_TRANSLATION = {
    nodes.Iir_Mode.In_Mode:      Mode.In,
    nodes.Iir_Mode.Out_Mode:     Mode.Out,
    nodes.Iir_Mode.Inout_Mode:   Mode.InOut,
    nodes.Iir_Mode.Buffer_Mode:  Mode.Buffer,
    nodes.Iir_Mode.Linkage_Mode: Mode.Linkage
}

@export
def GetIirKindOfNode(node) -> nodes.Iir_Kind:
    kind: int = nodes.Get_Kind(node)
    return nodes.Iir_Kind(kind)


@export
def NodeToName(node) -> str:
    """Return the python string from node :obj:`node` identifier"""
    identifier = nodes.Get_Identifier(node)
    return name_table.Get_Name_Ptr(identifier)


@export
def GetModeOfNode(node) -> Mode:
    """Return the mode of a :obj:`port`."""
    try:
        return __MODE_TRANSLATION[nodes.Get_Mode(node)]
    except KeyError:
        raise LibGHDLException("Unknown mode.")
