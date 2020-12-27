from pydecor import export

from pyVHDLModel.VHDLModel import Modes

from libghdl.thin       import name_table
from libghdl.thin.vhdl  import nodes

__all__ = []
__api__ = __all__


@export
class GHDLBaseException(Exception):
	pass


@export
class LibGHDLException(GHDLBaseException):
	pass


@export
class GHDLException(GHDLBaseException):
	pass


@export
class GHDLMixin:
	_MODE_TRANSLATION = {
		nodes.Iir_Mode.In_Mode:      Modes.In,
		nodes.Iir_Mode.Out_Mode:     Modes.Out,
		nodes.Iir_Mode.Inout_Mode:   Modes.InOut,
		nodes.Iir_Mode.Buffer_Mode:  Modes.Buffer,
		nodes.Iir_Mode.Linkage_Mode: Modes.Linkage
	}

	@classmethod
	def _ghdlNodeToName(cls, node):
		"""Return the python string from node :param:`node` identifier"""
		return name_table.Get_Name_Ptr(nodes.Get_Identifier(node)).decode("utf-8")

	@classmethod
	def _ghdlPortToMode(cls, port):
		"""Return the mode of a port."""
		try:
			return cls._MODE_TRANSLATION[nodes.Get_Mode(port)]
		except KeyError:
			raise LibGHDLException("Unknown mode.")
