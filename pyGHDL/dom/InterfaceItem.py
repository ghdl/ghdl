from pydecor                import export

from pyVHDLModel.VHDLModel  import PortSignalInterfaceItem as VHDLModel_PortSignalInterfaceItem
from pyVHDLModel.VHDLModel  import GenericConstantInterfaceItem as VHDLModel_GenericConstantInterfaceItem

from pyGHDL.dom.Common      import GHDLMixin

__all__ = []
__api__ = __all__


@export
class GenericConstantInterfaceItem(VHDLModel_GenericConstantInterfaceItem, GHDLMixin):
	@classmethod
	def parse(cls, generic):
		name = cls._ghdlNodeToName(generic)
		mode = cls._ghdlPortToMode(generic)

		generic = cls(name, mode)

		return generic


@export
class PortSignalInterfaceItem(VHDLModel_PortSignalInterfaceItem, GHDLMixin):
	@classmethod
	def parse(cls, port):
		name = cls._ghdlNodeToName(port)
		mode = cls._ghdlPortToMode(port)

		port = cls(name, mode)

		return port
