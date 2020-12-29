from pydecor import export

from pyVHDLModel.VHDLModel import Entity        as VHDLModel_Entity
from pyVHDLModel.VHDLModel import Architecture  as VHDLModel_Architecture
from pyVHDLModel.VHDLModel import Package       as VHDLModel_Package
from pyVHDLModel.VHDLModel import PackageBody   as VHDLModel_PackageBody
from pyVHDLModel.VHDLModel import Context       as VHDLModel_Context
from pyVHDLModel.VHDLModel import Configuration as VHDLModel_Configuration

from pyGHDL.libghdl.vhdl   import nodes
import pyGHDL.libghdl.utils as pyutils

from pyGHDL.dom.Common     import GHDLMixin

__all__ = []
__api__ = __all__

from pyGHDL.dom.InterfaceItem import GenericConstantInterfaceItem, PortSignalInterfaceItem


@export
class Entity(VHDLModel_Entity, GHDLMixin):

	@classmethod
	def parse(cls, libraryUnit):
		name = cls._ghdlNodeToName(libraryUnit)
		entity = cls(name)

		cls.__parseGenerics(libraryUnit, entity)
		cls.__parsePorts(libraryUnit, entity)

		return entity

	@classmethod
	def __ghdlGetGenerics(cls, entity):
		return pyutils.chain_iter(nodes.Get_Generic_Chain(entity))

	@classmethod
	def __ghdlGetPorts(cls, entity):
		return pyutils.chain_iter(nodes.Get_Port_Chain(entity))

	@classmethod
	def __parseGenerics(cls, libraryUnit, entity):
		for generic in cls.__ghdlGetGenerics(libraryUnit):
			genericConstant = GenericConstantInterfaceItem.parse(generic)
			entity.GenericItems.append(genericConstant)

	@classmethod
	def __parsePorts(cls, libraryUnit, entity):
		for port in cls.__ghdlGetPorts(libraryUnit):
			signalPort = PortSignalInterfaceItem.parse(port)
			entity.PortItems.append(signalPort)

@export
class Architecture(VHDLModel_Architecture, GHDLMixin):
	def __init__(self, name: str, entityName: str):
		super().__init__(name)

		self.__entityName = entityName

	@classmethod
	def parse(cls, libraryUnit):
		name = cls._ghdlNodeToName(libraryUnit)
		entityName = cls._ghdlNodeToName(nodes.Get_Entity_Name(libraryUnit))

		return cls(name, entityName)

	def resolve(self):
		pass

@export
class Package(VHDLModel_Package, GHDLMixin):

	@classmethod
	def parse(cls, libraryUnit):
		name = cls._ghdlNodeToName(libraryUnit)
		return cls(name)

@export
class PackageBody(VHDLModel_PackageBody, GHDLMixin):

	@classmethod
	def parse(cls, libraryUnit):
		name = cls._ghdlNodeToName(libraryUnit)
		return cls(name)

@export
class Context(VHDLModel_Context, GHDLMixin):

	@classmethod
	def parse(cls, libraryUnit):
		name = cls._ghdlNodeToName(libraryUnit)
		return cls(name)

@export
class Configuration(VHDLModel_Configuration, GHDLMixin):

	@classmethod
	def parse(cls, libraryUnit):
		name = cls._ghdlNodeToName(libraryUnit)
		return cls(name)
