from pydecor import export

from pyVHDLModel.VHDLModel import (
    IntegerType as VHDLModel_IntegerType,
    SubType as VHDLModel_SubType,
    Expression,
)


@export
class IntegerType(VHDLModel_IntegerType):
    def __init__(self, typeName: str, leftBound: Expression, rightBound: Expression):
        super().__init__(typeName)
        self._leftBound = leftBound
        self._rightBound = rightBound


@export
class SubType(VHDLModel_SubType):
    def __init__(self, subtypeName: str):
        super().__init__(subtypeName)
