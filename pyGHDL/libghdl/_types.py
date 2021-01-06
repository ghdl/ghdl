from typing import TypeVar

__all__ = [
	'ErrorIndex',
	'MessageIdWarnings',
	'NameId',
	'SourceFileEntry',
	'Iir',
	'IirKind'
]

ErrorIndex =  TypeVar('ErrorIndex', bound=int)
MessageIdWarnings = TypeVar('MessageIdWarnings', bound=int)
NameId = TypeVar('NameId', bound=int)
SourceFileEntry = TypeVar('SourceFileEntry', bound=int)

Iir = TypeVar('Iir', bound=int)
IirKind = TypeVar('IirKind', bound=int)
