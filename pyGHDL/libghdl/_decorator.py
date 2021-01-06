from functools import wraps
from typing import Callable, List

from pydecor import export


@export
def EnumLookupTable(cls) -> Callable:
    """
    Decorator to precalculate a enum lookup table (LUT) for enum position to
    enum literal name.

    .. todo:: Make compatible to chained decorators

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

        def wrapper(id: int) -> str:
            # function that replaces the placeholder function
            return __lut[id]

        return wrapper

    return decorator
