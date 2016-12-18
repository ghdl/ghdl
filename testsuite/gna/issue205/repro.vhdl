package SortListGenericPkg is
  generic (
    type ElementType;
    function "<"(L : ElementType; R : ElementType) return boolean;
    function "<="(L : ElementType; R : ElementType) return boolean
  );
end package;
