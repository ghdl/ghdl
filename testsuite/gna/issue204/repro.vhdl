package SortListGenericPkg is
  generic (
    type ElementType;
    type ArrayofElementType;
    function array_length(A : ArrayofElementType) return natural;
    function element_get(A : ArrayofElementType; index : natural) return ElementType
  );

  function inside (constant E : ElementType; constant A : in ArrayofElementType) return boolean ;
end package;

package body SortListGenericPkg is
  function inside (constant E : ElementType; constant A : in ArrayofElementType) return boolean is
  begin
    for i in 0 to array_length(A) - 1 loop  --A'range loop
      if E = element_get(A, i) then
        return TRUE ;
      end if ;
    end loop ;
    return FALSE ;
  end function inside ;
end package body;
