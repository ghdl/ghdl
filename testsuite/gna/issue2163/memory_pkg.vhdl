library ieee;
use ieee.std_logic_1164.all;

package MemoryGenericPkg is
  generic (
    type MemoryBaseType ;
    function ToMemoryBaseType(A : std_logic_vector) return MemoryBaseType is <> ;
    function FromMemoryBaseType(A : MemoryBaseType ; Size : integer) return std_logic_vector is <> ;
    function InitMemoryBaseType(Size : integer) return MemoryBaseType is <> 
  ) ;
-- Stuff
end package MemoryGenericPkg ; 
