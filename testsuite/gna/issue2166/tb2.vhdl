library OSVVM ;

use OSVVM.MemorySupportPkg.all ;

package MemoryPkg is new OSVVM.MemoryGenericPkg
  generic map (
--    MemoryBaseType      => MemoryBaseType_X,
    SizeMemoryBaseType  => SizeMemoryBaseType_X,
    ToMemoryBaseType    => ToMemoryBaseType_X,
    FromMemoryBaseType  => FromMemoryBaseType_X,
    InitMemoryBaseType  => InitMemoryBaseType_X
  ) ;

library ieee ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all ;
use work.MemoryPkg.all;

entity tb2 is
end;

architecture behav of tb2 is
begin
  process
    variable MemoryID : MemoryIDType;
  begin
    MemoryID := NewID(
      Name      => "my_sram",
      AddrWidth => 20,
      DataWidth => 16);

    MemWrite(MemoryId, x"00000", x"0000");
    wait;
  end process;
end ;
