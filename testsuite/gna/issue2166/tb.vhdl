library ieee ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all ;
library OSVVM ;
use OSVVM.MemoryPkg.all;

entity tb is
end;

architecture behav of tb is
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
