library ieee ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all ;
library OSVVM ;
use OSVVM.MemoryPkg.all;

Entity SRAM is
  port (
    Address : in     std_logic_vector (19 downto 0) ;
    Data    : inout  std_logic_vector (15 downto 0) ;
    nCE :in std_logic;
    nOE :in std_logic;
    nWE :in std_logic
    );
end SRAM ;

Architecture model of SRAM is
  signal MemoryID : MemoryIDType ;
  signal WriteEnable, ReadEnable  : std_logic ;
begin
  MemoryID <= NewID(
    Name      => SRAM'instance_name,
    AddrWidth => Address'length,
    DataWidth => Data'length) ;
  WriteEnable <= not nWE and not nCE ;

  RamWriteProc : process
  begin
    wait until falling_edge(WriteEnable) ;
    MemWrite(MemoryID, Address, Data) ;
  end process RamWriteProc  ;

  ReadEnable <= not nCE and not nOE ;

  ReadProc : process
  begin
    wait on Address, ReadEnable ;
    case ReadEnable is
      when '1' => Data <=  MemRead(MemoryID, Address) ;
      when '0' => Data <= (Data'range => 'Z') ;
      when others => Data <= (Data'range => 'X') ;
    end case ;
  end process ReadProc ;
end model ;
