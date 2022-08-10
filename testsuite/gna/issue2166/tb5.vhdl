use std.textio.all ;
library IEEE ;
  use IEEE.std_logic_1164.all ;
  use IEEE.numeric_std.all ;
  use IEEE.numeric_std_unsigned.all ;

package MemoryPkg is
  type MemoryPType is protected
    procedure MemInit (ID : integer ;  AddrWidth, DataWidth  : integer);

    procedure MemWrite (
      ID    : integer ;
      Addr  : std_logic_vector ;
      Data  : std_logic_vector
    ) ;
  end protected MemoryPType ;
end;

package body MemoryPkg is
  constant BLOCK_WIDTH : integer := 10 ;

  function InitMemoryBaseType(Size : integer) return integer_vector is
  begin
    return (Size / 32 downto 1 => 0);
  end;

  type MemoryPType is protected body

    subtype MemoryBaseType is integer_vector ;
    type MemBlockType      is array (integer range <>) of MemoryBaseType ;
    type MemBlockPtrType   is access MemBlockType ;
    type MemArrayType      is array (integer range <>) of MemBlockPtrType ;
    type MemArrayPtrType   is access MemArrayType ;

    type MemStructType is record
      MemArrayPtr         : MemArrayPtrType ;
      AddrWidth           : integer ;
      DataWidth           : natural ;
      BlockWidth          : natural ;
      MemoryBaseTypeWidth : natural ;
    end record MemStructType ;

    -- New Structure
    type     ItemArrayType    is array (integer range <>) of MemStructType ;
    type     ItemArrayPtrType is access ItemArrayType ;

    variable Template         : ItemArrayType(1 to 1) := (1 => (NULL, -1, 1, 0, 0)) ;  -- Work around for QS 2020.04 and 2021.02
    variable MemStructPtr     : ItemArrayPtrType := new ItemArrayType'(Template) ;
    variable NumItems         : integer := 0 ;
    constant MIN_NUM_ITEMS    : integer := 32 ; -- Min amount to resize array

   ------------------------------------------------------------
    -- PT Local
    procedure MemInit (ID : integer ;  AddrWidth, DataWidth  : integer ) is
    ------------------------------------------------------------
      constant ADJ_BLOCK_WIDTH : integer := minimum(BLOCK_WIDTH, AddrWidth) ;
    begin
      if AddrWidth <= 0 then
        return ;
      end if ;
      if DataWidth <= 0 then
        return ;
      end if ;

      MemStructPtr(ID).AddrWidth           := AddrWidth ;
      MemStructPtr(ID).DataWidth           := DataWidth ;
      MemStructPtr(ID).MemoryBaseTypeWidth := (DataWidth + 31) / 32;
      MemStructPtr(ID).BlockWidth          := ADJ_BLOCK_WIDTH ;
      MemStructPtr(ID).MemArrayPtr         := new MemArrayType(0 to 2**(AddrWidth-ADJ_BLOCK_WIDTH)-1) ;
    end procedure MemInit ;

    ------------------------------------------------------------
    procedure MemWrite (
    ------------------------------------------------------------
      ID    : integer ;
      Addr  : std_logic_vector ;
      Data  : std_logic_vector
    ) is
      variable BlockWidth : integer ;
      variable MemoryBaseTypeWidth : integer ;
      variable BlockAddr, WordAddr  : integer ;
    begin
      BlockWidth := MemStructPtr(ID).BlockWidth ;
      MemoryBaseTypeWidth := MemStructPtr(ID).MemoryBaseTypeWidth ;
      BlockAddr  := 0 ;

      -- If empty, allocate a memory block
      if MemStructPtr(ID).MemArrayPtr(BlockAddr) = NULL then
        MemStructPtr(ID).MemArrayPtr(BlockAddr) := new MemBlockType(0 to 2**BlockWidth-1)(MemoryBaseTypeWidth downto 1) ; -- => InitMemoryBaseType(Data'length)) ;
        MemStructPtr(ID).MemArrayPtr(BlockAddr)(0 to 2**BlockWidth-1) := (0 to 2**BlockWidth-1 => InitMemoryBaseType(Data'length)) ;
      end if ;
    end procedure MemWrite ;
  end protected body MemoryPType ;
end;

library ieee ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all ;
use work.MemoryPkg.all;

entity tb3 is
end;

architecture behav of tb3 is
  shared variable MemoryStore : MemoryPType ;
begin
  process
  begin
    MemoryStore.MemInit(1, AddrWidth => 20, DataWidth => 16);

    MemoryStore.MemWrite(1, x"00000", x"0000");
    wait;
  end process;
end ;
