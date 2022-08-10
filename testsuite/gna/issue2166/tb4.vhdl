library OSVVM ;

use OSVVM.MemorySupportPkg.all ;

use std.textio.all ;
library IEEE ;
  use IEEE.std_logic_1164.all ;
  use IEEE.numeric_std.all ;
  use IEEE.numeric_std_unsigned.all ;

package MemoryGenericPkg is
  generic (
--    type integer_vector ;
    function SizeMemoryBaseType(Size : integer) return integer ; -- is <> ;
    function ToMemoryBaseType  (A : std_logic_vector) return integer_vector ; -- is <> ;
    function FromMemoryBaseType(A : integer_vector ; Size : integer) return std_logic_vector ; -- is <> ;
    function InitMemoryBaseType(Size : integer) return integer_vector -- is <>
  ) ;

  type MemoryPType is protected

    ------------------------------------------------------------
    impure function NewID (
      Name                : String ;
      AddrWidth           : integer ;
      DataWidth           : integer 
    ) return integer ;

    ------------------------------------------------------------
    procedure MemWrite (
      ID    : integer ;
      Addr  : std_logic_vector ;
      Data  : std_logic_vector
    ) ;
    procedure MemRead (
      ID    : in integer ;
      Addr  : in  std_logic_vector ;
      Data  : out std_logic_vector
    ) ;
  end protected MemoryPType ;

end MemoryGenericPkg ;

package body MemoryGenericPkg is
  constant BLOCK_WIDTH : integer := 10 ;

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
    constant MEM_STRUCT_PTR_LEFT : integer := Template'left ;
    variable MemStructPtr     : ItemArrayPtrType := new ItemArrayType'(Template) ;
    variable NumItems         : integer := 0 ;
--    constant MIN_NUM_ITEMS    : integer := 4 ; -- Temporarily small for testing
    constant MIN_NUM_ITEMS    : integer := 32 ; -- Min amount to resize array

    ------------------------------------------------------------
    -- Package Local
    function NormalizeArraySize( NewNumItems, MinNumItems : integer ) return integer is
    ------------------------------------------------------------
      variable NormNumItems : integer := NewNumItems ;
      variable ModNumItems  : integer := 0;
    begin
      ModNumItems := NewNumItems mod MinNumItems ;
      if ModNumItems > 0 then
        NormNumItems := NormNumItems + (MinNumItems - ModNumItems) ;
      end if ;
      return NormNumItems ;
    end function NormalizeArraySize ;

    ------------------------------------------------------------
    -- Package Local
    procedure GrowNumberItems (
    ------------------------------------------------------------
      variable ItemArrayPtr     : InOut ItemArrayPtrType ;
      variable NumItems         : InOut integer ;
      constant GrowAmount       : in integer ;
--      constant NewNumItems      : in integer ;
--      constant CurNumItems      : in integer ;
      constant MinNumItems      : in integer
    ) is
      variable oldItemArrayPtr  : ItemArrayPtrType ;
      variable NewNumItems : integer ;
    begin
      NewNumItems := NumItems + GrowAmount ;
      -- Array Allocated in declaration to have a single item, but no items (historical mode)
      -- if ItemArrayPtr = NULL then
      --  ItemArrayPtr := new ItemArrayType(1 to NormalizeArraySize(NewNumItems, MinNumItems)) ;
      -- elsif NewNumItems > ItemArrayPtr'length then
      if NewNumItems > ItemArrayPtr'length then
        oldItemArrayPtr := ItemArrayPtr ;
        ItemArrayPtr := new ItemArrayType(1 to NormalizeArraySize(NewNumItems, MinNumItems)) ;
        ItemArrayPtr.all(1 to NumItems) := oldItemArrayPtr.all(1 to NumItems) ;
        deallocate(oldItemArrayPtr) ;
      end if ;
      NumItems := NewNumItems ;
    end procedure GrowNumberItems ;

   ------------------------------------------------------------
    -- PT Local
    procedure MemInit (ID : integer ;  AddrWidth, DataWidth  : integer ) is
    ------------------------------------------------------------
      constant ADJ_BLOCK_WIDTH : integer := minimum(BLOCK_WIDTH, AddrWidth) ;
    begin
      if AddrWidth <= 0 then
        return ;
      end if ;
--      if DataWidth <= 0 or DataWidth > 31 then
--        Alert(MemStructPtr(ID).AlertLogID, "MemoryPkg.MemInit/NewID.  DataWidth = " & to_string(DataWidth) & " must be > 0 and <= 31.", FAILURE) ;
      if DataWidth <= 0 then
        return ;
      end if ;

      MemStructPtr(ID).AddrWidth           := AddrWidth ;
      MemStructPtr(ID).DataWidth           := DataWidth ;
      MemStructPtr(ID).MemoryBaseTypeWidth := SizeMemoryBaseType(DataWidth) ;
      MemStructPtr(ID).BlockWidth          := ADJ_BLOCK_WIDTH ;
      MemStructPtr(ID).MemArrayPtr         := new MemArrayType(0 to 2**(AddrWidth-ADJ_BLOCK_WIDTH)-1) ;
    end procedure MemInit ;

    ------------------------------------------------------------
    impure function NewID (
    ------------------------------------------------------------
      Name                : String ;
      AddrWidth           : integer ;
      DataWidth           : integer
    ) return integer is
      variable NameID              : integer ;
    begin
      -- Add New Memory to Structure
      GrowNumberItems(MemStructPtr, NumItems, GrowAmount => 1, MinNumItems => MIN_NUM_ITEMS) ;
      -- Construct Memory, Reports agains AlertLogID
      MemInit(NumItems, AddrWidth, DataWidth) ;
      -- Check NameStore Index vs MemoryIndex
      return NumItems ;
    end function NewID ;

    ------------------------------------------------------------
    -- PT Local
    impure function IdOutOfRange(
    ------------------------------------------------------------
      constant ID    : in integer ;
      constant Name  : in string
    ) return boolean is
    begin
      return ID < MemStructPtr'Low or ID > MemStructPtr'High;
    end function IdOutOfRange ;

    ------------------------------------------------------------
    procedure MemWrite (
    ------------------------------------------------------------
      ID    : integer ;
      Addr  : std_logic_vector ;
      Data  : std_logic_vector
    ) is
      variable BlockWidth : integer ;
      variable MemoryBaseTypeWidth : integer ;
--      constant BlockWidth : integer := MemStructPtr(ID).BlockWidth;
      variable BlockAddr, WordAddr  : integer ;
      alias aAddr : std_logic_vector (Addr'length-1 downto 0) is Addr ;
--      subtype MemBlockSubType is MemBlockType(0 to 2**BlockWidth-1) ;
    begin
      if IdOutOfRange(ID, "MemWrite") then
        return ;
      end if ;
      BlockWidth := MemStructPtr(ID).BlockWidth ;
      MemoryBaseTypeWidth := MemStructPtr(ID).MemoryBaseTypeWidth ;

      -- Check Bounds of Address and if memory is initialized
      if Addr'length /= MemStructPtr(ID).AddrWidth then
        return ;
      end if ;

      -- Check Bounds on Data
      if Data'length /= MemStructPtr(ID).DataWidth then
        return ;
      end if ;

      if is_X( Addr ) then
        return ;
      end if ;

      -- Slice out upper address to form block address
      if aAddr'high >= BlockWidth then
        BlockAddr := to_integer(aAddr(aAddr'high downto BlockWidth)) ;
      else
        BlockAddr  := 0 ;
      end if ;

      -- If empty, allocate a memory block
      if (MemStructPtr(ID).MemArrayPtr(BlockAddr) = NULL) then
--        MemStructPtr(ID).MemArrayPtr(BlockAddr) := new MemBlockType'(0 to 2**BlockWidth-1 => InitMemoryBaseType(Data'length)) ;
        MemStructPtr(ID).MemArrayPtr(BlockAddr) := new MemBlockType(0 to 2**BlockWidth-1)(MemoryBaseTypeWidth downto 1) ; -- => InitMemoryBaseType(Data'length)) ;
        MemStructPtr(ID).MemArrayPtr(BlockAddr)(0 to 2**BlockWidth-1) := (0 to 2**BlockWidth-1 => InitMemoryBaseType(Data'length)) ;
--KO2       MemStructPtr(ID).MemArrayPtr(BlockAddr)(0 to 2**BlockWidth-1) := (others => InitMemoryBaseType(Data'length)) ;

      end if ;

      -- Address of a word within a block
      WordAddr  := to_integer(aAddr(BlockWidth -1 downto 0)) ;

      -- Write to BlockAddr, WordAddr
      MemStructPtr(ID).MemArrayPtr(BlockAddr)(WordAddr) := ToMemoryBaseType(Data) ;
    end procedure MemWrite ;

    ------------------------------------------------------------
    procedure MemRead (
    ------------------------------------------------------------
      ID    : in integer ;
      Addr  : in  std_logic_vector ;
      Data  : out std_logic_vector
    ) is
      variable BlockWidth : integer ;
      variable BlockAddr, WordAddr  : integer ;
      alias aAddr : std_logic_vector (Addr'length-1 downto 0) is Addr ;
    begin
      if IdOutOfRange(ID, "MemRead") then
        return ;
      end if ;
      BlockWidth := MemStructPtr(ID).BlockWidth ;

      -- Check Bounds of Address and if memory is initialized
      if Addr'length /= MemStructPtr(ID).AddrWidth then
        Data := (Data'range => 'U') ;
        return ;
      end if ;

      -- Check Bounds on Data
      if Data'length /= MemStructPtr(ID).DataWidth then
        Data := (Data'range => 'U') ;
        return ;
      end if ;

      -- If Addr X, data = X
      if is_X( aAddr ) then
        Data := (Data'range => 'X') ;
        return ;
      end if ;

      -- Slice out upper address to form block address
      if aAddr'high >= BlockWidth then
        BlockAddr := to_integer(aAddr(aAddr'high downto BlockWidth)) ;
      else
        BlockAddr  := 0 ;
      end if ;

      -- Empty Block, return all U
      if (MemStructPtr(ID).MemArrayPtr(BlockAddr) = NULL) then
        Data := (Data'range => 'U') ;
        return ;
      end if ;

      -- Address of a word within a block
      WordAddr := to_integer(aAddr(BlockWidth -1 downto 0)) ;

      Data := FromMemoryBaseType(MemStructPtr(ID).MemArrayPtr(BlockAddr)(WordAddr), Data'length) ;

    end procedure MemRead ;
  end protected body MemoryPType ;
end MemoryGenericPkg ;

library OSVVM ;

use OSVVM.MemorySupportPkg.all ;

package MemoryPkg is new work.MemoryGenericPkg
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

entity tb3 is
end;

architecture behav of tb3 is
  shared variable MemoryStore : MemoryPType ;
begin
  process
    variable MemoryID : integer;
  begin
    MemoryID := MemoryStore.NewID(
      Name      => "my_sram",
      AddrWidth => 20,
      DataWidth => 16);

    MemoryStore.MemWrite(MemoryId, x"00000", x"0000");
    wait;
  end process;
end ;
