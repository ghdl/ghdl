library ieee ;
use ieee.std_logic_1164.all ;

entity tb3 is
end;

architecture behav of tb3 is
begin
  process
    function InitMemoryBaseType(Size : integer) return integer_vector is
    begin
      return ((Size + 31) / 32 downto 1 => 0);
    end;

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

    variable MemStructPtr     : MemStructType := (NULL, -1, 1, 0, 0);

    procedure MemInit (AddrWidth, DataWidth  : integer ) is
      constant ADJ_BLOCK_WIDTH : integer := 10;
    begin
      MemStructPtr.AddrWidth           := AddrWidth ;
      MemStructPtr.DataWidth           := DataWidth ;
      MemStructPtr.MemoryBaseTypeWidth := (DataWidth + 31) / 32;
      MemStructPtr.BlockWidth          := ADJ_BLOCK_WIDTH ;
      MemStructPtr.MemArrayPtr         := new MemArrayType(0 to 2**(AddrWidth-ADJ_BLOCK_WIDTH)-1) ;
    end procedure MemInit ;

    ------------------------------------------------------------
    procedure MemWrite (
    ------------------------------------------------------------
      Addr  : std_logic_vector ;
      Data  : std_logic_vector
    ) is
      variable BlockWidth : integer ;
      variable MemoryBaseTypeWidth : integer ;
      variable BlockAddr, WordAddr  : integer ;
    begin
      BlockWidth := MemStructPtr.BlockWidth ;
      MemoryBaseTypeWidth := MemStructPtr.MemoryBaseTypeWidth ;
      BlockAddr  := 0 ;

      assert MemoryBaseTypeWidth = 1;
      
      -- If empty, allocate a memory block
      if MemStructPtr.MemArrayPtr(BlockAddr) = NULL then
        MemStructPtr.MemArrayPtr(BlockAddr) := new MemBlockType(0 to 2**BlockWidth-1)(MemoryBaseTypeWidth downto 1) ;
        MemStructPtr.MemArrayPtr(BlockAddr)(0 to 2**BlockWidth-1) := (0 to 2**BlockWidth-1 => InitMemoryBaseType(Data'length)) ;
      end if ;
    end procedure MemWrite ;
  begin
    MemInit(AddrWidth => 20, DataWidth => 16);

    MemWrite(x"00000", x"0000");
    wait;
  end process;
end ;
