entity MemPkgReproducer6 is
end;
architecture behav of MemPkgReproducer6 is
  function InitMemoryBaseType(Size : integer) return integer_vector is
    
  begin
    return (1 to Size => 0);
  end InitMemoryBaseType;
  subtype MemoryBaseType is integer_vector ;
  type MemBlockType      is array (integer range <>) of MemoryBaseType ;
  type MemBlockPtrType   is access MemBlockType ;
begin
  process
    variable MemArr : MemBlockPtrType;
    variable BlockWidth, BaseWidth : natural;
  begin
    BlockWidth := 4;
    BaseWidth  := 32;
    MemArr := new MemBlockType'( 0 to BlockWidth - 1 => InitMemoryBaseType(BaseWidth));
    wait;
  end process;
end;
