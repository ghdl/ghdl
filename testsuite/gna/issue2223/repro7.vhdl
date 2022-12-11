entity MemPkgReproducer7 is
end;

architecture behav of MemPkgReproducer7 is
  function InitMemoryBaseType(Size : integer) return integer_vector is
    
  begin
    return (1 to Size => 0);
  end InitMemoryBaseType;
  subtype MemoryBaseType is integer_vector ;
  type MemBlockType      is array (integer range <>) of MemoryBaseType ;
  type MemBlockPtrType   is access MemBlockType ;
  constant INIT_MEM_BASE : integer_vector := InitMemoryBaseType(32) ; 
begin
  process
    variable MemArr : MemBlockPtrType;
    variable BlockWidth, BaseWidth : natural;
  begin
    BlockWidth := 4;
    BaseWidth  := 32;
    MemArr := new MemBlockType'( 0 to BlockWidth - 1 => INIT_MEM_BASE);
    wait;
  end process;
end;
