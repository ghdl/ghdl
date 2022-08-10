entity repro2 is
end;

architecture behav of repro2 is
  function id(v : natural) return natural is
  begin
    return v;
  end id;

  function InitMemoryBaseType(Size : integer) return integer_vector is
  begin
    return (1 to Size => 0);
  end InitMemoryBaseType;

  subtype MemoryBaseType is integer_vector ;
  type MemBlockType      is array (integer range <>) of MemoryBaseType ;
  type MemBlockPtrType   is access MemBlockType ;
  type MemArrayType      is array (integer range <>) of MemBlockPtrType ;
  type MemArrayPtrType   is access MemArrayType ;

begin
  process
    variable MemArr : MemArrayPtrType;
    variable BlockWidth : natural;
  begin
    BlockWidth := 4;
    MemArr := new MemArrayType (0 to 7);
    MemArr(0) := new MemBlockType (0 to BlockWidth - 1)(0 to 31);
    MemArr(0)(0 to BlockWidth-1) :=
      (0 to BlockWidth-1 => InitMemoryBaseType(32)) ;
    wait;
  end process;
end;
