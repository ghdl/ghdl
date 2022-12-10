package MemorySupportPkg is

  subtype MemoryBaseType is integer_vector ;
  type    MemBlockType   is array (integer range <>) of MemoryBaseType ;

  function InitMemoryBaseType_X(BlockWidth, BaseWidth : integer) return MemBlockType ;

end MemorySupportPkg ;

package body MemorySupportPkg is


-- Works
--  ------------------------------------------------------------
--  function InitMemoryBaseType_X(BlockWidth, BaseWidth : integer) return MemBlockType is
--  ------------------------------------------------------------
--    constant BaseU       : MemoryBaseType(0 to BaseWidth-1)  := (others => -1) ;
--  begin
--    return MemBlockType'(0 to 2**BlockWidth-1 => BaseU) ;
--  end function InitMemoryBaseType_X ;

-- Fails
  ------------------------------------------------------------
  function InitMemoryBaseType_X(BlockWidth, BaseWidth : integer) return MemBlockType is
  ------------------------------------------------------------
    constant BaseU       : MemoryBaseType  := (0 to BaseWidth-1 => -1) ;
  begin
    return MemBlockType'(0 to 2**BlockWidth-1 => BaseU) ;
  end function InitMemoryBaseType_X ;

-- Fails
--   ------------------------------------------------------------
--   function InitMemoryBaseType_X(BlockWidth, BaseWidth : integer) return MemBlockType is
--   ------------------------------------------------------------
--     constant BaseU       : MemoryBaseType(0 to BaseWidth-1)   := (others => -1) ;
--     constant BlockWidthU : MemBlockType(0 to 2**BlockWidth-1) := (others => BaseU) ;
-- --  --!! Also Fails    constant BlockWidthU : MemBlockType    := (0 to 2**BlockWidth-1 => BaseU) ;
--   begin
--     return BlockWidthU ;
--   end function InitMemoryBaseType_X ;
end MemorySupportPkg ;
