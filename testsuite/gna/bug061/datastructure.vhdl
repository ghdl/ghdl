library ieee;
use ieee.std_logic_1164.all;



package DataStructures is


  -- Simple hashing functions
  function Modulo_Int (d : integer; size : positive) return natural;
  function Modulo (d : string;  size : positive) return natural;

  -- Dictionaries
  package Integer_Integer_Dict_Pkg is new work.corelib_Dict
    generic map (KEY_TYPE   => integer,
                 VALUE_TYPE => integer,
                 to_hash    => Modulo_Int);

  package Integer_StdLogicVector_Dict_Pkg is new work.corelib_Dict
    generic map (KEY_TYPE   => integer,
                 VALUE_TYPE => std_logic_vector,
                 to_hash    => Modulo_Int);

  package String_String_Dict_Pkg is new work.corelib_Dict
    generic map (KEY_TYPE   => string,
                 VALUE_TYPE => string,
                 to_hash    => Modulo);

  package String_StdLogicVector_Dict_Pkg is new work.corelib_Dict
    generic map (KEY_TYPE   => string,
                 VALUE_TYPE => std_logic_vector,
                 to_hash    => Modulo);

  -- Aliases for convenience reasons
  alias Integer_Integer_Dict is Integer_Integer_Dict_Pkg.PT_DICT;
  alias Integer_Slv_Dict     is Integer_StdLogicVector_Dict_Pkg.PT_DICT;
  alias String_String_Dict   is String_String_Dict_Pkg.PT_DICT;
  alias String_Slv_Dict      is String_StdLogicVector_Dict_Pkg.PT_DICT;


end package;



package body DataStructures is


  -- Simple modulo function for integers
  function Modulo_int (d : integer; size : positive) return natural is
  begin
    return d mod size;
  end function Modulo_Int;

  -- Simple modulo function for ISO 8859 Latin-1 8-bit strings
  -- of arbitrary length (>= VHDL 93)
  function Modulo (d : string; size : positive) return natural is
    variable hash : natural := 0;
  begin
    assert size <= ((natural'high - 255) / 256 + 1)
      report Modulo[string, natural return natural]'instance_name & ": size parameter too large, possible overflow"
                   severity failure;
    for i in d'range loop
      hash := (hash * 256 + Character'Pos (d(i))) mod size;
    end loop;
    return hash;
  end function Modulo;


end package body DataStructures;
