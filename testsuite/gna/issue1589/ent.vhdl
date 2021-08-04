library ieee;
use ieee.std_logic_1164.all;

entity ent is
end ent;

architecture ent of ent is

  type integer_array_t is array (natural range <>) of integer;
  type integer_2d_array_t is array (natural range <>) of integer_array_t;
  type std_logic_array_t is array (natural range <>) of std_logic_vector;

  constant test0 : integer_array_t := (0, 1, 2, 3);                                        -- OK

  -- array of arrays fail if type declaration has no size whatsoever
  constant test1 : integer_2d_array_t := ((0, 1, 2, 3), (0, 1, 2, 3));                     -- Fails
  constant test2 : std_logic_array_t := ( x"10", x"11");                                   -- Fails

  -- Constraining the std_logic_vector but not the array length also fails
  subtype byte is std_logic_vector(7 downto 0);
  constant test3 : std_logic_array_t := ( byte'(x"10"), byte'(x"10"));                     -- Fails

  -- Constraining the std_logic_vector via subtype AND the array length also fails
  constant test4 : std_logic_array_t(0 to 1) := ( byte'(x"10"), byte'(x"10"));             -- Fails

  -- If everything in constrained at the type declaration it works:
  constant test5 : std_logic_array_t(0 to 1)(7 downto 0) := ( byte'(x"10"), byte'(x"10")); -- Works
  constant test6 : std_logic_array_t(0 to 1)(byte'range) := ( byte'(x"10"), byte'(x"10")); -- Works
  constant test7 : std_logic_array_t(0 to 1)(byte'range) := ( x"10", x"10");               -- Works

  -- Interestingly, if array length is left unconstrained but the with not it works:
  constant test8 : std_logic_array_t(open)(7 downto 0) := ( x"10", x"11");                 -- Works

begin
end ent;
