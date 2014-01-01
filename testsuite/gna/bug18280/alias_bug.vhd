--
-- <alias_bug.vhd>
--
--  Illustrates GHDL 0.29.1 WinXP problem with attributes and aliases 
--
--  Problem:
--     A signal attribute, placed after an alias on the signal, causes errors like this:
--
--     .\alias_bug.vhd:35:13: alias "address_ms" does not denote the entire object
--
--
--  Workaround: move the attribute before the alias
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity alias_bug is
end alias_bug;
     
architecture test of alias_bug is

  signal processor_address : std_logic_vector(15 downto 0);

  --
  -- if alias is _NOT_ declared, error goes away
  --
  alias  address_ms       : std_logic_vector(3 downto 0)   is   processor_address(15 downto 12);

  --
  -- if the keep attribute is placed _BEFORE_ the alias, no error occurs
  --
  attribute keep : boolean;
  attribute keep of processor_address: signal is TRUE;

  begin

    processor_address <= X"1234";

  end test;
