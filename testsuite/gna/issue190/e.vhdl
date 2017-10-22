library ieee;
use      ieee.std_logic_1164.all;

entity e is
end entity;

architecture a of e is
  signal foo : std_logic_vector(7 downto 0);
  signal bar : std_logic;
  
  procedure proc(constant addr : in std_logic_vector; signal output : out std_logic) is
    alias addr_alias : std_logic_vector(addr'length downto 0) is addr;  -- inherited non-staticness from addr?
  begin
    -- case addr(1 downto 0) is     -- not static, even if addr is of kind constant
    case addr_alias(1 downto 0) is  -- see local constant declarations
      when others =>  output <= 'X';
    end case;
  end procedure;
begin
end architecture;
