library ieee;
use      ieee.std_logic_1164.all;

entity e is
end entity;

architecture a of e is
  type t_rec is record
    addr : std_logic_vector;
  end record;
  
  function init_signals(bits : natural) return t_rec is
    variable result : t_rec(addr(bits - 1 downto 0));
  begin
    result.addr   := (others => '0');
    -- result.addr   := (result.addr'range => '0');  -- this works
    return result;
  end function;
begin
end architecture;
