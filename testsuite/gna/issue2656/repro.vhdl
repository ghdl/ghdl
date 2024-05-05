library ieee;
use ieee.std_logic_1164.all;

entity repro is
end;

architecture behav of repro is
begin
  process
    type mem_t is array (0 to 15) of std_logic_vector (7 downto 0);
    type mem_ptr is access mem_t;
    variable a : mem_ptr;
  begin
    a := new mem_t;
    a(a'range).all := (others => (others =>'1'));
    wait;
  end process;
end;
