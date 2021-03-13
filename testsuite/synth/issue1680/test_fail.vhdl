library ieee;
use ieee.std_logic_1164.all;

entity test_fail is
  port(
    reset_n_i : in std_ulogic;
    clock_i : in  std_ulogic;
    value_o : out std_ulogic
    );
end test_fail;

architecture beh of test_fail is
  
  signal value_s: std_ulogic;

  attribute nomerge : string;
  attribute nomerge of value_s : signal is "";
  
begin

  regs: process (clock_i, reset_n_i)
  begin
    if reset_n_i = '0' then
      value_s <= '0';
    elsif rising_edge(clock_i) then
      value_s <= not value_s;
    end if;
  end process;

  value_o <= value_s;

end architecture;

