library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test1 is
end entity;

architecture rtl of test1 is
  type t_state is (ONE, TWO);
  signal state            : t_state;
  attribute syn_encoding          : string;
  attribute syn_encoding of state : signal is "safe, onehot";
  signal sig : unsigned(15 downto 0);
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture rtl of test is
begin
  i_test1 : entity work.test1;
  p_proc : process
    alias sig    is <<signal i_test1.sig : unsigned>>;
  begin
    wait;
  end process;
end architecture;
