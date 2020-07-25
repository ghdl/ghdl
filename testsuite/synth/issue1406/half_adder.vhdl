library ieee;
use ieee.std_logic_1164.all;

entity half_adder is
  port (
    clk_in   : in  std_logic;
    n_rst_in : in  std_logic;
    a_in     : in  std_logic;
    b_in     : in  std_logic;
    c_out    : out std_logic;
    s_out    : out std_logic
    );
end half_adder;

architecture rtl of half_adder is
begin
  reg_proc : process(clk_in, n_rst_in)
  begin
    if n_rst_in = '0' then
      c_out <= '0';
      s_out <= '0';
    elsif rising_edge(clk_in) then
      c_out <= a_in and b_in;
      s_out <= a_in xor b_in;
    end if;
  end process reg_proc;
end rtl;
