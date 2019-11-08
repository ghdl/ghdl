library ieee;
use ieee.std_logic_1164.all;

entity dff09 is
  port (q : out std_logic_vector(3 downto 0);
        d : std_logic_vector(3 downto 0);
        clk : std_logic;
        rst : std_logic);
end dff09;

architecture behav of dff09 is
begin
  process (clk, rst) is
  begin
    if rst = '1' then
      for i in q'range loop
        q(i) <= '0';
      end loop;
      --  q <= x"0";
    elsif rising_edge (clk) then
      q <= d;
    end if;
  end process;
end behav;
