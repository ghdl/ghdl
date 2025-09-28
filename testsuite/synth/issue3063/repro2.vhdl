library ieee;
  use ieee.std_logic_1164.all;

entity dff18 is
  port (
    clk : in std_logic;
    rst : in std_logic;
    en  : in std_logic;
    d1  : in std_logic;
    q   : out std_logic
  );
end;

architecture behavioral of dff18 is
begin
  process (clk, rst) is
    variable p_hold : std_logic;
  begin
    if (rst = '1') then
      q <= '0';
    elsif rising_edge(clk) then
      p_hold := q;
      if en = '1' then
        p_hold := d1;
      end if;
      q <= p_hold;
    end if;
  end process;
end architecture behavioral;

