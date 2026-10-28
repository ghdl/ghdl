library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity srcattr01 is
  port (clk : in std_logic;
        rst : in std_logic;
        en  : in std_logic;
        sel : in std_logic_vector(1 downto 0);
        q   : out std_logic_vector(7 downto 0);
        hit : out std_logic;
        m   : out std_logic_vector(7 downto 0));
end srcattr01;

architecture rtl of srcattr01 is
  signal r : unsigned(7 downto 0);
  attribute keep : boolean;
  attribute keep of r : signal is true;
begin
  process (clk, rst)
  begin
    if rst = '1' then
      r <= (others => '0');
    elsif rising_edge(clk) then
      if en = '1' then
        r <= r + 1;
      end if;
    end if;
  end process;

  q <= std_logic_vector(r);
  hit <= '1' when r = 42 else '0';

  with sel select m <=
    std_logic_vector(r) when "00",
    not std_logic_vector(r) when "01",
    std_logic_vector(r(6 downto 0) & '0') when "10",
    x"55" when others;
end rtl;
