library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
  port (clk : std_logic;
        a : in std_logic_vector(15 downto 0);
        b : out std_logic_vector(15 downto 0));
end repro;

architecture behav of repro is
  function my_conv (arg : unsigned) return std_logic_vector
  is
    alias xarg : unsigned(arg'length - 1 downto 0) is arg;
  begin
    return std_logic_vector(xarg);
  end my_conv;
begin

  process (clk)
    variable t : std_logic_vector(0 to 15);
  begin
    if rising_edge(clk) then
      t := a;
      b <= my_conv (unsigned(t));
    end if;
  end process;
end behav;
