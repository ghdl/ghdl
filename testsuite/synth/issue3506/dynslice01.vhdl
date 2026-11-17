library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dynslice01 is
  port (clk, we, s, w : in std_logic;
        d : in std_logic_vector(7 downto 0);
        q : out std_logic_vector(63 downto 0));
end entity;

architecture rtl of dynslice01 is
  type mem_t is array (0 to 1) of std_logic_vector(63 downto 0);
  signal mem : mem_t := (others => (others => '0'));
begin
  process (clk)
    variable si, wi : integer range 0 to 1;
  begin
    if rising_edge(clk) then
      if s = '1' then si := 1; else si := 0; end if;
      if w = '1' then wi := 1; else wi := 0; end if;
      if we = '1' then
        --  Dynamic slice of a dynamically indexed element, with a
        --  constant term (8) that is not a multiple of the step (32).
        mem(si)(32*wi+15 downto 32*wi+8) <= d;
      end if;
    end if;
  end process;
  q <= mem(1) when s = '1' else mem(0);
end architecture;
