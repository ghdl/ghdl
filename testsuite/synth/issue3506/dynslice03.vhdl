library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dynslice03 is
  port (clk, we, w : in std_logic;
        d : in std_logic_vector(15 downto 0);
        q : out std_logic_vector(63 downto 0));
end entity;

architecture rtl of dynslice03 is
  type b_t is array (natural range <>) of std_logic_vector(7 downto 0);
  signal mem : b_t(7 downto 0) := (others => x"00");
begin
  process (clk)
    variable wi : integer range 0 to 1;
  begin
    if rising_edge(clk) then
      if w = '1' then wi := 1; else wi := 0; end if;
      if we = '1' then
        --  Dynamic slice of an array of bytes: the constant term (1) is
        --  one element, so 8 bits.
        mem(4*wi+2 downto 4*wi+1) <= (d(15 downto 8), d(7 downto 0));
      end if;
    end if;
  end process;
  g : for i in 0 to 7 generate
    q(8*i+7 downto 8*i) <= mem(i);
  end generate;
end architecture;
