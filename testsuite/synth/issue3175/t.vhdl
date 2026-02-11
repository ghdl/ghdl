library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity s is
  generic (
    a : integer;
    b : integer;
    c : integer := a / 8
  );
  port (
    clk : in std_logic;
    d : in integer range 32 to 255;
    e : in integer range 0 to c - 1;
    f : in integer range 0 to b - 1;
    g : out std_logic_vector(7 downto 0)
  );
end s;

architecture rtl of s is

  type a_t is array (0 to b - 1) of std_logic_vector(7 downto 0);
  type b_t is array (0 to c - 1) of a_t;
  type c_t is array (32 to 255) of b_t;

  signal i : c_t := (others => (others => (others => (others => '1'))));

begin

  ROM_PROC : process(clk)
  begin
    if rising_edge(clk) then
      g <= i(d)(e)(f);
    end if;
  end process;

end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity t is
  port (
    clk : in std_logic;
    h : out std_logic_vector(7 downto 0)
  );
end t;

architecture str of t is

  constant a : integer := 32;
  constant b : integer := 14;
  constant c : integer := a / 8;

  signal d : integer range 32 to 255;
  signal e : integer range 0 to c - 1;
  signal f : integer range 0 to b - 1;

begin

  d <= 127;
  e <= 2;
  f <= 12;

  S_INST : entity work.s(rtl)
  generic map (
    a => a,
    b => b
  )
  port map (
    clk => clk,
    d => d,
    e => e,
    f => f,
    g => h
  );

end architecture;
