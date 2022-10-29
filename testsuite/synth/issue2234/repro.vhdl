library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
  port (clk : std_logic;
        we : std_logic;
        sel : std_logic;
        addr : std_logic_vector(7 downto 0);
        dati : std_logic_vector(31 downto 0);
        dato_0 : out std_logic_vector(31 downto 0);
        dato_1 : out std_logic_vector(31 downto 0));
end repro;

architecture syn of repro is
  type mem_t is array (0 to 255) of std_logic_vector(31 downto 0);
  signal mem_0 : mem_t;
  signal mem_1 : mem_t;
begin
  process (clk)
  begin
    if rising_edge(clk) then
      if we = '1' then
        if sel = '0' then
          mem_0 (to_integer(unsigned(addr))) <= dati;
        else
          mem_1 (to_integer(unsigned(addr))) <= dati;
        end if;
      end if;
      dato_0 <= mem_0 (to_integer(unsigned(addr)));
      dato_1 <= mem_1 (to_integer(unsigned(addr)));
    end if;
  end process;
end syn;
