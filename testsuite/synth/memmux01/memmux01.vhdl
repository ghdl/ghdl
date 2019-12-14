library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity memmux01 is
  port (
    wen : std_logic;
    addr : std_logic_vector (3 downto 0);
    wdat : std_logic;
    rdat : out std_logic_vector (15 downto 0);
    clk : std_logic;
    rst : std_logic);
end memmux01;

architecture rtl of memmux01 is
begin
  process (clk)
  is
    variable mem : std_logic_vector (15 downto 0);
    variable ad : natural range 0 to 15;
  begin
    if rising_edge(clk) then
      rdat <= mem;

      if rst = '1' then
        mem := (others => '0');
      else
        ad := to_integer(unsigned(addr));
        if wen = '1' then
          mem (ad) := wdat;
        end if;
      end if;
    end if;
  end process;
end rtl;
