library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity memmux04 is
  port (
    wen : std_logic;
    waddr : std_logic_vector (3 downto 0);
    wdat : std_logic_vector (31 downto 0);
    raddr : std_logic_vector (3 downto 0);
    rsel : std_logic_vector (1 downto 0);
    rdat : out std_logic_vector(7 downto 0);
    clk : std_logic);
end memmux04;

architecture rtl of memmux04 is
begin
  process (clk)
  is
    type mem_type is array(0 to 15) of std_logic_vector(31 downto 0);
    variable mem : mem_type;
    variable ad : natural range 0 to 15;
    variable sd : natural range 0 to 3;
    variable w : std_logic_vector (31 downto 0);
  begin
    if rising_edge(clk) then
      --  Read
      ad := to_integer(unsigned(raddr));
      w := mem (ad);
      sd := to_integer(unsigned(rsel));
      rdat <= w (sd*8 + 7 downto sd*8);

      ad := to_integer(unsigned(waddr));
      if wen = '1' then
        mem (ad) := wdat;
      end if;
    end if;
  end process;
end rtl;
