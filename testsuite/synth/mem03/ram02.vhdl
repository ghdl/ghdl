library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram02 is
  port (raddr : std_logic_vector (3 downto 0);
        rdat : out std_logic_vector (7 downto 0);
        waddr : std_logic_vector (3 downto 0);
        wdat : std_logic_vector (7 downto 0);
        wen : std_logic;
        clk : std_logic);
end ram02;

architecture behav of ram02 is
  type memdat is record
    d1 : std_logic_vector(7 downto 0);
  end record;

  type memtype is array(0 to 15) of memdat;
  signal rdat2 : std_logic_vector(7 downto 0);
begin
  process (clk)
    variable mem : memtype;
  begin
    if rising_edge (clk) then
      if wen = '1' then
        mem (to_integer(unsigned (waddr))).d1 := wdat;
      end if;
    end if;
    rdat2 <= mem (to_integer(unsigned (raddr))).d1;
  end process;

  --  The read port is not synchronous
  process (clk)
  begin
    if rising_edge (clk) then
      rdat <= rdat2;
    end if;
  end process;
end behav;
