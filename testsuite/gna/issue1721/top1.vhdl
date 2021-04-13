library ieee;
use ieee.std_logic_1164.all;

entity top1 is
end entity;

architecture a of top1 is
    signal a,b,c,d : std_logic := '0';
    signal clk_sys, clk1, clk2 : std_logic;
begin

  -- Following throws bug occured with:
  --  "psl.sem_property: cannot handle N_CLOCKED_SER"
  -- Clocked SERE shall be allowed according to 6.1.1.1 of PSL LRM 2003
  -- psl my_seq : assert never {a;b;c} @ rising_edge(clk_sys);
end a;
