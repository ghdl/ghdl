library ieee; 
use ieee.std_logic_1164.all;

entity comparator2 is
  port(
    a, b : in std_logic_vector(1 downto 0);
    eq : out std_logic
  ); 
end comparator2;

architecture structure of comparator2 is
  signal s0, s1: std_logic; 
begin
  -- use Verilog component i.e. comparator1BitVerilog
  eq_bit0: entity work.comparator1
    port map (a=>a(0), b=>b(0), eq=>s0);
  eq_bit1: entity work.comparator1
    port map (a=>a(1), b=>b(1), eq=>s1);
      
  eq <= s0 and s1;
end structure;  
