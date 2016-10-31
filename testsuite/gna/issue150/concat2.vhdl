library ieee;
use ieee.std_logic_1164.all;
entity concat is
  port (a: in std_ulogic_vector (3 downto 0);
        b: in std_ulogic_vector (3 downto 0);
        q1: out std_ulogic_vector (7 downto 0);
        q2: out std_ulogic_vector (7 downto 0);
        q3: out std_ulogic_vector (7 downto 0));
end concat;


architecture rtl of concat is
begin
  as_q1: q1 <= "0000" & b;
  as_q2: q2 <= a & "0000";
  as_q3: q3 <= a & b;
end rtl;


