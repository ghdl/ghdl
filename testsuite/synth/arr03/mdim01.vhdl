library ieee;
use ieee.std_logic_1164.all;

entity mdim01 is
  port (a0, a1 : std_logic_vector (3 downto 0);
        o0 : out std_logic_vector (3 downto 0));
end mdim01;

architecture behav of mdim01 is
  type t_matrix is array (0 to 1, 3 downto 0) of boolean;
  constant mat : t_matrix :=
    (0 => (3 => true, 2 => true, 1 => false, 0 => false),
     1 => (3 => true, 2 => false, 1 => true, 0 => false));
begin
  process (a0, a1)
    variable b : std_logic;
  begin
    for i in t_matrix'range(2) loop
      if mat (0, i) then
        b := a0 (i);
      else
        b := '0';
      end if;
      if mat (1, i) then
        b := b xor a1 (i);
      end if;
      o0 (i) <= b;
    end loop;
  end process;
end behav;
