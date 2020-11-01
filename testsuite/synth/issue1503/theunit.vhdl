library ieee;
use ieee.std_logic_1164.all;
entity theunit is
  port (a : in  std_ulogic);
end;

architecture rtl of theunit is
begin
  comb : process (a)
    variable c : natural range 0 to 3;
    variable d : std_ulogic_vector(3 downto 0);
  begin
    if a = '1' then
      for i in 0 to 2 loop
        exit;
      end loop;
    end if;
    c := 0;
    d := (others => '0');
    d(c) := '1';
  end process;
end;
