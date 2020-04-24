-- count number of '1'.
library ieee;
use ieee.std_logic_1164.all;
entity ent is
  port    (
    sel   : in  std_ulogic;
    din   : in  std_ulogic_vector(15 downto 0);
    dout  : out std_ulogic
  );
end;

architecture rtl of ent is
begin
  comb : process (sel, din)
    variable v : std_ulogic;
  begin
    v := '0';
    if sel = '1' then
      for i in din'range loop
        if din(i) = '0' then
          next;
        end if;
        v := not v;
      end loop;
    end if;
    dout <= v;
  end process;
end;
