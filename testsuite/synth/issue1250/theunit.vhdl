library ieee;
use ieee.std_logic_1164.all;
entity theunit is
  port (dout : out std_ulogic);
end;

architecture rtl of theunit is
  subtype thenum_t is integer range 0 to 1;
  type rec_t is record
    -- NOTE: changing order of these members prevents crash
    data0 : std_ulogic;
    bankm : std_ulogic_vector(thenum_t);
  end record;
  signal r : rec_t;
begin
  thecomb : process(r)
    variable v      : rec_t;
    variable thenum : thenum_t := 1;
  begin
    v.data0 := '1';
    v.bankm := (others => '1');
    -- NOTE: removing any of the lines below prevents crash
    v.bankm(thenum) := '0';
    r <= v;
    dout <= r.data0;
  end process;
end;

