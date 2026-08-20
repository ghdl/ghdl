library ieee;
use ieee.std_logic_1164.all;

entity passthrough is
  port (
    wdata : in  std_ulogic_vector(1 downto 0);
    rdata : out std_ulogic_vector(1 downto 0)
  );
end;

architecture rtl of passthrough is
begin
  rdata <= wdata;
end;


library ieee;
use ieee.std_logic_1164.all;

entity theunit is
  port (
    inx   : in    std_ulogic_vector(1 downto 0);
    utx   : out   std_ulogic_vector(1 downto 0)
  );
end;

architecture rtl of theunit is
  type tdata_t is record
    hello : std_ulogic_vector(123 downto 0);
  end record;

  subtype tdatavec_t is std_ulogic_vector(1 downto 0);

  function encodex(u : tdata_t) return std_ulogic_vector is
    variable ret : tdatavec_t;
  begin
    ret(1) := u.hello(101);
    ret(0) := u.hello(100);
    return ret;
  end;

  signal wtdata  : tdata_t;

begin
  wtdata.hello <= (others => '0');

  ram: entity work.passthrough
  port map (
    wdata => encodex(wtdata),
    rdata => utx
  );
end;
