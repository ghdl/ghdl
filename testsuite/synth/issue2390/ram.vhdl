library ieee;
context ieee.ieee_std_context;
use work.uCPUtypes.all;

entity RAM is
  port (
    clk   : in logic;
    abus  : in unsigned_byte;
    dbus  : inout unsigned_byte;
    wr_en : in logic
  );
end entity RAM;

architecture RTL of RAM is
  type memory is array (0 to 255) of unsigned_byte;
  signal mem : memory := (others => x"00");
begin

dbus <= mem(to_integer(abus)) when not wr_en else x"ZZ";

syn_write: process (clk)
begin
  if rising_edge(clk) then
    mem(to_integer(abus)) <= dbus when wr_en else unaffected;
  end if;
end process syn_write;

end architecture RTL;
