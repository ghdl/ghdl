package pkg is
  procedure proc(signal sig : in integer; msg : string);
end package;

package body pkg is
  procedure proc(signal sig : in integer; msg : string) is
  begin
    loop
      wait on sig;
      report integer'image(sig) & " : " & msg;
    end loop;
  end procedure;
end package body;

use work.pkg.all;

entity ent2 is
  port (
    prt : out integer);
begin
  proc(prt, "entity");
end entity;

architecture a of ent2 is
begin
  proc(prt, "architecture");
  main : process
  begin
    prt <= 1;
    wait for 1 ns;
    prt <= 2;
    wait;
  end process;
end architecture;

entity ent is
end entity;

architecture a of ent is
  signal sig : integer;
begin
  ent2_inst : entity work.ent2 port map(prt => sig);
end architecture;
