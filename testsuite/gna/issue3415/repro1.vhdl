package repro1_pkg is
    type Inter is record
        Valid : bit;
        Addr  : bit_vector;
    end record;

    view InterSlave of Inter is
      Valid : in;
      Addr : in;
    end view;
end;

entity repro1_sub is
  port (
    v : bit;
    ad : bit_vector(7 downto 0));
end;

architecture behav of repro1_sub is
begin
end;

use work.repro1_pkg.all;

entity repro1_top is
  port (
    p : view InterSlave);
end;

architecture behav of repro1_top is
begin
  inst: entity work.repro1_sub
    port map (
      v => p.Valid,
      ad => p.Addr (7 downto 0));
end;
