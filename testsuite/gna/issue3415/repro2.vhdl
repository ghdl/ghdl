package repro2_pkg is
    type Inter is record
        Valid : bit;
        Addr  : bit_vector;
    end record;

    view InterSlave of Inter is
      Valid : in;
      Addr : in;
    end view;
end;

entity repro2_sub is
  port (
    v : bit;
    ad : bit_vector(7 downto 0));
end;

architecture behav of repro2_sub is
begin
end;

use work.repro2_pkg.all;

entity repro2_top is
  port (
    p : Inter);
end;

architecture behav of repro2_top is
begin
  inst: entity work.repro2_sub
    port map (
      v => p.Valid,
      ad => p.Addr (7 downto 0));
end;
