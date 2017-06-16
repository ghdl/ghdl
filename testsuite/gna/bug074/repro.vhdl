package pkg is
  procedure proc (a : bit_vector; r : out bit);
end pkg;

package body pkg is
  procedure proc (a : bit_vector; r : out bit)
  is
    variable v1, v2 : bit_vector (1 to 2 * a'length);
  begin
    v1 := a & a;
    v2 := a & a;
    r := '0';
  end proc;
end pkg;

entity repro is
end repro;

architecture behav of repro is
begin
   process
     variable v : bit;
     constant c : bit_vector (1 to 1024) := (1 => '1', others => '0');
   begin
     loop
       work.pkg.proc (c, v);
       wait for 1 ns;
     end loop;
   end process;
end behav;
