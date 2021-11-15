entity tb is
end;

use work.repro.all;

architecture behav of tb
is
begin
  process
    variable sum : natural;
  begin
    sum := 0;

    for i in cst'range(1) loop
      for j in cst'range(2) loop
        sum := (sum + cst(i, j)) mod 4096;
      end loop;
    end loop;

    report "sum: " & natural'image(sum);
    wait;
  end process;
end behav;
