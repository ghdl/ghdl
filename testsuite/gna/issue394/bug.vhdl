use std.textio.all;

entity bug is
end entity;

architecture a of bug is
begin
  main : process
    procedure echo(msg : string) is
      variable l : line;
    begin
      write(l, msg);
      writeline(OUTPUT, l);
    end;

  begin
    echo("1");
    report "2";
    echo("3");
    report "4";
    wait;
  end process;
end;
