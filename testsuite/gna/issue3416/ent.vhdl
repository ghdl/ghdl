use std.textio.all;

entity ent is
end entity;

architecture a of ent is
begin
  process
    type time_array is array (natural range <>) of time;
    constant C_VALUES : time_array := (0 ns, 10 ns, 100 ns, 1000 ns, 19 ns, 110 ns, -10 ns);
    variable l : line;
  begin
    for i in C_VALUES'range loop
      write(l, time'image(C_VALUES(i)) & " -> to_string(v, ns) = [" & to_string(C_VALUES(i), ns) & "]");
      writeline(output, l);
    end loop;
    write(l, "computed zero -> to_string(now - now, ns) = [" & to_string(now - now, ns) & "]");
    writeline(output, l);
    wait;
  end process;
end;
