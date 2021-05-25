entity repro2 is
end;

architecture behav of repro2 is
  type arr_el_t is array(0 to 1) of string (1 to 4);
  type arr_t is array(integer range <>) of arr_el_t;

  constant cst : arr_t := (1 => ("abcd", "efgh"),
                           2 => ("ijkl", "mnop"));
begin
  process
  begin
    for k in arr_el_t'range loop
      null;
    end loop;
    
    for i in cst'range loop
      report "cst(" & to_string(i) & "):";
      for j in cst'element'range loop
        report cst(i)(j);
      end loop;
    end loop;
    wait;
  end process;
end behav;

