entity repro is
end;

architecture behav of repro is
  type arr_el_t is array(0 to 3) of string;
  type arr_t is array(integer range <>) of arr_el_t;

  constant cst : arr_t :=
    (1 => ("   ","A0      ","VAL2","ELEMENT_VALUE_3"),
     2 => ("   ","10      ","VAL2","ELEMENT_VALUE_3"));
begin
  process
  begin
    for i in cst'range loop
      report "cst(" & to_string(i) & "):";
      for j in cst(i)'range loop
        report cst(i)(j);
      end loop;
    end loop;
    wait;
  end process;
end behav;

