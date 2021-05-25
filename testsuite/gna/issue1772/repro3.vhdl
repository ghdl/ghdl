entity repro3 is
end;

architecture behav of repro3 is
  type arr_el_t is array(0 to 1) of string (1 to 4);
begin
  process
  begin
    for k in arr_el_t'range loop
      report natural'image (k);
      null;
    end loop;
    wait;
  end process;
end behav;

