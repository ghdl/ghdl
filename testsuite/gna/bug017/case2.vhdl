entity case1 is
end;

architecture behav of case1 is
  type vec2 is array (natural range <>) of bit_vector (1 to 4);
  constant vects : vec2 := (x"0", x"4", x"9", x"3", x"a");
begin
  process
    variable i : natural := 0;
  begin
    for i in vects'range loop
      case vects (i) is
        when "0100" =>
          report "value is 4";
          wait for 4 ns;
        when "0011" =>
          report "value is 3";
          wait for 3 ns;
        when others =>
          report "unknown value";
          wait for 1 ns;
      end case;
    end loop;
    report "SUCCESS";
    wait;
  end process;
 
end behav;
