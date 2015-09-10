entity case4 is
end;

architecture behav of case4 is
  subtype bv4 is bit_vector (1 to 4);
  type vec2 is array (natural range <>) of bv4;
  constant vects : vec2 := (x"0", x"3", x"9", x"4", x"a");

  procedure print (msg : string; t : time) is
  begin
    report msg;
    wait for t;
  end print;
begin
  process
  begin
    for i in vects'range loop
      case bv4'(vects (i)) is
        when "0100" =>
          print ("value is 4", 4 ns);
          print ("yes, really 4", 4 ns);
        when "0011" =>
          print ("value is 3", 3 ns);
        when "0101" =>
          print ("value is 5", 5 ns);
        when others =>
          print ("unknown value", 0 ns);
      end case;
    end loop;
    report "SUCCESS";
    wait;
  end process;
 
end behav;
