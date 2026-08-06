use std.textio.all ;

entity Tb_to_string is
end entity Tb_to_string ;
Architecture T of Tb_to_string is
begin
  TestProc : process
    variable buf : line ;
  begin

    write(buf, "to_string(1 min, sec) "  & to_string(1 min, sec) & LF) ;
    write(buf, "to_string(1 min, min) "  & to_string(1 min, min) & LF) ;
    write(buf, "to_string(1 hr,  sec) "  & to_string(1 hr,  sec) & LF) ;
    write(buf, "to_string(1 hr,   hr) "  & to_string(1 hr,   hr) & LF) ;
    WriteLine(OUTPUT, buf) ;

    std.env.stop ;

    wait ;
  end process TestProc ;
end architecture T ;

