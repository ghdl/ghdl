entity repro is
end repro ;

architecture arch of repro is
  function get_str (l : natural) return string is
    variable res : string (1 to l);
  begin
    for i in res'range loop
      res (i) := character'val (96 + i);
    end loop;
    return res;
  end get_str;

  procedure check2 (msg : string) is
  begin
    report msg;
    assert msg (1 to 10) = "abcdefghij" severity failure;
  end check2;

  procedure check1 (msg : string) is
  begin
    report "before check";
    wait for 1 ns;
    check2 (msg);
    report "after check";
    wait for 1 ns;
  end check1;
begin
   process
   begin
      check1 (get_str (24));
      wait;
   end process;

   process
   begin
     for i in 10 to 20 loop
       report get_str (i);
       wait for 1 ns;
     end loop;
     wait;
   end process;
end arch;
