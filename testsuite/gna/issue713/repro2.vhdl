entity repro2 is
   generic (str : string := "1234");
end;

use std.textio.all;
architecture behav of repro2 is
  type line_array is array (1 to 10) of line;
begin
  p: process
    function f return natural is
    begin
      return 8;
    end f;

    subtype st is natural range str'range; -- natural range 1 to f;
    variable v : line_array;

    procedure fill (l : natural) is
    begin
      for i in v'range loop
        deallocate (v(i));
        v(i) := new string'(1 to l * i => 'a');
      end loop;
    end fill;

    procedure doloop (variable l : line)
    is
      constant num : natural := l'length;
      variable count : natural := 0;
    begin
      for i in l'range loop
        count := count + 1;
        assert i = count
          report "count=" & natural'image (count) & ", i=" & natural'image(i)
          severity failure;
        fill (i);
      end loop;
    end doloop;
    
  begin
    fill (7);
    doloop (v(3));
    doloop (v(8));
    for k in p.st loop
      wait for 1 ns; 
    end loop;
    wait;
  end process;
end behav;
