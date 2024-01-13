library IEEE ;
use ieee.std_logic_1164.all ; 

entity aggr2 is 
end;

architecture test of aggr2 is

  function get_left(s : std_logic_vector) return natural is
  begin
    return s'left;
  end get_left;

  signal Index, res1, res2 : integer := 0 ; 
begin
  res1 <= get_left((index => '1'));
 
    
  RtlProc : process(Index)
  begin
    res2 <= get_left((index => '1'));
  end process RtlProc;

  
  TestProc : process
  begin
    for i in 1 to 7 loop
      Index <= i ;
      wait for 10 ns ;
      report "i=" & natural'image(i);
      assert res1 = i report "res=" & natural'image (res1) severity failure;
      assert res2 = i;
    end loop ; 
    
    wait ; 
  end process TestProc ; 
end architecture test ; 
