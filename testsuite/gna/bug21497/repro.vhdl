entity tb is
  generic (low : integer := 1 ; high : integer := 10);
end;

architecture behav of tb is
begin
   process
     type  st_arr1 is array (low to high) of integer;

     type  st_arr2 is array (low to high) of st_arr1;
     constant c_st_arr2 : st_arr2 := (others => (others => 1));

     type a_st_arr2 is access st_arr2;
     variable v_st_arr2 : a_st_arr2 := new st_arr2'(c_st_arr2) ;
   begin
     assert v_st_arr2.all = c_st_arr2 severity failure;
     wait;
   end process ;
end behav;
