entity reproct is
   generic ( lowb : integer := 1 ;
             highb : integer := 10 ;
             lowb_i2 : integer := 0 ;
             highb_i2 : integer := 1000 );
   
   constant c_boolean_1 : boolean := false ;
   constant c_boolean_2 : boolean := true ;
   constant c_integer_1 : integer := lowb ;
   constant c_integer_2 : integer := highb ;
   constant c_time_1 : time := 1 ns ;
   constant c_time_2 : time := 2 ns ;
   constant c_real_1 : real := 0.0 ;
   constant c_real_2 : real := 1.0 ;

   type     t_rec1 is record
              f1 : integer range lowb_i2 to highb_i2 ;
              f2 : time ;
              f3 : boolean ;
              f4 : real ;
           end record ;
  constant c_t_rec1_1 : t_rec1 :=
                          (c_integer_1, c_time_1, c_boolean_1, c_real_1) ;
  constant c_t_rec1_2 : t_rec1 :=
                          (c_integer_2, c_time_2, c_boolean_2, c_real_2) ;
  subtype  st_rec1 is t_rec1 ;
  constant c_st_rec1_1 : st_rec1 := c_t_rec1_1 ;
  constant c_st_rec1_2 : st_rec1 := c_t_rec1_2 ;

end;

architecture ARCH of reproct is
   signal i_t_rec1_1, i_t_rec1_2 : st_rec1
       := c_st_rec1_1 ;
begin
   L1:
   block
   port (i_t_rec1_1 : inout t_rec1 := c_st_rec1_1 ) ;
   port map (i_t_rec1_1) ;
   begin
         i_t_rec1_1 <= c_st_rec1_2 ;
   end block L1 ;
end ARCH ;
