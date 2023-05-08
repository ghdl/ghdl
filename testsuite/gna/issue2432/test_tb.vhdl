package test_pkg is

    type x_t is record
        a : bit_vector ;
    end record ;

end package ;

use work.test_pkg.x_t ;

entity test is
  port (
    clock : in bit ;
    in_x : in x_t
  ) ;
end entity ;

architecture arch of test is

    type state_t is array(natural range 0 to 10) of in_x'subtype ;

    signal state : state_t ;

begin

    process(clock)
    begin
        if( clock'event and clock = '1' ) then
            state <= in_x & state(1 to 10) ;
        end if ;
    end process ;

end architecture ;

use work.test_pkg.x_t ;

entity test_tb is
end entity ;

architecture arch of test_tb is

    signal clock : bit := '1' ;
    signal x : x_t(a(1 downto 0)) ;

begin

  dut : entity work.test
    port map (clock => clock, in_x => x);
  
    clock <= not clock after 1 ns ;

    tb : process
    begin
        x.a <= "00" ;
        for i in 1 to 100 loop
            wait until rising_edge(clock) ;
        end loop ;
        std.env.stop ;
    end process ;

end architecture ;
