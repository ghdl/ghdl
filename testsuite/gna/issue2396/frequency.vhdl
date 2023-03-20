library ieee ;
    use ieee.std_logic_1164.std_logic ;
    use ieee.std_logic_1164.std_ulogic ;
    use ieee.std_logic_1164."not";

package frequency is

    type frequency is range 0 to 2e9 units
        Hz ;
        kHz = 1000 Hz ;
        MHz = 1000 kHz ;
        GHz = 1000 MHz ;
        THz = 1000 GHz ;
    end units ;

    function half_period(freq : frequency) return time ;
    function period(freq : frequency) return time ;

    -- GHDL workaround to make a generic package and instantiate it?
    package gen_generate_clock is
      generic (
        type t ;
        function "not"(x : t) return t is <>
      ) ;
        procedure generate_clock(signal clock : inout t ; freq : frequency ; count : natural := 0) ;
    end package ;

    package generate_clock_bit is new gen_generate_clock generic map(t => bit) ;
    package generate_clock_std_logic is new gen_generate_clock generic map(t => std_logic) ;
    package generate_clock_std_ulogic is new gen_generate_clock generic map(t => std_ulogic) ;

    alias generate_clock is generate_clock_bit.generate_clock[bit, frequency, natural] ;
    alias generate_clock is generate_clock_std_logic.generate_clock[std_logic, frequency, natural] ;
    -- Dedeclaration?
    --alias generate_clock is generate_clock_std_ulogic.generate_clock[std_ulogic, frequency, natural] ;

    -- Doesn't work in GHDL
    --procedure generate_clock generic (
    --    type t ;
    --    function "not"(x : t) return t is <>
    --) parameter (signal clock : inout t ; freq : frequency ; count : natural := 0) ;

    --procedure generate_clock is new generate_clock generic map(t => std_ulogic) ;
    --procedure generate_clock is new generate_clock generic map(t => std_logic) ;
    --procedure generate_clock is new generate_clock generic map(t => bit) ;

end package ;

package body frequency is

    function period(freq : frequency) return time is
    begin
        return 1 sec / frequency'pos(freq) ;
    end function ;

    function half_period(freq : frequency) return time is
    begin
        return period(freq) / 2.0 ;
    end function ;

    package body gen_generate_clock is
        procedure generate_clock(
            signal clock : inout t ;
            freq : frequency ;
            count : natural := 0
        ) is
            constant hp : time := half_period(freq) ;
            variable downcount : natural := count ;
        begin
            -- count = 0 means forever, otherwise we look at the downcount
            while count = 0 or downcount > 0 loop
                clock <= not clock after hp ;
                downcount := downcount - 1 ;
            end loop ;
        end procedure ;
    end package body ;

    --procedure generate_clock generic(
    --  type t ;
    --  function "not"(x : t) return t is <>
    --) parameter (
    --    signal clock : inout t ;
    --    freq : frequency ;
    --    count : natural := 0
    --) is
    --    constant hp : time := half_period(freq) ;
    --    variable downcount : natural := count ;
    --begin
    --    -- count = 0 means forever, otherwise we look at the downcount
    --    while count = 0 or downcount > 0 loop
    --        clock <= not clock after hp ;
    --        downcount := downcount - 1 ;
    --    end loop ;
    --end procedure ;

end package body ;
