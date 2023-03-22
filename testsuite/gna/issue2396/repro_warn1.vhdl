package repro_warn1 is

    type repro_warn1 is range 0 to 2e9 units
        Hz ;
        kHz = 1000 Hz ;
    end units ;

    function half_period(freq : time) return time ;
end;

package body repro_warn1 is

    function half_period(freq : time) return time is
    begin
        return freq / 2.0 ;
    end function ;
end package body ;
