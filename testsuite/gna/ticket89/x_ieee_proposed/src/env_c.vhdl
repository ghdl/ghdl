package ENV is

  procedure STOP (STATUS   : INTEGER);
  procedure FINISH (STATUS : INTEGER);

  function RESOLUTION_LIMIT return DELAY_LENGTH;

end package ENV;
library ieee_proposed;
use ieee_proposed.standard_additions.all;
package body ENV is

  procedure STOP (STATUS   : INTEGER) is
  begin
    report "Procedure STOP called with status: " & INTEGER'image(STATUS)
      severity failure;
  end procedure STOP;
  procedure FINISH (STATUS : INTEGER) is
  begin
    report "Procedure FINISH called with status: " & INTEGER'image(STATUS)
      severity failure;
  end procedure FINISH;

  constant BASE_TIME_ARRAY : time_vector :=
    (
      1 fs, 10 fs, 100 fs,
      1 ps, 10 ps, 100 ps,
      1 ns, 10 ns, 100 ns,
      1 us, 10 us, 100 us,
      1 ms, 10 ms, 100 ms,
      1 sec, 10 sec, 100 sec,
      1 min, 10 min, 100 min,
      1 hr, 10 hr, 100 hr
      ) ;

  function RESOLUTION_LIMIT return DELAY_LENGTH is
  begin
    for i in BASE_TIME_ARRAY'range loop
      if BASE_TIME_ARRAY(i) > 0 hr then
        return BASE_TIME_ARRAY(i);
      end if;
    end loop;
    report "STANDATD.RESOLUTION_LIMIT: Simulator resolution not less than 100 hr"
      severity failure;
    return 1 ns;
  end function RESOLUTION_LIMIT;

end package body ENV;
