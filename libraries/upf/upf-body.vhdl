package body upf is

  function supply_on (
    constant supply_name : string;
    constant voltage     : real)
    return boolean is
  begin
    return true;
  end supply_on;

  function supply_partial_on (
    constant supply_name : string;
    constant voltage     : real)
    return boolean is
  begin
    return true;
  end supply_partial_on;

  function supply_off (
    constant supply_name : string)
    return boolean is
  begin
    return true;
  end supply_off;

end upf;
