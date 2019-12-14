-- modelled according to IEEE Std 1801-2015, 11.2

package upf is

  function supply_on (
    constant supply_name : string;
    constant voltage     : real)
    return boolean;

  function supply_partial_on (
    constant supply_name : string;
    constant voltage     : real)
    return boolean;

  function supply_off (
    constant supply_name : string)
    return boolean;

end upf;
