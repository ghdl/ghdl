library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity array_index_crash is
end entity;

architecture behaviour of array_index_crash is

  constant SIZE : integer := 8;
  constant AMIN : integer := 0;
  constant AMAX : integer := 7;

  subtype data_t is std_logic_vector((SIZE-1) downto 0);
  type data_arr_t is array(AMIN to AMAX) of data_t;

  function initialise return data_arr_t is
    variable ret : data_arr_t;
    variable itv : integer;
  begin
    for i in AMIN to AMAX
    loop
      itv := 2*AMAX;
      --  vvv oops
      ret(itv) := std_logic_vector(to_unsigned(itv, SIZE));
    end loop;
    return ret;
  end function;

  constant data_arr : data_arr_t := initialise;
begin
end architecture;
