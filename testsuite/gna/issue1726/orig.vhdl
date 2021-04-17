package my_gpkg is
  generic (
    type data_t;
    c_default : data_t);

 constant my_constant : data_t := c_default;

end package my_gpkg;

use std.standard.all;
package my_pkg_int is new work.my_gpkg
  generic map (
    data_t    => integer,
    c_default => 5);
