package xilnames is

  type state is (state1, state2, state3);

end xilnames;

use work.xilnames.all;
package xilname1 is

  constant state1 : state := state1;
end xilname1;
