use work.mylib_pkg.all;

package types_pkg is

  constant TYPE_WIDTH : natural := CEIL_LOG2(4);
  subtype mytype_t is bit_vector(TYPE_WIDTH - 1 downto 0);

end package;
