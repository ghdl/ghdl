use work.cst.all;

package pkg is
  type rec is record
    s : string;
    a : integer;
  end record;

  constant cfour : natural := work.cst.four;

  subtype rec_4 is rec (s(1 to 4));
  subtype rec_4bis is rec (s(1 to 4));
  subtype rec_4dyn is rec (s(1 to cfour));
end pkg;
