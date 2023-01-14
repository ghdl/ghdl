package prot_pkg is
  type myprot is protected
    impure function get return natural;
    procedure set (val : natural);
  end protected;
end prot_pkg;

package body prot_pkg is
  shared variable v: myprot;

  type myprot is protected body
    variable var : natural;
    impure function get return natural is
    begin
      return var;
    end get;

    procedure set (val : natural) is
    begin
      var := val;
    end set;
  end protected body myprot;
end prot_pkg;
