entity err_prot01 is
  port (a : bit; o : out bit);
end;

architecture behav of err_prot01 is
  type prot_type is protected
    procedure write (v : natural);
    impure function read return natural;
  end protected;

  type prot_type is protected body
    variable var : natural;
    procedure write (v : natural) is
    begin
      var := v;
    end write;
    impure function read return natural is
    begin
      return var;
    end read;
  end protected body;
begin
  process (a)
    variable prot : prot_type;
  begin
    o <= a;
  end process;
end behav;
    
