entity e2b is
  generic (gen1 : natural := 5);
end;

architecture a of e2b is
    function outer(arg : integer) return integer is
        function inner1(arg : integer) return integer is
        begin
            return arg + gen1;
        end;
        function inner2(arg : integer) return integer is
        begin
            return inner1(arg + 2);
        end;
    begin
        return inner2(arg + 3);
    end;
begin
end;
