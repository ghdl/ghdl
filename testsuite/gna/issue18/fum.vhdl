package fum is 
    type fie is protected 
        impure function foo return integer;
        impure function foo(input: real) return integer; -- 4
        impure function foo return integer_vector;
        impure function foo (input: real) return integer_vector; -- 6
        impure function foo (input: integer) return integer;
        impure function foo(input: integer) return integer_vector;

    end protected fie;
end package;

package body fum is
    type fie is protected body
        variable answer:    integer := 42;

        impure function foo return integer is
        begin
            return answer;
        end;

        impure function foo(input:real) return integer is
        begin
            return integer(input) + answer;
        end;

        impure function foo return integer_vector is 
            variable conv_vector: integer_vector (0 to 1);
        begin
            conv_vector(0) := answer;
            conv_vector(1) := 0;
            return conv_vector;
        end; 

        impure function foo (input: real) return integer_vector is
            variable conv_vector: integer_vector (0 to 1);
        begin
            conv_vector(0) := integer(input) + answer;
            conv_vector(1) := 0;
        end;

        impure function foo (input: integer) return integer is
        begin
            return answer + input;
        end;

        impure function foo(input: integer) return integer_vector is 
            variable conv_vector: integer_vector (0 to 1);
        begin
            conv_vector(0) := input + answer;
            conv_vector(1) := 0;
            return conv_vector;
        end; 
    end protected body fie;
end package body;
