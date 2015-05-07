library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity alu is
    Port ( op  : in Alu_Op_Type;
           a   : in Logic_Word;
           b   : in Logic_Word;
           res : out Logic_Word);
end alu;

architecture Behavioral of alu is
    signal num_a, num_b, num_res : u16;
    signal a_lt_b   : std_logic_vector(0 downto 0); -- a < b as signed
    signal a_ltu_b  : std_logic_vector(0 downto 0); -- a < b as unsigned
begin
    num_a <= u16(a);
    num_b <= u16(b);

    -- TODO: make these use subtraction byproducts?
    a_lt_b  <= "1" when signed(num_a) < signed(num_b) else "0";
    a_ltu_b <= "1" when num_a < num_b                 else "0";

    with op select num_res <=
        num_a + num_b                           when aluop_add,
        num_a - num_b                           when aluop_sub,
        num_a and num_b                         when aluop_and,
        num_a or num_b                          when aluop_or,
        num_a nor num_b                         when aluop_nor,
        num_a xor num_b                         when aluop_xor,
        shift_left(num_b,  to_integer(num_a))   when aluop_sll,
        shift_right(num_b, to_integer(num_a))   when aluop_srl,
        resize(unsigned(a_lt_b), 16)            when aluop_slt,
        resize(unsigned(a_ltu_b), 16)           when aluop_sltu,
        (others => '0')                         when others;

    res <= Logic_Word(num_res);
end Behavioral;
