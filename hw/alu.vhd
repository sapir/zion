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
begin
	num_a <= u16(a);
	num_b <= u16(b);

	with op select num_res <=
		num_a + num_b		when aluop_add,
		num_a - num_b		when aluop_sub,
		num_a and num_b	when aluop_and,
		num_a or num_b		when aluop_or,
		num_a xor num_b	when aluop_xor,
		shift_left(num_a,  to_integer(num_b))	when aluop_sll,
		shift_right(num_a, to_integer(num_b))	when aluop_srl,
		u16(resize(signed(b(7 downto 0)), 16))	when aluop_exts,
		(others => '0')	when others;

	res <= Logic_Word(num_res);
end Behavioral;
