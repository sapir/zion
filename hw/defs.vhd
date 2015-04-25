library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;


package defs is
	subtype Logic_Byte is std_logic_vector(7 downto 0);
	subtype Logic_Word is std_logic_vector(15 downto 0);
	subtype MemByteAddr is std_logic_vector(10 downto 0);
	subtype MemWordAddr is std_logic_vector(9 downto 0);

	subtype Reg_Index is std_logic_vector(3 downto 0);

	subtype u16 is unsigned(15 downto 0);
	subtype u8 is unsigned(7 downto 0);
	subtype u4 is unsigned(3 downto 0);

	type Opcode_Type is (
		opc_add, opc_sub, opc_slt, opc_sltu,
		opc_lb, opc_lw, opc_sb, opc_sw,
		opc_li8, opc_lui, opc_addi, opc_ori,
		opc_b, opc_bal, opc_beqz, opc_bnez,
		opc_sll, opc_srl, opc_slli, opc_srli,
		opc_and, opc_or, opc_nor, opc_xor,
		opc_jr, opc_jalr,
		opc_exts, opc_break
		);

	type Alu_Op_Type is (
		aluop_add,
		aluop_sub,
		aluop_and,
		aluop_or,
		aluop_xor,
		aluop_sll,
		aluop_srl,
		aluop_exts
		);
end defs;

package body defs is
end defs;
