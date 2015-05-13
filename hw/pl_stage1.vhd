library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity pl_stage1 is
    Port (
        clk             : in std_logic;

        iram_dout       : in Instr_Type;
        st1in           : in Stage_0_1_Interface;

        -- communication with Register File
        reg_idx1        : out Reg_Index;
        reg_idx2        : out Reg_Index;
        reg_dout1       : in Logic_Word;
        reg_dout2       : in Logic_Word;

        st1out          : out Stage_1_2_Interface;

        -- input back from stage 2
        branch_flag     : in std_logic;

        -- inputs from pipeline hazard logic

        -- see st1_stall_flag in pl_stage0 inputs
        st1_stall_flag  : in std_logic);
end pl_stage1;


architecture Behavioral of pl_stage1 is
    signal cur_opcode   : Opcode_Type;

    -- instruction fields
    signal opcode_fld   : std_logic_vector(5 downto 0);
    signal opcode_grp   : std_logic_vector(1 downto 0);
    signal rd, rs, rt   : Reg_Index;
    signal imm8         : Logic_Byte;
    signal addr12       : std_logic_vector(11 downto 0);
    signal addr8        : std_logic_vector(7 downto 0);
    signal link_flag    : std_logic;  -- part of jump opcodes
begin

    opcode_fld <= iram_dout(17 downto 12);
    rd         <= iram_dout(11 downto 8);
    rs         <= iram_dout( 7 downto 4);
    rt         <= iram_dout( 3 downto 0);
    imm8       <= iram_dout(11 downto 4);
    addr12     <= iram_dout(11 downto 0);
    addr8      <= iram_dout(11 downto 4);
    link_flag  <= iram_dout(12);

    opcode_grp <= opcode_fld(5 downto 4);

    -- feed parts of IRAM output into Register File inputs
    reg_idx1 <= rs;
    reg_idx2 <= rt;

    -- feed Register File outputs directly to stage 1 output
    st1out.value1.reg_val <= reg_dout1;
    st1out.value2.reg_val <= reg_dout2;

    -- invalidate instructions on stalls and following taken branches
    st1out.invalid_flag <= st1_stall_flag or branch_flag;

    st1out.pc_plus_2 <= st1in.pc_plus_2;


    with opcode_fld select cur_opcode <=
        opc_add     when "000000",
        opc_sub     when "000001",
        opc_slt     when "000010",
        opc_sltu    when "000011",
        opc_and     when "000100",
        opc_or      when "000101",
        opc_nor     when "000110",
        opc_xor     when "000111",
        opc_sll     when "001000",
        opc_srl     when "001001",
        opc_exts    when "001010",

        opc_addi    when "010000",
        opc_slti    when "010010",
        opc_sltiu   when "010011",
        opc_ori     when "010101",
        opc_slli    when "011000",
        opc_srli    when "011001",
        opc_li8     when "011011",
        opc_lui     when "011100",

        opc_j       when "100000",
        opc_jal     when "100001",
        opc_jr      when "100010",
        opc_jalr    when "100011",
        opc_beqz    when "100100",
        opc_bnez    when "100110",

        opc_lb      when "110000",
        opc_lw      when "110001",
        opc_sb      when "110010",
        opc_sw      when "110011",
        opc_break   when "110101",

        opc_break   when others;

    -- decide on stage 1 outputs
    outputs_proc : process(cur_opcode, st1in.pc_plus_2,
        opcode_grp, rd, rs, rt, imm8, addr12, addr8, link_flag)
    begin
        -- first set default values (except for values decided
        -- elsewhere)
        st1out.alu_op           <= aluop_add;
        st1out.value1.use_reg   <= '0';
        st1out.value1.imm       <= (others => '0');
        st1out.value2.use_reg   <= '0';
        st1out.value2.imm       <= (others => '0');
        st1out.branch_type      <= b_none;
        st1out.branch_dest      <= (others => '-');
        st1out.wr_type          <= wr_none;
        st1out.wr_reg_idx       <= (others => '-');

        case opcode_grp is
            -- group 0: IFmt_Math3, IFmt_Math2; group 1: IFmt_Imm8
            when "00" | "01" =>

                case cur_opcode is
                    when opc_add|opc_addi   => st1out.alu_op <= aluop_add;
                    when opc_sub            => st1out.alu_op <= aluop_sub;
                    when opc_slt|opc_slti   => st1out.alu_op <= aluop_slt;
                    when opc_sltu|opc_sltiu => st1out.alu_op <= aluop_sltu;
                    when opc_and            => st1out.alu_op <= aluop_and;
                    when opc_or|opc_ori     => st1out.alu_op <= aluop_or;
                    when opc_nor            => st1out.alu_op <= aluop_nor;
                    when opc_xor            => st1out.alu_op <= aluop_xor;
                    when opc_sll|opc_slli   => st1out.alu_op <= aluop_sll;
                    when opc_srl|opc_srli   => st1out.alu_op <= aluop_srl;
                    when opc_exts           => st1out.alu_op <= aluop_exts;

                    when opc_li8|opc_lui    => st1out.alu_op <= aluop_sll;

                    when others => null;
                end case;

                st1out.wr_type <= wr_alu_to_reg;

                if opcode_grp = "00" then
                    -- IFmt_Math3/Math2

                    st1out.wr_reg_idx     <= rd;
                    st1out.value1.use_reg <= '1';
                    st1out.value2.use_reg <= '1';

                elsif cur_opcode = opc_li8 or cur_opcode = opc_lui then
                    st1out.wr_reg_idx <= rt;

                    st1out.value1.use_reg <= '0';

                    if cur_opcode = opc_li8 then
                        -- li8 - shift by 0
                        st1out.value1.imm     <= X"0000";
                    else
                        -- lui - shift by 8
                        st1out.value1.imm     <= X"0008";
                    end if;

                    st1out.value2.use_reg <= '0';
                    st1out.value2.imm     <= "00000000" & imm8;

                else
                    -- IFmt_Imm8

                    st1out.wr_reg_idx     <= rt;

                    st1out.value1.use_reg <= '0';
                    -- some instrs. treat immediate as signed, some as unsigned
                    if cur_opcode = opc_addi or cur_opcode = opc_slti then
                        st1out.value1.imm <= Logic_Word(resize(signed(imm8), 16));
                    else
                        st1out.value1.imm <= Logic_Word(resize(unsigned(imm8), 16));
                    end if;

                    st1out.value2.use_reg <= '1';
                end if;

            -- group 2: jumps and branches
            when "10" =>

                case cur_opcode is
                    when opc_j | opc_jal =>
                        st1out.branch_type <= b_always_imm;
                        st1out.branch_dest <= MemWordAddr(
                            signed(st1in.pc_plus_2) + resize(signed(addr12), 13));

                    when opc_jr | opc_jalr =>
                        -- note we'll be using the value of $rt as the branch
                        -- destination, but we don't care what the ALU sees
                        -- because we won't be using it.
                        st1out.branch_type <= b_always_reg;
                        st1out.branch_dest <= (others => '-');

                    -- note for cond. branches we'll be using the value of $rt
                    -- as the branch destination, but we don't care what the
                    -- ALU sees because we won't be using it.
                    when opc_beqz =>
                        st1out.branch_type <= b_eqz;
                        st1out.branch_dest <= MemWordAddr(
                            signed(st1in.pc_plus_2) + resize(signed(addr8), 13));

                    when opc_bnez =>
                        st1out.branch_type <= b_nez;
                        st1out.branch_dest <= MemWordAddr(
                            signed(st1in.pc_plus_2) + resize(signed(addr8), 13));

                    when others => null; -- invalid instr., use defaults
                end case;

                -- link: save $pc+2 in $ra
                if link_flag = '1' then
                    st1out.wr_reg_idx <= ra_reg_idx;
                    st1out.wr_type <= wr_pc_plus_2_to_ra;
                end if;

            -- group 3: everything else
            when "11" =>
                case cur_opcode is
                    -- IFmt_Mem
                    when opc_lb | opc_sb | opc_lw | opc_sw =>

                        -- use ALU to calculate memory address
                        st1out.alu_op <= aluop_add;
                        st1out.value1.use_reg <= '1';
                        -- offset value is in rd
                        st1out.value2.use_reg <= '0';
                        st1out.value2.imm <= Logic_Word(resize(signed(rd), 16));

                        -- decide what to write where
                        case cur_opcode is
                            -- for loads, write mem output to $rt
                            when opc_lb =>
                                st1out.wr_type <= wr_memb_to_reg;
                                st1out.wr_reg_idx <= rt;

                            when opc_lw =>
                                st1out.wr_type <= wr_memw_to_reg;
                                st1out.wr_reg_idx <= rt;

                            -- for stores, write $rt to mem.
                            -- note that we're reading $rt but not passing it to the
                            -- alu - we're setting use_reg to '0'. alu calculation uses
                            -- value2.imm instead
                            when opc_sb =>
                                st1out.wr_type <= wr_reg_to_memb;
                                st1out.value2.use_reg <= '0';

                            when opc_sw =>
                                st1out.wr_type <= wr_reg_to_memw;
                                st1out.value2.use_reg <= '0';

                            when others => null; -- impossible
                        end case;

                    -- TODO:
                    when opc_break => null;

                    when others => null; -- use defaults

                end case;

            when others => null; -- impossible
        end case;
    end process;

end Behavioral;
