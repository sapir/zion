library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity pl_stage1 is
    Port (
        clk             : in std_logic;

        -- communication with IRAM
        iram_addr       : out MemWordAddr;
        iram_dout       : in Logic_Word;

        -- communication with Register File
        reg_idx1        : out Reg_Index;
        reg_idx2        : out Reg_Index;
        reg_dout1       : in Logic_Word;
        reg_dout2       : in Logic_Word;

        st1out          : out Stage_1_2_Interface;

        -- inputs back from stage 2
        branch_flag     : in std_logic;
        branch_dest     : in MemWordAddr;

        -- inputs from pipeline hazard logic

        -- if '1', pc won't be updated to next_pc, so instruction in stage 1
        -- will be repeated. (stage 1 should of course also be invalidated using
        -- st1out.invalid_flag.) in case of branch from stage 2, stall of stage
        -- 1 will be ignored; it should have been invalidated anyway.
        stall_flag      : in std_logic);
end pl_stage1;


architecture Behavioral of pl_stage1 is
    signal pc           : MemWordAddr;
    signal pc_plus_2    : MemWordAddr;
    signal next_pc      : MemWordAddr;
    signal cur_opcode   : Opcode_Type;
    -- some instruction fields that are useful to multiple instructions
    signal rd, rs, rt   : Reg_Index;
    signal imm8         : Logic_Byte;
begin

    sync_proc : process(clk)
    begin
        if rising_edge(clk) then
            pc <= next_pc;
        end if;
    end process;

    pc_plus_2 <= std_logic_vector(unsigned(pc) + 1);

    iram_addr <= pc;

    rd <= iram_dout(11 downto 8);
    rs <= iram_dout(7 downto 4);
    rt <= iram_dout(3 downto 0);
    imm8 <= iram_dout(11 downto 4);

    -- feed parts of IRAM output into Register File inputs
    reg_idx1 <= rs;
    reg_idx2 <= rt;

    -- feed Register File outputs directly to stage 1 output
    st1out.value1.reg_val <= reg_dout1;
    st1out.value2.reg_val <= reg_dout2;


    opcode_proc : process(iram_dout)
    begin
        cur_opcode <= opc_break; -- catch-all
        if iram_dout(15) = '0' then
            -- 4-bit opcode. first bit is 0
            case iram_dout(14 downto 12) is
                when "000" => cur_opcode <= opc_add;
                when "001" => cur_opcode <= opc_sub;
                when "010" => cur_opcode <= opc_slt;
                when "011" => cur_opcode <= opc_sltu;
                when "100" => cur_opcode <= opc_li8;
                when "101" => cur_opcode <= opc_lui;
                when "110" => cur_opcode <= opc_addi;
                when "111" => cur_opcode <= opc_ori;
                when others => cur_opcode <= opc_break;
            end case;

        elsif iram_dout(14) = '0' then
            -- 5-bit opcode. first 2 bits are 10
            case iram_dout(13 downto 11) is
                when "000" => cur_opcode <= opc_lb;
                when "001" => cur_opcode <= opc_lw;
                when "010" => cur_opcode <= opc_sb;
                when "011" => cur_opcode <= opc_sw;
                when "100" => cur_opcode <= opc_b;
                when "101" => cur_opcode <= opc_bal;
                when "110" => cur_opcode <= opc_beqz;
                when "111" => cur_opcode <= opc_bnez;
                when others => cur_opcode <= opc_break;
            end case;

        else
            -- 8-bit opcode. first 2 bits are 11
            case iram_dout(13 downto 8) is
                when "000000" => cur_opcode <= opc_sll;
                when "000001" => cur_opcode <= opc_srl;
                when "000010" => cur_opcode <= opc_slli;
                when "000011" => cur_opcode <= opc_srli;
                when "000100" => cur_opcode <= opc_and;
                when "000101" => cur_opcode <= opc_or;
                when "000110" => cur_opcode <= opc_nor;
                when "000111" => cur_opcode <= opc_xor;
                when "001000" => cur_opcode <= opc_jr;
                when "001001" => cur_opcode <= opc_jalr;
                when "001010" => cur_opcode <= opc_exts;

                when "001100" => cur_opcode <= opc_break;
                when others => cur_opcode <= opc_break;
            end case;
        end if; -- opcode width
    end process;

    -- decide on stage 1 outputs
    outputs_proc : process(iram_dout, cur_opcode,
        rd, rs, rt, imm8, pc_plus_2)
    begin
        -- first set default values
        st1out.alu_op           <= aluop_add;
        st1out.value1.use_reg   <= '0';
        st1out.value1.imm       <= (others => '0');
        st1out.value2.use_reg   <= '0';
        st1out.value2.imm       <= (others => '0');
        st1out.branch_type      <= b_none;
        st1out.branch_dest      <= (others => '-');
        st1out.wr_type          <= wr_none;
        st1out.wr_reg_idx       <= (others => '-');
        st1out.pc_plus_2        <= pc_plus_2;

        case cur_opcode is
            -- IFmt_Math3, IFmt_Math2
            when  opc_add | opc_sub | opc_slt | opc_sltu
                | opc_sll | opc_srl | opc_and | opc_or
                | opc_nor | opc_xor | opc_exts =>

                case cur_opcode is
                    when opc_add    => st1out.alu_op <= aluop_add;
                    when opc_sub    => st1out.alu_op <= aluop_sub;
                    when opc_and    => st1out.alu_op <= aluop_and;
                    when opc_or     => st1out.alu_op <= aluop_or;
                    when opc_nor    => st1out.alu_op <= aluop_nor;
                    when opc_xor    => st1out.alu_op <= aluop_xor;
                    when opc_sll    => st1out.alu_op <= aluop_sll;
                    when opc_srl    => st1out.alu_op <= aluop_srl;
                    when opc_exts   => st1out.alu_op <= aluop_exts;
                    when opc_slt    => st1out.alu_op <= aluop_slt;
                    when opc_sltu   => st1out.alu_op <= aluop_sltu;

                    when others => st1out.alu_op <= aluop_add;
                end case;

                st1out.value1.use_reg <= '1';
                st1out.value2.use_reg <= '1';

                st1out.wr_type <= wr_alu_to_reg;

                case cur_opcode is
                    -- IFmt_Math3
                    when opc_add | opc_sub | opc_slt | opc_sltu =>
                        st1out.wr_reg_idx <= rd;

                    -- IFmt_Math2
                    when others =>
                        st1out.wr_reg_idx <= rs;
                end case;

            -- IFmt_Mem
            when opc_lb | opc_sb | opc_lw | opc_sw =>

                -- use ALU to calculate memory offset
                st1out.alu_op <= aluop_add;
                st1out.value1.use_reg <= '1';
                st1out.value2.imm <= Logic_Word(
                    resize(signed(iram_dout(10 downto 8)), 16));

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

                    when others =>
                        -- impossible
                end case;

            -- IFmt_Imm8
            when opc_li8 =>
                st1out.wr_reg_idx <= rt;
                st1out.wr_type <= wr_alu_to_reg;

                st1out.alu_op <= aluop_add;
                st1out.value1.imm <= (others => '0');
                st1out.value2.imm <= "00000000" & imm8;

            when opc_lui =>
                st1out.wr_reg_idx <= rt;
                st1out.wr_type <= wr_alu_to_reg;

                st1out.alu_op <= aluop_add;
                st1out.value1.imm <= (others => '0');
                st1out.value2.imm <= imm8 & "00000000";

            when opc_addi =>
                st1out.wr_reg_idx <= rt;
                st1out.wr_type <= wr_alu_to_reg;

                st1out.alu_op <= aluop_add;
                st1out.value1.imm <= Logic_Word(resize(signed(imm8), 16));
                st1out.value2.use_reg <= '1';

            when opc_ori =>
                st1out.wr_reg_idx <= rt;
                st1out.wr_type <= wr_alu_to_reg;

                st1out.alu_op <= aluop_or;
                st1out.value1.imm <= "00000000" & imm8;
                st1out.value2.use_reg <= '1';

            -- IFmt_JmpRel
            when opc_b | opc_bal =>
                st1out.branch_type <= b_always_imm;

                st1out.branch_dest <= MemWordAddr(
                    unsigned(pc_plus_2)
                    + ("00" & unsigned(iram_dout(10 downto 0))));

                -- link: save $pc+2 in $ra
                if cur_opcode = opc_bal then
                    st1out.wr_reg_idx <= ra_reg_idx;
                    st1out.wr_type <= wr_pc_plus_2_to_ra;
                end if;

            -- IFmt_Branch
            when opc_beqz | opc_bnez =>
                if cur_opcode = opc_beqz then
                    st1out.branch_type <= b_eqz;
                else
                    st1out.branch_type <= b_nez;
                end if;

                -- branch instruction format is weird.
                st1out.branch_dest <= MemWordAddr(
                    unsigned(pc_plus_2)
                    + unsigned(resize(signed(iram_dout(10 downto 4)), 13)));

                -- note we'll be using the value of $rt (value2.reg_val) to
                -- evaluate the branch condition, but we don't care what the
                -- ALU sees because we won't be using it.

            -- IFmt_JmpReg
            when opc_jr | opc_jalr =>
                st1out.branch_type <= b_always_reg;

                -- note we'll be using the value of $rt as the branch
                -- destination, but we don't care what the ALU sees because
                -- we won't be using it.

                -- link: save $pc+2 in $ra
                if cur_opcode = opc_jalr then
                    st1out.wr_reg_idx <= ra_reg_idx;
                    st1out.wr_type <= wr_pc_plus_2_to_ra;
                end if;

            -- TODO: opc_break?

            when others =>
                -- use defaults
        end case;
    end process;

    next_pc_proc : process(pc, pc_plus_2, stall_flag, branch_flag, branch_dest)
    begin
        if branch_flag = '1' then
            next_pc <= branch_dest;
            -- previous instruction branched, ignore instruction we read in
            -- this cycle
            st1out.invalid_flag <= '1';

        elsif stall_flag = '1' then
            next_pc <= pc;
            -- instruction is stalling. ignore it for one cycle.
            st1out.invalid_flag <= '1';

        else
            next_pc <= pc_plus_2;
            st1out.invalid_flag <= '0';
        end if;
    end process;

end Behavioral;
