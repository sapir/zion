library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity pl_stage2 is
    Port (
        clk             : in std_logic;

        -- inputs from stage 1
        st2in           : in Stage_1_2_Interface;

        -- inputs from hazard control
        reg1_fwd        : in FwdValue;
        reg2_fwd        : in FwdValue;

        -- outputs back to stage 1
        branch_flag     : out std_logic;
        branch_dest     : out MemWordAddr;

        -- outputs to stage 3
        st2out          : out Stage_2_3_Interface);
end pl_stage2;


architecture Behavioral of pl_stage2 is

    COMPONENT alu
    PORT (
        op  : in Alu_Op_Type;
        a   : in Logic_Word;
        b   : in Logic_Word;
        res : out Logic_Word);
    END COMPONENT;

    -- value of $rs and $rt to actually use. may be different from
    -- st2in.value1/2.reg_val due to data hazards and forwarding.
    signal final_reg1_val : Logic_Word;
    signal final_reg2_val : Logic_Word;
    -- inputs for 'main_alu' instance
    signal alu_inp1 : Logic_Word;
    signal alu_inp2 : Logic_Word;

begin

    final_reg1_val <= reg1_fwd.fwd_val when reg1_fwd.use_fwd = '1'
        else st2in.value1.reg_val;

    final_reg2_val <= reg2_fwd.fwd_val when reg2_fwd.use_fwd = '1'
        else st2in.value2.reg_val;


    alu_inp1 <= final_reg1_val when st2in.value1.use_reg = '1'
        else st2in.value1.imm;

    alu_inp2 <= final_reg2_val when st2in.value2.use_reg = '1'
        else st2in.value2.imm;


    -- copy values from stage 1 (with a few overrides)
    st2out.wr_type      <= wr_none when st2in.invalid_flag = '1'
                                else st2in.wr_type;
    st2out.wr_reg_idx   <= st2in.wr_reg_idx;
    st2out.pc_plus_2    <= st2in.pc_plus_2;
    st2out.reg2_val     <= final_reg2_val;    -- override stage 1 value


    main_alu : alu PORT MAP (
        op  => st2in.alu_op,
        a   => alu_inp1,    -- decided in st2_alu_proc
        b   => alu_inp2,    -- decided in st2_alu_proc
        res => st2out.alu_res);


    branch_proc : process(st2in.branch_type, st2in.branch_dest, final_reg2_val,
        st2in.invalid_flag)
    begin
        -- default
        branch_flag <= '0';
        branch_dest <= (others => '-');

        -- ignore branches for invalidated instructions
        if st2in.invalid_flag = '0' then

            case st2in.branch_type is
                when b_always_imm =>
                    branch_flag <= '1';
                    branch_dest <= st2in.branch_dest;

                when b_always_reg =>
                    branch_flag <= '1';
                    branch_dest <= word_to_mem_addr(final_reg2_val);

                when b_eqz =>
                    if final_reg2_val = "0000000000000000" then
                        branch_flag <= '1';
                        branch_dest <= st2in.branch_dest;
                    end if;

                when b_nez =>
                    if final_reg2_val /= "0000000000000000" then
                        branch_flag <= '1';
                        branch_dest <= st2in.branch_dest;
                    end if;

                when others =>
                    -- no branch, use default
            end case;

        end if;
    end process;

end Behavioral;
