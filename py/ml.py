#!/usr/bin/python
from __future__ import print_function
import operator
from collections import namedtuple


InstrFormat = namedtuple('InstrFormat',
    'desc fieldWidths operandTypes operandOrder')


IFmt_Math3 = InstrFormat('math, 3 regs',
    (4, 4,4,4), 'reg reg reg', None)

IFmt_Imm8 = InstrFormat('imm8: reg=imm8',
    (4, 8,4), 'reg imm8', [0,2,1])

# memory access - 2 regs + 3-bit signed offset
# (for lw/sw, offset must be aligned anyway, so we can multiply by 2)
IFmt_Mem = InstrFormat('memory access',
    (5, 3,4,4), 'reg regofs', [0,3,2,1])

IFmt_Math2 = InstrFormat('math, 2 regs',
    (8, 4,4), 'reg reg', None)
IFmt_MathImm4 = InstrFormat('math, reg <<= imm4',
    (8, 4,4), 'reg imm4', None)

IFmt_JmpRel = InstrFormat('jump to relative address',
    (5, 11), 'addr11', None)
# TODO: move 4-bits to end so it's a 12-bit opcode
IFmt_JmpReg = InstrFormat('jump to register',
    (8, 4,0), 'reg', None)
IFmt_Branch = InstrFormat('branch (cond. jump)',
    (5, 7,4), 'reg addr7', [0,2,1])

IFmt_Special = InstrFormat('special',
    (8, 0), '', None)


# opcodes by group:
#
### math/logic
# add, sub
# slt, sltu
# sll, srl
# and, or, nor, xor
# addi, (andi, slti, sltiu)
# slli, srli
#
### immediate loads
# li8       # = addi w/ $zero
# (li16)    # = lui+addi/ori
# lui       # = li8 + slli 8
# (move)    # = add w/ $zero
#
### jumps and branches
# (j, jal)
# jr, jalr
# b, bal
# beqz, bnez
#
### memory access
# lb, lw
# sb, sw


OpcodeInfo = namedtuple('OpcodeInfo', 'id ifmt')

_O = OpcodeInfo

OPCODES = {
    # 4-bit opcodes (8)
    'add':  _O(0, IFmt_Math3),  # write to 0 for nop
    'sub':  _O(1, IFmt_Math3),  # used for neg
    # TODO: these 2 could be 5-bit Math3 with restricted reg, though
    # branches would need to be moved aside. (freeing 4-bit opcode slots
    # for...?)
    'slt':  _O(2, IFmt_Math3),
    'sltu': _O(3, IFmt_Math3),

    'li8':  _O(4, IFmt_Imm8),      # = 3-reg addi w/ $zero
    'lui':  _O(5, IFmt_Imm8),      # = li8 + slli w/ 16
    'addi': _O(6, IFmt_Imm8),      # useful for loops
    'ori':  _O(7, IFmt_Imm8),      # useful for li16

    # 5-bit opcodes (8)
    'lb':   _O(0x10, IFmt_Mem),
    'lw':   _O(0x11, IFmt_Mem),
    'sb':   _O(0x12, IFmt_Mem),
    'sw':   _O(0x13, IFmt_Mem),

    # TODO: b = beqz $zero. but currently no equiv. for bal
    'b':    _O(0x14, IFmt_JmpRel),    # relative jump
    'bal':  _O(0x15, IFmt_JmpRel),    # relative jump & link
    'beqz': _O(0x16, IFmt_Branch),
    'bnez': _O(0x17, IFmt_Branch),

    # 8-bit opcodes (64)
    'sll':  _O(0x80, IFmt_Math2),
    'srl':  _O(0x81, IFmt_Math2),

    'slli': _O(0x82, IFmt_MathImm4),
    'srli': _O(0x83, IFmt_MathImm4),

    'and':  _O(0x84, IFmt_Math2),
    'or':   _O(0x85, IFmt_Math2),
    'nor':  _O(0x86, IFmt_Math2),     # nor w/ self to get not
    'xor':  _O(0x87, IFmt_Math2),

    'jr':   _O(0x88, IFmt_JmpReg),
    'jalr': _O(0x89, IFmt_JmpReg),

    # TODO: move above jr/jalr
    'exts': _O(0x8a, IFmt_Math2),
    # 'not':  IFmt_Math2,     # =nor w/ self

    'break':_O(0xcc, IFmt_Special),

    # not implementing:
    # 'j':    IFmt_JmpAbs,
    # 'jal':  IFmt_JmpAbs,
    # 'andi': IFmt_MathImm8,
    # 'slti': IFmt_MathImm8,
    # 'sltiu':IFmt_MathImm8,
    }

OPCODES_BY_ID = dict(
    (o.id, (name, o))
    for (name, o)
    in OPCODES.iteritems())


NICE_REGISTER_NAMES = dict(
    (name, num)
    for (num, name) in enumerate(
        '$zero $sp $s0 $s1'
        ' $a0 $a1 $v0 $v1'
        ' $a2 $a3 $v2 $v3'
        ' $s2 $s3 $ra $fp'
        .split()))

NUM_REGISTERS = len(NICE_REGISTER_NAMES)

REGISTER_NAMES = dict(NICE_REGISTER_NAMES)
REGISTER_NAMES.update(
    ('$r{0}'.format(i), i)
    for i in xrange(16))


def extractBits(n, msb, width):
    lsb = msb - width + 1
    n &= (1 << (msb + 1)) - 1
    n >>= lsb
    return n

def splitBitFields(word, fieldWidths):
    assert sum(fieldWidths) == 16 or fieldWidths[-1] == 0
    # only last field may be 0-sized (means "skip rest")
    assert not any(w == 0 for w in fieldWidths[:-1])

    res = []

    msb = 15
    for width in fieldWidths:
        if width == 0:
            break

        res.append(extractBits(word, msb, width))
        msb -= width

    return res

def joinBitFields(values, fieldWidths):
    word = 0
    msb = 15
    for value, width in zip(values, fieldWidths):
        lsb = msb - width + 1
        truncValue = value & ((1 << width) - 1)
        assert value == truncValue, (
            "overflow! {} doesn't fit in {} bits (msb is {})"
            .format(value, width, msb))

        word |= value << lsb

        msb = lsb - 1

    return word

def permute(values, order):
    if order is None:
        return values

    return [values[idx] for idx in order]

def unpermute(values, order):
    if order is None:
        return values

    return [values[order.index(i)] for i in xrange(len(values))]

def decodeML(word):
    if (word & 0x8000) == 0:
        opcodeLen = 4
    elif (word & 0x4000) == 0:
        opcodeLen = 5
    else:
        opcodeLen = 8

    opcodeID =  extractBits(word, 15, opcodeLen)
    name, opcodeInfo = OPCODES_BY_ID[opcodeID]
    fields = splitBitFields(word, opcodeInfo.ifmt.fieldWidths)
    fields = unpermute(fields, opcodeInfo.ifmt.operandOrder)

    assert fields[0] == opcodeID
    operands = fields[1:]

    return (name, operands)
