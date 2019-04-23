# This is an example of the assembly generated for isZero.cnot
# This is the assembly you'd get when the input was, in fact, 0

.globl main
main:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $376, %rsp
    movq    $0, %rax
    movq    %rax, %rbx
    movq    $0, %rcx
    movq    $0, %rdx
    cmp     %rdx, %rbx
    je      if_start1
    jmp     else_start2
if_start1:
    movq    $0, %rcx
    jmp     if_cmd_end3
else_start2:
    movq    $1, %rsi
    cmp     %rsi, %rbx
    je      if_start4
    jmp     else_start5
if_start4:
    movq    $1, %rcx
    jmp     if_cmd_end6
else_start5:
    movq    $2, %rdi
    cmp     %rdi, %rbx
    je      if_start7
    jmp     else_start8
if_start7:
    movq    $1, %rcx
    jmp     if_cmd_end9
else_start8:
    movq    $3, %r8
    cmp     %r8, %rbx
    je      if_start10
    jmp     else_start11
if_start10:
    movq    $2, %rcx
    jmp     if_cmd_end12
else_start11:
    movq    $4, %r9
    cmp     %r9, %rbx
    je      if_start13
    jmp     else_start14
if_start13:
    movq    $3, %rcx
    jmp     if_cmd_end15
else_start14:
    movq    $5, %r10
    cmp     %r10, %rbx
    je      if_start16
    jmp     else_start17
if_start16:
    movq    $5, %rcx
    jmp     if_cmd_end18
else_start17:
    movq    $6, %r11
    cmp     %r11, %rbx
    je      if_start19
    jmp     else_start20
if_start19:
    movq    $8, %rcx
    jmp     if_cmd_end21
else_start20:
    movq    $7, %r12
    cmp     %r12, %rbx
    je      if_start22
    jmp     else_start23
if_start22:
    movq    $13, %rcx
    jmp     if_cmd_end24
else_start23:
    movq    $8, %r13
    cmp     %r13, %rbx
    je      if_start25
    jmp     else_start26
if_start25:
    movq    $21, %rcx
    jmp     if_cmd_end27
else_start26:
    movq    $9, %r15
    movq    %r15, -8(%rbp)
    cmp     -8(%rbp), %rbx
    je      if_start28
    jmp     else_start29
if_start28:
    movq    $34, %rcx
    jmp     if_cmd_end30
else_start29:
    movq    $10, %r15
    movq    %r15, -16(%rbp)
    cmp     -16(%rbp), %rbx
    je      if_start31
    jmp     else_start32
if_start31:
    movq    $55, %rcx
    jmp     if_cmd_end33
else_start32:
    movq    $11, %r15
    movq    %r15, -24(%rbp)
    cmp     -24(%rbp), %rbx
    je      if_start34
    jmp     else_start35
if_start34:
    movq    $89, %rcx
    jmp     if_cmd_end36
else_start35:
    movq    $12, %r15
    movq    %r15, -32(%rbp)
    cmp     -32(%rbp), %rbx
    je      if_start37
    jmp     else_start38
if_start37:
    movq    $144, %rcx
    jmp     if_cmd_end39
else_start38:
    movq    $13, %r15
    movq    %r15, -40(%rbp)
    cmp     -40(%rbp), %rbx
    je      if_start40
    jmp     else_start41
if_start40:
    movq    $233, %rcx
    jmp     if_cmd_end42
else_start41:
    movq    $14, %r15
    movq    %r15, -48(%rbp)
    cmp     -48(%rbp), %rbx
    je      if_start43
    jmp     else_start44
if_start43:
    movq    $377, %rcx
    jmp     if_cmd_end45
else_start44:
    movq    $15, %r15
    movq    %r15, -56(%rbp)
    cmp     -56(%rbp), %rbx
    je      if_start46
    jmp     else_start47
if_start46:
    movq    $610, %rcx
    jmp     if_cmd_end48
else_start47:
    movq    $16, %r15
    movq    %r15, -64(%rbp)
    cmp     -64(%rbp), %rbx
    je      if_start49
    jmp     else_start50
if_start49:
    movq    $987, %rcx
    jmp     if_cmd_end51
else_start50:
    movq    $17, %r15
    movq    %r15, -72(%rbp)
    cmp     -72(%rbp), %rbx
    je      if_start52
    jmp     else_start53
if_start52:
    movq    $1597, %rcx
    jmp     if_cmd_end54
else_start53:
    movq    $18, %r15
    movq    %r15, -80(%rbp)
    cmp     -80(%rbp), %rbx
    je      if_start55
    jmp     else_start56
if_start55:
    movq    $2584, %rcx
    jmp     if_cmd_end57
else_start56:
    movq    $19, %r15
    movq    %r15, -88(%rbp)
    cmp     -88(%rbp), %rbx
    je      if_start58
    jmp     else_start59
if_start58:
    movq    $4181, %rcx
    jmp     if_cmd_end60
else_start59:
    movq    $20, %r15
    movq    %r15, -96(%rbp)
    cmp     -96(%rbp), %rbx
    je      if_start61
    jmp     else_start62
if_start61:
    movq    $6765, %rcx
    jmp     if_cmd_end63
else_start62:
    movq    $21, %r15
    movq    %r15, -104(%rbp)
    cmp     -104(%rbp), %rbx
    je      if_start64
    jmp     else_start65
if_start64:
    movq    $10946, %rcx
    jmp     if_cmd_end66
else_start65:
    movq    $22, %r15
    movq    %r15, -112(%rbp)
    cmp     -112(%rbp), %rbx
    je      if_start67
    jmp     else_start68
if_start67:
    movq    $17711, %rcx
    jmp     if_cmd_end69
else_start68:
    movq    $23, %r15
    movq    %r15, -120(%rbp)
    cmp     -120(%rbp), %rbx
    je      if_start70
    jmp     else_start71
if_start70:
    movq    $28657, %rcx
    jmp     if_cmd_end72
else_start71:
    movq    $24, %r15
    movq    %r15, -128(%rbp)
    cmp     -128(%rbp), %rbx
    je      if_start73
    jmp     else_start74
if_start73:
    movq    $46368, %rcx
    jmp     if_cmd_end75
else_start74:
    movq    $25, %r15
    movq    %r15, -136(%rbp)
    cmp     -136(%rbp), %rbx
    je      if_start76
    jmp     else_start77
if_start76:
    movq    $75025, %rcx
    jmp     if_cmd_end78
else_start77:
    movq    $26, %r15
    movq    %r15, -144(%rbp)
    cmp     -144(%rbp), %rbx
    je      if_start79
    jmp     else_start80
if_start79:
    movq    $121393, %rcx
    jmp     if_cmd_end81
else_start80:
    movq    $27, %r15
    movq    %r15, -152(%rbp)
    cmp     -152(%rbp), %rbx
    je      if_start82
    jmp     else_start83
if_start82:
    movq    $196418, %rcx
    jmp     if_cmd_end84
else_start83:
    movq    $28, %r15
    movq    %r15, -160(%rbp)
    cmp     -160(%rbp), %rbx
    je      if_start85
    jmp     else_start86
if_start85:
    movq    $317811, %rcx
    jmp     if_cmd_end87
else_start86:
    movq    $29, %r15
    movq    %r15, -168(%rbp)
    cmp     -168(%rbp), %rbx
    je      if_start88
    jmp     else_start89
if_start88:
    movq    $514229, %rcx
    jmp     if_cmd_end90
else_start89:
    movq    $30, %r15
    movq    %r15, -176(%rbp)
    cmp     -176(%rbp), %rbx
    je      if_start91
    jmp     else_start92
if_start91:
    movq    $832040, %rcx
    jmp     if_cmd_end93
else_start92:
    movq    $31, %r15
    movq    %r15, -184(%rbp)
    cmp     -184(%rbp), %rbx
    je      if_start94
    jmp     else_start95
if_start94:
    movq    $1346269, %rcx
    jmp     if_cmd_end96
else_start95:
    movq    $32, %r15
    movq    %r15, -192(%rbp)
    cmp     -192(%rbp), %rbx
    je      if_start97
    jmp     else_start98
if_start97:
    movq    $2178309, %rcx
    jmp     if_cmd_end99
else_start98:
    movq    $33, %r15
    movq    %r15, -200(%rbp)
    cmp     -200(%rbp), %rbx
    je      if_start100
    jmp     else_start101
if_start100:
    movq    $3524578, %rcx
    jmp     if_cmd_end102
else_start101:
    movq    $34, %r15
    movq    %r15, -208(%rbp)
    cmp     -208(%rbp), %rbx
    je      if_start103
    jmp     else_start104
if_start103:
    movq    $5702887, %rcx
    jmp     if_cmd_end105
else_start104:
    movq    $35, %r15
    movq    %r15, -216(%rbp)
    cmp     -216(%rbp), %rbx
    je      if_start106
    jmp     else_start107
if_start106:
    movq    $9227465, %rcx
    jmp     if_cmd_end108
else_start107:
    movq    $36, %r15
    movq    %r15, -224(%rbp)
    cmp     -224(%rbp), %rbx
    je      if_start109
    jmp     else_start110
if_start109:
    movq    $14930352, %rcx
    jmp     if_cmd_end111
else_start110:
    movq    $37, %r15
    movq    %r15, -232(%rbp)
    cmp     -232(%rbp), %rbx
    je      if_start112
    jmp     else_start113
if_start112:
    movq    $24157817, %rcx
    jmp     if_cmd_end114
else_start113:
    movq    $38, %r15
    movq    %r15, -240(%rbp)
    cmp     -240(%rbp), %rbx
    je      if_start115
    jmp     else_start116
if_start115:
    movq    $39088169, %rcx
    jmp     if_cmd_end117
else_start116:
    movq    $39, %r15
    movq    %r15, -248(%rbp)
    cmp     -248(%rbp), %rbx
    je      if_start118
    jmp     else_start119
if_start118:
    movq    $63245986, %rcx
    jmp     if_cmd_end120
else_start119:
    movq    $40, %r15
    movq    %r15, -256(%rbp)
    cmp     -256(%rbp), %rbx
    je      if_start121
    jmp     else_start122
if_start121:
    movq    $102334155, %rcx
    jmp     if_cmd_end123
else_start122:
    movq    $63245986, %r15
    movq    %r15, -264(%rbp)
    movq    $102334155, %r15
    movq    %r15, -272(%rbp)
    movq    $41, %r15
    movq    %r15, -280(%rbp)
continue125:
    cmp     %rbx, -280(%rbp)
    je      if_start126
    jmp     else_start127
if_start126:
    jmp     break124
    jmp     if_cmd_end128
else_start127:
    movq    -264(%rbp), %r15
    movq    %r15, -288(%rbp)
    movq    -272(%rbp), %r15
    addq    %r15, -288(%rbp)
    movq    -288(%rbp), %r15
    movq    %r15, -296(%rbp)
    movq    -272(%rbp), %r15
    movq    %r15, -264(%rbp)
    movq    -296(%rbp), %r15
    movq    %r15, -272(%rbp)
    movq    -280(%rbp), %r15
    movq    %r15, -304(%rbp)
    movq    $1, %r15
    addq    %r15, -304(%rbp)
    movq    -304(%rbp), %r15
    movq    %r15, -280(%rbp)
if_cmd_end128:
    movq    $15150, %r15
    movq    %r15, -312(%rbp)
    movq    $15122, %r15
    movq    %r15, -320(%rbp)
    movq    -320(%rbp), %r15
    cmp     %r15, -312(%rbp)
    jg      if_start129
    jmp     else_start130
if_start129:
    jmp     continue125
    jmp     if_cmd_end131
else_start130:
if_cmd_end131:
    movq    -272(%rbp), %r15
    movq    %r15, -328(%rbp)
    movq    $15122, %r15
    movq    -328(%rbp), %r14
    imulq   %r15, %r14
    movq    %r14, -328(%rbp)
    movq    -328(%rbp), %r15
    movq    %r15, -272(%rbp)
    jmp     continue125
break124:
    movq    -264(%rbp), %r15
    movq    %r15, -336(%rbp)
    movq    -272(%rbp), %r15
    addq    %r15, -336(%rbp)
    movq    -336(%rbp), %rcx
if_cmd_end123:
if_cmd_end120:
if_cmd_end117:
if_cmd_end114:
if_cmd_end111:
if_cmd_end108:
if_cmd_end105:
if_cmd_end102:
if_cmd_end99:
if_cmd_end96:
if_cmd_end93:
if_cmd_end90:
if_cmd_end87:
if_cmd_end84:
if_cmd_end81:
if_cmd_end78:
if_cmd_end75:
if_cmd_end72:
if_cmd_end69:
if_cmd_end66:
if_cmd_end63:
if_cmd_end60:
if_cmd_end57:
if_cmd_end54:
if_cmd_end51:
if_cmd_end48:
if_cmd_end45:
if_cmd_end42:
if_cmd_end39:
if_cmd_end36:
if_cmd_end33:
if_cmd_end30:
if_cmd_end27:
if_cmd_end24:
if_cmd_end21:
if_cmd_end18:
if_cmd_end15:
if_cmd_end12:
if_cmd_end9:
if_cmd_end6:
if_cmd_end3:
    movq    $0, %r15
    movq    %r15, -344(%rbp)
continue133:
    movq    -344(%rbp), %r15
    movq    %r15, -352(%rbp)
    movq    -344(%rbp), %r15
    movq    -352(%rbp), %r14
    imulq   %r15, %r14
    movq    %r14, -352(%rbp)
    cmp     %rcx, -352(%rbp)
    jne     if_start134
    jmp     else_start135
if_start134:
    movq    -344(%rbp), %r15
    movq    %r15, -360(%rbp)
    movq    -344(%rbp), %r15
    movq    -360(%rbp), %r14
    imulq   %r15, %r14
    movq    %r14, -360(%rbp)
    cmp     %rcx, -360(%rbp)
    jg      if_start137
    jmp     else_start138
if_start137:
    movq    $0, %rdi
    jmp     return0
    jmp     if_cmd_end139
else_start138:
    movq    -344(%rbp), %r15
    movq    %r15, -368(%rbp)
    movq    $1, %r15
    addq    %r15, -368(%rbp)
    movq    -368(%rbp), %r15
    movq    %r15, -344(%rbp)
if_cmd_end139:
    jmp     if_cmd_end136
else_start135:
    jmp     break132
if_cmd_end136:
    jmp     continue133
break132:
    movq    $0, %r15
    movq    %r15, -376(%rbp)
    movq    -376(%rbp), %r15
    cmp     %r15, -344(%rbp)
    je      if_start140
    jmp     else_start141
if_start140:
    movq    $1, %rdi
    jmp     return0
    jmp     if_cmd_end142
else_start141:
    movq    $0, %rdi
    jmp     return0
if_cmd_end142:
return0:
    movq    $60, %rax
    syscall
