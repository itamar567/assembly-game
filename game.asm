IDEAL

MODEL small

STACK 256

DATASEG
x_offset dw 00h
y_offset dw 00h

; bullet xy cords
bx_offset dw 00h
by_offset dw 05h

; we define the plane in an array of BIOS colors
plane db 00h, 00h, 23 dup(0fh), 00h, 00h, 23 dup(0fh), 25 dup(00h), 25 dup(00h)

; since we need more than a byte, we will use variables for the draw/delete plane functions.
draw_delete_plane_x dw 00h
draw_delete_plane_y dw 00h

CODESEG

proc move_to_graphics_mode
    mov ah, 00h ;enter graphics mode
    mov al, 13h
    int 10h
endp

proc change_bg_color
    mov ax, 0600h
    mov bh, 1fh
    mov cx, 0000h
    mov dx, 184fh
    int 10h
    ret
endp

proc delete_plane
    push ax
    push si
    push cx
    push dx

    mov di, offset x_offset
    mov ax, [di]
    mov [draw_delete_plane_x], ax
    mov di, offset y_offset
    mov ax, [di]
    mov [draw_delete_plane_y], ax
    mov bx, 00h ; bc is the loop index
    mov si, offset plane
    delete_plane_main_loop:
        mov al, 0fh ; We will "delete" the pixel by changing its color to the background color, making it invisible
        mov ah, 0Ch
	   mov cx, [draw_delete_plane_x]
        mov dx, [draw_delete_plane_y]
        int 10h

        inc [draw_delete_plane_x]

        mov di, offset x_offset
        mov ax, [di]
        sub [draw_delete_plane_x], ax
        cmp [draw_delete_plane_x], 19h ; 19h=25 in decimal (number of pixels in a row)
        jge delete_plane_update_y
        delete_plane_repeat:
            inc si ; move to the next pixel
            add [draw_delete_plane_x], ax
            inc bx
            cmp bx, 64h ; check if we delete everything (64h=100=25*4)
            jge delete_plane_end
            jmp delete_plane_main_loop
    delete_plane_update_y:
        sub [draw_delete_plane_x], 19h ; 19h=25 in decimal (number of pixels in a row). we are resetting cx to its default value
	   inc [draw_delete_plane_y]
        jmp delete_plane_repeat
    delete_plane_end:
        pop dx
        pop cx
        pop si
        pop ax
        ret
endp

proc draw_plane
    push ax
    push si
    push cx
    push dx

    mov di, offset x_offset
    mov ax, [di]
    mov [draw_delete_plane_x], ax
    mov di, offset y_offset
    mov ax, [di]
    mov [draw_delete_plane_y], ax
    mov bx, 00h ; bc is the loop index
    mov si, offset plane
    draw_plane_main_loop:
        mov al, [si] ; We will draw the pixel by changing it to its color in the array
        mov ah, 0Ch
	   mov cx, [draw_delete_plane_x]
        mov dx, [draw_delete_plane_y]
        int 10h

        inc [draw_delete_plane_x]

        mov di, offset x_offset
        mov ax, [di]
        sub [draw_delete_plane_x], ax
        cmp [draw_delete_plane_x], 19h ; 19h=25 in decimal (number of pixels in a row)
        jge draw_plane_update_y
        draw_plane_repeat:
            inc si ; move to the next pixel
            add [draw_delete_plane_x], ax
            inc bx
            cmp bx, 64h ; check if we draw everything (64h=100=25*4)
            jge draw_plane_end
            jmp draw_plane_main_loop
    draw_plane_update_y:
        sub [draw_delete_plane_x], 19h ; 19h=25 in decimal (number of pixels in a row). we are resetting cx to its default value
	   inc [draw_delete_plane_y]
        jmp draw_plane_repeat
    draw_plane_end:
        pop dx
        pop cx
        pop si
        pop ax
        ret
endp

proc draw_bullet
    mov cx, [bx_offset]
    mov dx, [by_offset]
    draw_bullet_pixel:
        mov al, 00h
        mov ah, 0Ch
        int 10h
    repeat:
        sub cx, [bx_offset]
        cmp cx, 03h
        je db_add_y
        add cx, [bx_offset]
        inc cx
        jmp draw_bullet_pixel
    db_add_y:
        sub dx, [by_offset]
        cmp dx, 03h
        je exit_draw_bullet
        add dx, [by_offset]
        inc dx
        mov cx, [bx_offset]
        jmp draw_bullet_pixel
    exit_draw_bullet:
        ret
endp

proc delete_bullet
    mov cx, [bx_offset]
    mov dx, [by_offset]
    delete_bullet_pixel:
        mov al, 0fh
        mov ah, 0Ch
        int 10h
    drepeat:
        sub cx, [bx_offset]
        cmp cx, 03h
        je ddb_add_y
        add cx, [bx_offset]
        inc cx
        jmp delete_bullet_pixel
    ddb_add_y:
        sub dx, [by_offset]
        cmp dx, 03h
        je dexit_draw_bullet
        add dx, [by_offset]
        inc dx
        mov cx, [bx_offset]
        jmp delete_bullet_pixel
    dexit_draw_bullet:
        ret
endp

proc wait_for_spacebar
    wait_for_spacebar_start:
        mov ah, 0
        int 16h
        cmp al, ' '
        je spacebar_pressed
        jmp exitfunc
    spacebar_pressed:
        call delete_bullet
        mov si, offset by_offset
	    mov ax, [si]
	    inc ax
	    mov [si], ax
	    call draw_bullet
    exitfunc:
        jmp wait_for_spacebar_start
        ret
endp

Start:
    mov ax, @data
    mov ds, ax

    call move_to_graphics_mode
    call draw_plane
    call wait_for_spacebar

Exit:
END start