IDEAL

MODEL small

STACK 256

DATASEG
x_offset dw 00h
y_offset dw 00h

bullet_count dw 00h

; bullet xy cords array
bx_offset_array dw 500 dup(0000h)
by_offset_array dw 500 dup(0000h)
dead_bullets_array dw 500 dup(0000h)

bullets_left dw 03h

bx_offset dw 00h
by_offset dw 00h

; building width and height
building_width dw 00h
building_height dw 00h

building_x dw 00h
building_y dw 05h

; we define the plane in an array of BIOS colors
plane db 2 dup(64h),3 dup(0h),45 dup(64h),0h,2 dup(04h),0h,44 dup(64h),0h,2 dup(04h),0h,44 dup(64h),0h,3 dup(04h),0h,43 dup(64h),0h,3 dup(04h),0h,43 dup(64h),0h,4 dup(04h),0h,42 dup(64h),0h,4 dup(04h),0h,42 dup(64h),0h,5 dup(04h),0h,41 dup(64h),0h,6 dup(04h),2 dup(0h),18 dup(64h),11 dup(0h),10 dup(64h),0h,8 dup(04h),0h,14 dup(64h),3 dup(0h),11 dup(07h),3 dup(0h),6 dup(64h),25 dup(0h),13 dup(07h),0h,08h,08h,08h,0h,4 dup(64h),0h,26 dup(07h),0h,07h,0h,07h,0h,07h,0h,07h,0h,4 dup(07h),5 dup(0h),3 dup(64h),0h,07h,8 dup(0h),07h,0h,07h,0h,07h,0h,07h,0h,07h,0h,07h,0h,07h,0h,21 dup(07h),2 dup(0h),64h,0h,07h,0h,6 dup(04h),0h,15 dup(07h),9 dup(0h),13 dup(07h),0h,64h,2 dup(0h),5 dup(04h),0h,15 dup(07h),0h,8 dup(04h),0h,13 dup(07h)
db 0h,2 dup(64h),0h,4 dup(04h),3 dup(0h),13 dup(07h),0h,8 dup(04h),0h,12 dup(07h),2 dup(0h),2 dup(64h),0h,4 dup(04h),0h,3 dup(64h),13 dup(0h),8 dup(04h),0h,9 dup(07h),4 dup(0h),4 dup(64h),0h,2 dup(04h),2 dup(0h),15 dup(64h),0h,7 dup(04h),12 dup(0h),8 dup(64h),3 dup(0h),16 dup(64h),0h,6 dup(04h),2 dup(0h),38 dup(64h),0h,5 dup(04h),2 dup(0h),39 dup(64h),0h,4 dup(04h),2 dup(0h),41 dup(64h),5 dup(0h),64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h,64h

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
    mov ah, 06h
    xor al, al
    xor cx, cx
    mov dx, 184Fh
    mov bh, 64h
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
        mov cx, [draw_delete_plane_x]
        mov dx, [draw_delete_plane_y]
        mov al, 64h
        mov ah, 0Ch
        cmp cx, 320
        jge delete_plane_pixel_add_y
        int 10h
        jmp delete_plane_skip_pixel_add_y
        delete_plane_pixel_add_y:
            add dx, 10h
            sub cx, 320
            int 10h
        delete_plane_skip_pixel_add_y:

        inc [draw_delete_plane_x]

        mov di, offset x_offset
        mov ax, [di]
        sub [draw_delete_plane_x], ax
        cmp [draw_delete_plane_x], 48 ;number of pixels in a row
        jge delete_plane_update_y
        delete_plane_repeat:
            inc si ; move to the next pixel
            add [draw_delete_plane_x], ax
            inc bx
            cmp bx, 1056 ; check if we delete everything (1056=48*22)
            jge delete_plane_end
            jmp delete_plane_main_loop
    delete_plane_update_y:
        sub [draw_delete_plane_x], 48 ;number of pixels in a row
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
        ; check if the pixel is black (if we are colliding with a building)
        mov cx, [draw_delete_plane_x]
        mov dx, [draw_delete_plane_y]
        mov ah, 0Dh
        cmp cx, 320
        jge draw_plane_pixel_add_y
        int 10h
        jmp draw_plane_skip_pixel_add_y
        draw_plane_pixel_add_y:
            add dx, 10h
            sub cx, 320
            int 10h
        draw_plane_skip_pixel_add_y:
        cmp al, 00h
        je exit_game

        mov al, [si] ; We will draw the pixel by changing it to its color in the array
        mov ah, 0Ch
        int 10h

        inc [draw_delete_plane_x]

        mov di, offset x_offset
        mov ax, [di]
        sub [draw_delete_plane_x], ax
        cmp [draw_delete_plane_x], 48 ;number of pixels in a row
        jge draw_plane_update_y
        draw_plane_repeat:
            inc si ; move to the next pixel
            add [draw_delete_plane_x], ax
            inc bx
            cmp bx, 1056 ; check if we draw everything (1056=48*22)
            jge draw_plane_end
            jmp draw_plane_main_loop
    draw_plane_update_y:
        sub [draw_delete_plane_x], 48 ;number of pixels in a row
	    inc [draw_delete_plane_y]
        jmp draw_plane_repeat
    draw_plane_end:
        pop dx
        pop cx
        pop si
        pop ax
        ret
    exit_game:
        int 4Ch
endp

proc draw_building
    mov cx, [building_x]
    mov dx, [building_y]
    draw_building_pixel:
        mov al, 00h
        mov ah, 0Ch
        int 10h
    draw_building_repeat:
        sub cx, [building_x]
        cmp cx, [building_width]
        je draw_building_add_y
        add cx, [building_x]
        inc cx
        jmp draw_building_pixel
    draw_building_add_y:
        sub dx, [building_y]
        cmp dx, [building_height]
        je exit_draw_building
        add dx, [building_y]
        inc dx
        mov cx, [building_x]
        jmp draw_building_pixel
    exit_draw_building:
        ret   
endp

proc draw_bullet
    mov cx, [bx_offset]
    mov dx, [by_offset]
    draw_bullet_pixel:
        mov al, 04h
        mov ah, 0Ch
        int 10h
    repeat:
        sub cx, [bx_offset]
        cmp cx, 09h
        je db_add_y
        add cx, [bx_offset]
        inc cx
        jmp draw_bullet_pixel
    db_add_y:
        sub dx, [by_offset]
        cmp dx, 0eh
        je exit_draw_bullet
        add dx, [by_offset]
        inc dx
        mov cx, [bx_offset]
        jmp draw_bullet_pixel
    exit_draw_bullet:
        ret
endp

proc delete_bullet
    push cx
    push dx
    mov cx, [bx_offset]
    mov dx, [by_offset]
    delete_bullet_pixel:
        mov al, 64h
        mov ah, 0Ch
        int 10h
    drepeat:
        sub cx, [bx_offset]
        cmp cx, 09h
        je ddb_add_y
        add cx, [bx_offset]
        inc cx
        jmp delete_bullet_pixel
    ddb_add_y:
        sub dx, [by_offset]
        cmp dx, 0eh
        je dexit_draw_bullet
        add dx, [by_offset]
        inc dx
        mov cx, [bx_offset]
        jmp delete_bullet_pixel
    dexit_draw_bullet:
        pop dx
        pop cx
        ret
endp

; arguments: dx=seed
; returns a random number between 1 and 100
proc randomize_dx
    push ax
    push bx
    push cx

    mov ax, 62753
    mov cx, 100

    randomize_dx_loop:
        mul dx
        add ax, 48541
        mov dx, 00h
        div cx
        inc bx
        cmp bx, 35
        jle randomize_dx_loop

    pop cx
    pop bx
    pop ax
    ret
endp

proc draw_all_buildings
    mov bx, 00h
    draw_all_buildings_loop:
        mov ah, 00h
        int 1ah
        mov ax, dx
        xor dx, dx
        mov cx, 100
        div cx
        mov [building_width], 09h
        call randomize_dx
        mov [building_height], dx
        mov ax, 200
        sub ax, dx
        mov [building_y], ax
        mov [building_x], bx
        call draw_building
        add bx, 0ah
    
    mov ax, bx ; save bx in ax
    mov bx, 02h
    call delay
    mov bx, ax ; restore bx

    cmp bx, 320
    jne draw_all_buildings_loop
    ret
endp

proc delay
    push ax
    tick:
    mov ah, 00h
    int 1ah
    cmp si, dx
    jz tick 
    mov si, dx
    dec bx
    jnz tick
    pop ax
    ret
endp

proc update_bullets
    push bx
    mov bx, 00h
    update_current_bullet:
        mov cx, [bx_offset_array+bx]
        mov dx, [by_offset_array+bx]
        cmp [dead_bullets_array+bx], 01h
        je repeat_update_bullets
        cmp dx, 0000h
        je repeat_update_bullets
        mov [bx_offset], cx
        mov [by_offset], dx
        call delete_bullet
        add dx, 05h
        cmp dx, 195
        jge kill_bullet
        mov [by_offset], dx
        mov [by_offset_array+bx], dx
        call draw_bullet
        jmp repeat_update_bullets
        kill_bullet:
            mov [dead_bullets_array+bx], 01h

    repeat_update_bullets:
        add bx, 02h
        cmp bx, [bullet_count]
        jle update_current_bullet
        jmp exit_update_bullets
    exit_update_bullets:
        pop bx
        ret
endp

proc create_bullet
    push bx
    inc [bullets_left]
    call delete_bullets_left
    dec [bullets_left]
    mov bx, [bullet_count]
    mov cx, [x_offset]
    mov dx, [y_offset]
    add dx, 01h
    cmp cx, 320
    jge create_bullet_add_plane_y
    jmp create_bullet_skip_add_plane_y
    create_bullet_add_plane_y:
        add dx, 10h
    create_bullet_skip_add_plane_y:
    mov [bx_offset_array+bx], cx
    mov [by_offset_array+bx], dx
    add bx, 02h
    mov [bullet_count], bx
    call draw_bullets_left
    pop bx
    ret
endp

proc draw_bullets_left
    mov bx, 0
    cmp [bullets_left], 00h
    je exit_draw_ammo
    draw_ammo:
        mov cx, bx
        add cx, cx
        add cx, 1h
        mov dx, 1h
        
        draw_ammo_pixel:
            mov ah, 0Ch
            mov al, 04h
            int 10h
        draw_ammo_update_y:
            mov cx, bx
            add cx, cx
            add cx, 1
            inc dx
            cmp dx, 04h
            jl draw_ammo_pixel
    inc bx
    cmp bx, [bullets_left]
    jl draw_ammo
    exit_draw_ammo:
    ret
endp

proc delete_bullets_left
    mov bx, 0
    delete_ammo:
        mov cx, bx
        add cx, cx
        add cx, 1h
        mov dx, 1h
        
        delete_ammo_pixel:
            mov ah, 0Ch
            mov al, 64h
            int 10h
        delete_ammo_update_y:
            mov cx, bx
            add cx, cx
            add cx, 1
            inc dx
            cmp dx, 04h
            jl delete_ammo_pixel
    inc bx
    cmp bx, [bullets_left]
    jl delete_ammo
    ret
endp

Start:
    mov ax, @data
    mov ds, ax

    call move_to_graphics_mode
    call change_bg_color
    call draw_plane
    call draw_all_buildings
    jmp mainloop

update_plane_y:
    add [y_offset], 10h
    mov [x_offset], 00h
    add [bullets_left], 03h
    call delete_bullets_left
    call draw_bullets_left
    jmp update_frame

update_frame:
    call delete_plane
    mov si, offset x_offset
    mov ax, [si]
    cmp ax, 320
    jge update_plane_y
    add ax, 0ah
    mov [si], ax
	call draw_plane
    call update_bullets
    jmp mainloop
spacebar_pressed:
    cmp [bullets_left], 00h
    jg spacebar_pressed_create_bullet
    jmp spacebar_pressed_skip_create_bullet
    spacebar_pressed_create_bullet:
        dec [bullets_left]
        call create_bullet
    spacebar_pressed_skip_create_bullet:
    jmp update_frame

mainLoop:
    mov bx, 04h
    call delay

    mov ah, 01h
    int 16h
    jz update_frame
    mov ah, 00h
    int 16h
    cmp al, ' '
    je spacebar_pressed
    jmp update_frame

Exit:
END start