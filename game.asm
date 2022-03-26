IDEAL

MODEL small

STACK 256

DATASEG
x_offset dw 00h
y_offset dw 00h

bullet_count dw 00h
bullet_durability dw 10h

; bullet xy cords array
bx_offset_array dw 500 dup(0000h)
by_offset_array dw 500 dup(0000h)
dead_bullets_array dw 500 dup(0000h)
bullets_destroy_count_array dw 500 dup(0000h)
bullets_type_array dw 500 dup(0000h)

current_bullet dw 0000h
draw_bullet_inc_destroy_count dw 00h

bullets_left dw 05h
super_bullets_left dw 01h

bx_offset dw 00h
by_offset dw 00h

speed dw 05h

plane_x_0 dw 00h

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

filename db 'start.bmp',0
filehandle dw ?
Header db 54 dup (0)
Palette db 256*4 dup (0)
ScrLine db 320 dup (0)
ErrorMsg db 'Error', 13, 10 ,'$'

CODESEG

proc move_to_graphics_mode
    mov ah, 00h ;enter graphics mode
    mov al, 13h
    int 10h
endp

proc OpenFile
    ; Open file
    mov ah, 3Dh
    xor al, al
    mov dx, offset filename
    int 21h
    jc openerror
    mov [filehandle], ax
    ret
    openerror:
        mov dx, offset ErrorMsg
        mov ah, 9h
        int 21h
        ret
endp OpenFile

proc ReadHeader
    ; Read BMP file header, 54 bytes
    mov ah,3fh
    mov bx, [filehandle]
    mov cx,54
    mov dx,offset Header
    int 21h
    ret
endp ReadHeader

proc ReadPalette
    ; Read BMP file color palette, 256 colors * 4 bytes (400h)
    mov ah,3fh
    mov cx,400h
    mov dx,offset Palette
    int 21h
    ret
endp ReadPalette

proc CopyPal
    ; Copy the colors palette to the video memory
    ; The number of the first color should be sent to port 3C8h
    ; The palette is sent to port 3C9h
    mov si,offset Palette
    mov cx,256
    mov dx,3C8h
    mov al,0
    ; Copy starting color to port 3C8h
    out dx,al
    ; Copy palette itself to port 3C9h
    inc dx
    PalLoop:
        ; Note: Colors in a BMP file are saved as BGR values rather than RGB .
        mov al,[si+2] ; Get red value .
        shr al,2 ; Max. is 255, but video palette maximal
        ; value is 63. Therefore dividing by 4.
        out dx,al ; Send it .
        mov al,[si+1] ; Get green value .
        shr al,2
        out dx,al ; Send it .
        mov al,[si] ; Get blue value .
        shr al,2
        out dx,al ; Send it .
        add si,4 ; Point to next color .
        ; (There is a null chr. after every color.)
    loop PalLoop
    ret
endp CopyPal

proc CopyBitmap
    ; BMP graphics are saved upside-down .
    ; Read the graphic line by line (200 lines in VGA format),
    ; displaying the lines from bottom to top.
    mov ax, 0A000h
    mov es, ax
    mov cx,200
    PrintBMPLoop :
        push cx
        ; di = cx*320, point to the correct screen line
        mov di,cx
        shl cx,6
        shl di,8
        add di,cx
        ; Read one line
        mov ah,3fh
        mov cx,320
        mov dx,offset ScrLine
        int 21h
        ; Copy one line into video memory
        cld ; Clear direction flag, for movsb
        mov cx,320
        mov si,offset ScrLine
        rep movsb ; Copy line to the screen
            ;rep movsb is same as the following code :
            ;mov es:di, ds:si
            ;inc si
            ;inc di
            ;dec cx
            ;... ;loop until cx=0
        pop cx
    loop PrintBMPLoop
    ret
endp CopyBitmap

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
        mov ah, 00h
        int 16h
        cmp al, ' '
        je restart_middle1
        jmp exit_game
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

jmp restart_middle1_skip
restart_middle1:
    jmp restart_middle2
restart_middle1_skip:

proc draw_bullet
    mov cx, [bx_offset]
    mov dx, [by_offset]
    draw_bullet_pixel:
        mov ah, 0Dh
        int 10h
        cmp al, 00h
        je destroy_building
        jmp skip_destroy_building

        destroy_building:
            cmp [draw_bullet_inc_destroy_count], 01h
            je skip_destroy_building
            mov bx, [current_bullet]
            inc [bullets_destroy_count_array+bx]
            mov [draw_bullet_inc_destroy_count], 01h

        skip_destroy_building:

        mov si, offset bullets_type_array
        add si, [current_bullet]
        cmp [si], 0001h
        je draw_super_bullet
        mov al, 04h
        jmp draw_super_bullet_skip
        draw_super_bullet:
            mov al, 01h
        draw_super_bullet_skip:
        mov ah, 0Ch
        int 10h
    repeat:
        sub cx, [bx_offset]
        cmp cx, 09h
        je db_add_y
        add cx, [bx_offset]
        inc cx
        jmp draw_bullet_pixel

    jmp restart_middle2_skip
    restart_middle2:
        jmp restart_middle3
    restart_middle2_skip:

    db_add_y:
        sub dx, [by_offset]
        cmp dx, 0eh
        je exit_draw_bullet
        add dx, [by_offset]
        inc dx
        mov cx, [bx_offset]
        jmp draw_bullet_pixel
    exit_draw_bullet:
        mov [draw_bullet_inc_destroy_count], 00h
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
        add ax, 13849
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

jmp restart_middle3_skip
restart_middle3:
    jmp restart
restart_middle3_skip:

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
        cmp dx, 190
        jge kill_bullet
        cmp [bullets_type_array+bx], 0001h
        je skip_bullet_durabillity_check
        mov ax, [bullet_durability]
        cmp [bullets_destroy_count_array+bx], ax
        jge kill_bullet
        skip_bullet_durabillity_check:
        mov [by_offset], dx
        mov [by_offset_array+bx], dx
        mov [current_bullet], bx
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

proc create_super_bullet
    push bx
    inc [bullets_left]
    call delete_bullets_left
    dec [bullets_left]
    mov bx, [bullet_count]
    mov cx, [x_offset]
    mov dx, [y_offset]
    add dx, 01h
    cmp cx, 320
    jge create_super_bullet_add_plane_y
    jmp create_super_bullet_skip_add_plane_y
    create_super_bullet_add_plane_y:
        add dx, 10h
    create_super_bullet_skip_add_plane_y:
    mov [bx_offset_array+bx], cx
    mov [by_offset_array+bx], dx
    mov [bullets_type_array+bx], 0001h
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

proc clear_array
    push bx
    push ax
    mov bx, 00h
    clear_array_loop:
        mov ax, 0000h
        mov [si+bx], ax
        add bx, 02h
        cmp bx, 1000
        jl clear_array_loop
    pop ax
    pop bx
    ret
endp

proc draw_square
    mov cx, 315
    draw_square_x:
        inc cx
        mov dx, 00h
        draw_square_y:
            inc dx
            int 10h
            cmp dx, 04h
            jl draw_square_y
        cmp cx, 318
        jl draw_square_x
    ret
endp

proc update_super_ammo
    cmp [super_bullets_left], 0000h
    je delete_super_ammo
    mov al, 01h
    mov ah, 0Ch
    mov cx, 317
    mov dx, 01h
    call draw_square
    jmp exit_update_super_ammo
    delete_super_ammo:
    mov al, 064h
    mov ah, 0Ch
    mov cx, 317
    mov dx, 01h
    call draw_square
    exit_update_super_ammo:
    ret
endp

proc color_top
    mov cx, 00h
    mov dx, 00h
    mov ah, 0Ch
    mov al, 00h
    color_top_loop:
        int 10h
	inc cx
	cmp cx, 320
	jg color_top_exit
	jmp color_top_loop
    color_top_exit:
    ret
endp

Start:
    mov ax, @data
    mov ds, ax

    call move_to_graphics_mode

    show_controls:
        ; Process BMP file
        call OpenFile
        call ReadHeader
        call ReadPalette
        call CopyPal
        call CopyBitmap
	    call color_top
        ; Wait for key press
        mov ah,1
        int 21h
    
        call move_to_graphics_mode
	    jmp restart

    restart:
        mov si, offset bx_offset_array
        call clear_array
        mov si, offset by_offset_array
        call clear_array
        mov si, offset dead_bullets_array
        call clear_array
        mov si, offset bullets_destroy_count_array
        call clear_array
        mov si, offset bullets_type_array
        call clear_array
        mov [bullet_count], 00h
        mov [bullet_durability], 10h
        mov [bullets_left], 05h
        mov [super_bullets_left], 01h
        mov [speed], 05h
        mov [x_offset], 00h
        mov [y_offset], 00h

    call change_bg_color
    call draw_plane
    call draw_all_buildings
    jmp mainloop

update_plane_y:
    add [y_offset], 10h
    mov [x_offset], 00h
    mov [plane_x_0], 0001h
    mov [bullets_left], 05h
    mov [super_bullets_left], 01h
    call update_super_ammo
    call update_super_ammo
    inc [bullets_left]
    call delete_bullets_left
    dec [bullets_left]
    call draw_bullets_left
    jmp update_frame

next_level:
    ; clear the screen
    call change_bg_color
    mov [x_offset], 00h
    mov [y_offset], 00h
    call draw_all_buildings
    mov [bullets_left], 05h
    dec [bullet_durability]
    cmp [speed], 03h
    jg update_speed
    jmp update_speed_skip
    update_speed:
        dec [speed]
    update_speed_skip:
    jmp update_plane_y_middle_skip
    update_plane_y_middle:
        jmp update_plane_y
    update_plane_y_middle_skip:

    ; clear bullet arrays to prevent overflow
    mov si, offset bx_offset_array
    call clear_array
    mov si, offset by_offset_array
    call clear_array
    mov si, offset dead_bullets_array
    call clear_array
    mov si, offset bullets_destroy_count_array
    call clear_array
    mov si, offset bullets_type_array
    call clear_array
    mov [bullet_count], 00h

    jmp mainloop

update_frame:
    call delete_plane
    mov si, offset x_offset
    mov ax, [si]
    cmp ax, 310
    jge update_plane_y_middle
    cmp [y_offset], 0B0h
    jge next_level
    cmp [plane_x_0], 0001h
    je skip_add_plane_x
    add ax, 0ah
    mov [si], ax
    jmp skip_skip_add_plane_x
    skip_add_plane_x:
        mov [plane_x_0], 0000h
    skip_skip_add_plane_x:
	call draw_plane
    call update_bullets
    jmp mainloop

jmp middles_skip
show_controls_middle:
    jmp show_controls
update_frame_middle:
    jmp update_frame
middles_skip:
spacebar_pressed:
    cmp [bullets_left], 00h
    jg spacebar_pressed_create_bullet
    jmp spacebar_pressed_skip_create_bullet
    spacebar_pressed_create_bullet:
        dec [bullets_left]
        call create_bullet
    spacebar_pressed_skip_create_bullet:
    jmp update_frame

jmp restart_middle_skip
restart_middle:
    jmp restart
restart_middle_skip:

c_pressed:
    cmp [super_bullets_left], 00h
    jg c_pressed_create_bullet
    jmp c_pressed_skip_create_bullet
    c_pressed_create_bullet:
        dec [super_bullets_left]
        call update_super_ammo
        call create_super_bullet
    c_pressed_skip_create_bullet:
    jmp update_frame

mainLoop:
    mov bx, [speed]
    call delay

    mov ah, 01h
    int 16h
    jz update_frame
    mov ah, 00h
    int 16h
    cmp al, ' '
    je spacebar_pressed
    cmp al, 'c'
    je c_pressed
    cmp al, 'r'
    je restart_middle
    cmp al, 'p'
    je pause_game
    cmp al, 'z'
    je show_controls_middle
    jmp update_frame

jmp pause_game_skip
pause_game:
    mov ah, 00h
    int 16h
    cmp al, 'p'
    je update_frame_middle
    jmp pause_game
pause_game_skip:


Exit:
END start
