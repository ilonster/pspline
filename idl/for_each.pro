function for_each, x,  loop_fun, nf=nf

  ndim = size(x,/n_dimensions)
  dim  = size(x,/dimensions)
  if dim[0] EQ 0 then dim=1

  if nf LT 1 then begin
     nf=1
     f=make_array(dim,type=size(x,/type))
  endif else begin
    f=make_array([nf,dim],type=size(x,/type)) 
  endelse
  if ndim EQ 8 AND nf GT 1 then begin
        print, "foreach: requires nf=1 when ndim(x)=8"  
  endif

if nf EQ 1 then begin
  case ndim of 
    0: f=loop_fun(x)
    1: begin
         for i0=0,dim[0]-1 do begin
            f[i0]=loop_fun(x[i0])
         endfor
       end
    2: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
            f[i0,i1]=loop_fun(x[i0,i1])
         endfor
         endfor
       end
    3: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
            f[i0,i1,i2]=loop_fun(x[i0,i1,i2])
         endfor
         endfor
         endfor
       end
    4: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
         for i3=0,dim[3]-1 do begin  
            f[i0,i1,i2,i3]=loop_fun(x[i0,i1,i2,i3])
         endfor
         endfor
         endfor
         endfor
       end
    5: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
         for i3=0,dim[3]-1 do begin  
         for i4=0,dim[4]-1 do begin 
            f[i0,i1,i2,i3,i4]=loop_fun(x[i0,i1,i2,i3,i4])
         endfor
         endfor
         endfor
         endfor
         endfor
       end
    6: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
         for i3=0,dim[3]-1 do begin  
         for i4=0,dim[4]-1 do begin 
         for i5=0,dim[5]-1 do begin 
            f[i0,i1,i2,i3,i4,i5]=loop_fun(x[i0,i1,i2,i3,i4,i5])
         endfor
         endfor
         endfor
         endfor
         endfor
         endfor
       end
    7: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
         for i3=0,dim[3]-1 do begin  
         for i4=0,dim[4]-1 do begin 
         for i5=0,dim[5]-1 do begin 
         for i6=0,dim[6]-1 do begin 
            f[i0,i1,i2,i3,i4,i5,i6]=loop_fun(x[i0,i1,i2,i3,i4,i5])
         endfor
         endfor
         endfor
         endfor
         endfor
         endfor
         endfor
       end
    8: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
         for i3=0,dim[3]-1 do begin  
         for i4=0,dim[4]-1 do begin 
         for i5=0,dim[5]-1 do begin 
         for i6=0,dim[6]-1 do begin 
         for i7=0,dim[7]-1 do begin 
            f[i0,i1,i2,i3,i4,i5,i6,i7]=loop_fun(x[i0,i1,i2,i3,i4,i5,i7])
         endfor
         endfor
         endfor
         endfor
         endfor
         endfor
         endfor
         endfor
       end
    endcase
endif else begin
  case ndim of 
    0: f=loop_fun(x)
    1: begin
         for i0=0,dim[0]-1 do begin
            f[*,i0]=loop_fun(x[i0])
         endfor
       end
    2: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
            f[*,i0,i1]=loop_fun(x[i0,i1])
         endfor
         endfor
       end
    3: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
            f[*,i0,i1,i2]=loop_fun(x[i0,i1,i2])
         endfor
         endfor
         endfor
       end
    4: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
         for i3=0,dim[3]-1 do begin  
            f[*,i0,i1,i2,i3]=loop_fun(x[i0,i1,i2,i3])
         endfor
         endfor
         endfor
         endfor
       end
    5: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
         for i3=0,dim[3]-1 do begin  
         for i4=0,dim[4]-1 do begin 
            f[*,i0,i1,i2,i3,i4]=loop_fun(x[i0,i1,i2,i3,i4])
         endfor
         endfor
         endfor
         endfor
         endfor
       end
    6: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
         for i3=0,dim[3]-1 do begin  
         for i4=0,dim[4]-1 do begin 
         for i5=0,dim[5]-1 do begin 
            f[*,i0,i1,i2,i3,i4,i5]=loop_fun(x[i0,i1,i2,i3,i4,i5])
         endfor
         endfor
         endfor
         endfor
         endfor
         endfor
       end
    7: begin
         for i0=0,dim[0]-1 do begin
         for i1=0,dim[1]-1 do begin       
         for i2=0,dim[2]-1 do begin  
         for i3=0,dim[3]-1 do begin  
         for i4=0,dim[4]-1 do begin 
         for i5=0,dim[5]-1 do begin 
         for i6=0,dim[6]-1 do begin 
            f[*,i0,i1,i2,i3,i4,i5,i6]=loop_fun(x[i0,i1,i2,i3,i4,i5])
         endfor
         endfor
         endfor
         endfor
         endfor
         endfor
         endfor
       end
    endcase
endelse
end
