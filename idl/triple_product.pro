function triple_product,a,b,c

  na=size(a,/n_elements)
  nb=size(b,/n_elements)
  nc=size(c,/n_elements)

  ans = fltarr(na,nb,nc)
  ans0 = a##b

  for i=0,nc-1 do begin
    ans[*,*,i] = ans0*c[i]
  endfor

  return,ans
end

