function calltest, n1, n2, x, y

  status=call_external("libpspline_idl.so","test2d_idl", $
    n1,n2,x,y)

  print, 'done'

  return, status
end
