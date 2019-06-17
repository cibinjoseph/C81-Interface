function indx = binarySearch(A,T)
  L = 1;
  n = length(A);
  R = n;
  
  if (abs(A(n) - T) > eps)
    while (L < R)
      indx = floor((L+R)*0.5);
      if (A(indx) <= T)
        L = indx+1;
      else
        R = indx;
      end
    end
    indx = L-1;
    L = L-1;
    if (abs(A(R)-T) < eps)
      R = L;
    end
  else
  indx = n;
  L = n;
  R = n;
  end
L
R
  return;
