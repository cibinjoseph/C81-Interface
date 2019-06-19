function [L,R] = getInterval(A,S)
  % Uses binary search to determine interval 
  % in which S lies in sorted array A
  % A is assumed to be a sorted 1-d array

  L = 1;
  n = length(A);
  R = n;

  if (abs(A(n) - S) > eps)  % if last element is not S
    % Binary search algorithm
    while ((L ~= R) && (R ~= L+1))
      i = floor((L+R)*0.5);
      if (A(i) <= S)
        L = i+1;
      else
        R = i;
      end
    end
    L = max(L-1,1);
  else  % if last element is S
    L = n;
    R = n;
  end

  if (abs(A(L)-S) <= eps)  % if first element is S
    R = L;
  end

  return;
